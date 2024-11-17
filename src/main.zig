const std = @import("std");
const vaxis = @import("vaxis");
const Cell = vaxis.Cell;
const Directory = std.fs.Dir;
const time = std.time;
const Instant = time.Instant;
const Timer = time.Timer;
const print = std.debug.print;
const UUID = @import("uuid.zig").UUID;

const log = std.log.scoped(.main);

// Constants moved to top level for better organization
const MAX_HISTORY = 10;
const MAX_ENTRIES = 10000;
const MAX_FILE_PREVIEW_SIZE = 2056;

fn pathMatchesAtDepth(
    selected_path: []const u8,
    depth: usize,
    file_name: []const u8,
) bool {
    var it = std.mem.splitSequence(u8, selected_path, "/");
    var current_depth: usize = 0;
    while (it.next()) |segment| {
        if (current_depth == depth) {
            const trimmed_segment = std.mem.trimRight(u8, segment, &[_]u8{0});
            return std.mem.eql(u8, trimmed_segment, file_name);
        }
        current_depth += 1;
    }
    return false;
}

fn printChildren(win: *vaxis.Window, children: ?std.ArrayList(FileEntry), depth: usize, offset: usize, cur_path: []u8) !usize {
    var new_offset = offset;
    if (children) |unwrapped_children| {
        for (unwrapped_children.items) |child| {
            var style: vaxis.Style = .{};
            if (child.is_open) {
                style.ul_style = .single;
            }
            if (child.kind == .directory) {
                style.fg = .{ .index = 4 };
            }
            if (pathMatchesAtDepth(cur_path, depth, child.name)) {
                style.reverse = true;
            }

            const indentation = 2 * depth;
            var seg = [_]vaxis.Segment{.{ .text = child.name, .style = style }};
            _ = win.print(&seg, .{ .row_offset = new_offset, .col_offset = indentation }) catch {};
            new_offset += 1;

            if (child.kind == .directory and child.is_open) {
                new_offset = try printChildren(win, child.children, depth + 1, new_offset, cur_path);
            }
        }
    }
    return new_offset;
}

const FilePreviews = struct {
    preview_map: std.StringHashMap([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !FilePreviews {
        return .{
            .preview_map = std.StringHashMap([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *FilePreviews) void {
        var it = self.preview_map.iterator();
        while (it.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            self.allocator.free(entry.value_ptr.*);
        }
        self.preview_map.deinit();
    }

    pub fn loadPreviewForFilePath(self: *FilePreviews, path: []const u8, upsert: bool) ![]const u8 {
        if (!upsert) {
            if (self.preview_map.get(path)) |existing| {
                return existing;
            }
        }

        var file = try std.fs.cwd().openFile(path, .{});
        defer file.close();

        const file_size = try file.getEndPos();
        const slice_size = @min(file_size, MAX_FILE_PREVIEW_SIZE);
        const buffer = try self.allocator.alloc(u8, slice_size);
        errdefer self.allocator.free(buffer);

        const bytes_read = try file.read(buffer);
        const final_buffer = if (bytes_read < slice_size)
            try self.allocator.realloc(buffer, bytes_read)
        else
            buffer;

        if (self.preview_map.getPtr(path)) |value_ptr| {
            self.allocator.free(value_ptr.*);
            value_ptr.* = final_buffer;
        } else {
            const key_dup = try self.allocator.dupe(u8, path);
            errdefer self.allocator.free(key_dup);
            try self.preview_map.put(key_dup, final_buffer);
        }

        return final_buffer;
    }

    pub fn getPreviewForFilePath(self: *FilePreviews, path: []const u8) ?[]const u8 {
        return self.preview_map.get(path);
    }
};

const DirectoryState = struct {
    path: []const u8,
    open_uuids: std.ArrayList([36]u8),

    fn init(allocator: std.mem.Allocator, path: []const u8) !DirectoryState {
        return .{
            .path = try allocator.dupe(u8, path),
            .open_uuids = std.ArrayList([36]u8).init(allocator),
        };
    }

    fn deinit(self: *DirectoryState) void {
        self.open_uuids.allocator.free(self.path);
        self.open_uuids.deinit();
    }
};

const FileEntry = struct {
    name: []const u8,
    kind: std.fs.File.Kind,
    children: ?std.ArrayList(FileEntry),
    uuid: [36]u8,
    is_open: bool,

    fn init(allocator: std.mem.Allocator, name: []const u8, kind: std.fs.File.Kind) !*FileEntry {
        const entry = try allocator.create(FileEntry);
        entry.* = .{
            .name = try allocator.dupe(u8, name),
            .kind = kind,
            .children = if (kind == .directory) std.ArrayList(FileEntry).init(allocator) else null,
            .uuid = undefined,
            .is_open = false,
        };
        const uuid = UUID.init();
        uuid.to_string(&entry.uuid);
        return entry;
    }

    fn findEntryRecursive(self: *FileEntry, uuid: [36]u8) ?*FileEntry {
        if (std.mem.eql(u8, &self.uuid, &uuid)) {
            return self;
        }
        if (self.children) |children| {
            for (children.items) |*child| {
                if (child.findEntryRecursive(uuid)) |found| {
                    return found;
                }
            }
        }
        return null;
    }

    fn collectOpenUUIDs(self: *FileEntry, list: *std.ArrayList([36]u8)) !void {
        if (self.is_open) {
            try list.append(self.uuid);
        }
        if (self.children) |children| {
            for (children.items) |*child| {
                try child.collectOpenUUIDs(list);
            }
        }
    }
};

const DirectorySelector = struct {
    arena: std.heap.ArenaAllocator,
    root: *FileEntry,
    selected_path: [std.fs.max_path_bytes]u8,
    selected_path_len: usize,
    history: std.ArrayList(DirectoryState),
    history_allocator: std.mem.Allocator,
    entry_count: usize,

    pub fn init(allocator: std.mem.Allocator, path: []const u8) !DirectorySelector {
        var arena = std.heap.ArenaAllocator.init(allocator);
        errdefer arena.deinit();

        var self = DirectorySelector{
            .arena = arena,
            .root = undefined,
            .selected_path = undefined,
            .selected_path_len = 0,
            .history = std.ArrayList(DirectoryState).init(allocator),
            .history_allocator = allocator,
            .entry_count = 0,
        };
        try self.setSelectedPath(path);

        self.root = try FileEntry.init(self.arena.allocator(), path, .directory);
        try self.loadChildren(self.root, 0);

        return self;
    }

    pub fn deinit(self: *DirectorySelector) void {
        for (self.history.items) |*state| {
            state.deinit();
        }
        self.history.deinit();
        self.arena.deinit();
    }

    pub fn setSelectedPath(self: *DirectorySelector, path: []const u8) !void {
        if (path.len > self.selected_path.len) {
            return error.PathTooLong;
        }

        @memcpy(self.selected_path[0..path.len], path);
        @memset(self.selected_path[path.len..], 0);
        self.selected_path_len = path.len;
    }

    pub fn setSelectedPathToSibling(self: *DirectorySelector, sibling_name: []const u8) !void {
        const last_slash_index = std.mem.lastIndexOfScalar(u8, self.selected_path[0..self.selected_path_len], '/') orelse return error.NoParentDirectory;
        const new_len = last_slash_index + 1 + sibling_name.len;

        if (new_len > self.selected_path.len) {
            return error.PathTooLong;
        }

        @memcpy(self.selected_path[last_slash_index + 1 .. new_len], sibling_name);

        if (new_len < self.selected_path_len) {
            @memset(self.selected_path[new_len..self.selected_path_len], 0);
        }

        self.selected_path_len = new_len;
    }

    pub fn appendToSelectedPath(self: *DirectorySelector, name: []const u8) !void {
        if (self.selected_path_len + name.len + 1 > self.selected_path.len) {
            return error.PathTooLong;
        }

        self.selected_path[self.selected_path_len] = '/';
        @memcpy(self.selected_path[self.selected_path_len + 1 .. self.selected_path_len + 1 + name.len], name);
        self.selected_path_len += name.len + 1;
    }

    pub fn popFromSelectedPath(self: *DirectorySelector) void {
        if (self.selected_path_len == 0) return;

        var i = self.selected_path_len - 1;
        while (i > 0) : (i -= 1) {
            if (self.selected_path[i] == '/') {
                break;
            }
        }

        self.selected_path_len = i;
        @memset(self.selected_path[self.selected_path_len..], 0);
    }

    fn loadChildren(self: *DirectorySelector, entry: *FileEntry, depth: usize) !void {
        if (entry.kind != .directory or depth >= 2 or self.entry_count >= MAX_ENTRIES) return;

        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const path_len = @min(self.selected_path_len, std.fs.max_path_bytes);
        @memcpy(path_buf[0..path_len], self.selected_path[0..path_len]);

        const path_slice = path_buf[0..path_len];

        var dir = std.fs.openDirAbsolute(path_slice, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) {
                log.err("Directory not found: {s}", .{entry.name});
                return;
            }
            return err;
        };
        defer dir.close();

        var it = dir.iterate();
        while (try it.next()) |file_entry| {
            if (self.entry_count >= MAX_ENTRIES) break;

            const child = try FileEntry.init(self.arena.allocator(), file_entry.name, file_entry.kind);
            try entry.children.?.append(child.*);
            self.entry_count += 1;

            if (file_entry.kind == .directory and depth < 1) {
                try self.loadChildren(child, depth + 1);
            }
        }
    }

    fn saveCurrentState(self: *DirectorySelector) !void {
        var state = try DirectoryState.init(self.history_allocator, self.selected_path[0..self.selected_path_len]);
        errdefer state.deinit();

        try self.root.collectOpenUUIDs(&state.open_uuids);

        if (self.history.items.len >= MAX_HISTORY) {
            var oldest = self.history.orderedRemove(0);
            oldest.deinit();
        }

        try self.history.append(state);
    }

    pub fn toggleDirectoryState(self: *DirectorySelector, uuid: [36]u8) !void {
        if (self.findEntry(uuid)) |entry| {
            if (entry.kind == .directory) {
                entry.is_open = !entry.is_open;
                if (entry.is_open) {
                    entry.children = std.ArrayList(FileEntry).init(self.arena.allocator());
                    try self.loadChildren(entry, 0);
                }
            }
        }
    }

    fn findEntry(self: *DirectorySelector, uuid: [36]u8) ?*FileEntry {
        return self.root.findEntryRecursive(uuid);
    }

    fn findEntryFromPath(self: *DirectorySelector, path: []const u8) ?*FileEntry {
        var it = std.mem.splitSequence(u8, path, "/");
        var current_entry: ?*FileEntry = self.root;

        while (it.next()) |segment| {
            if (segment.len == 0) continue;

            current_entry = if (current_entry) |entry| blk: {
                if (entry.children) |children| {
                    for (children.items) |*child| {
                        if (std.mem.startsWith(u8, segment, child.name)) {
                            break :blk child;
                        }
                    }
                }
                break :blk null;
            } else null;

            if (current_entry == null) return null;
        }

        return current_entry;
    }

    pub fn changeDirectory(self: *DirectorySelector, new_path: []const u8) !void {
        try self.saveCurrentState();

        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.history_allocator);
        self.entry_count = 0;

        self.root = try FileEntry.init(self.arena.allocator(), new_path, .directory);
        try self.setSelectedPath(new_path);
        try self.loadChildren(self.root, 0);
    }
};

fn findChildIndex(parent: *FileEntry, child: *FileEntry) ?usize {
    if (parent.children) |children| {
        for (children.items, 0..) |*entry, index| {
            if (entry == child) {
                return index;
            }
        }
    }
    return null;
}

fn navigateToSibling(selector: *DirectorySelector, index_increment: i32, current_absolute_path: []u8) !void {
    const selected_entry = selector.findEntryFromPath(selector.selected_path[current_absolute_path.len..]) orelse return error.EntryNotFound;
    const last_slash_index = std.mem.lastIndexOfScalar(u8, selector.selected_path[0..selector.selected_path_len], '/') orelse return error.NoParentDirectory;

    const parent_path = selector.selected_path[0..last_slash_index];
    const parent_path_minus_absolute = parent_path[current_absolute_path.len..];
    const parent_entry = selector.findEntryFromPath(parent_path_minus_absolute) orelse selector.root;

    const index_of_selected = findChildIndex(parent_entry, selected_entry) orelse {
        log.err("Error finding index of selected", .{});
        return error.IndexNotFound;
    };

    var new_cursor_index: usize = if (index_increment < 0)
        if (@abs(index_increment) > index_of_selected)
            0
        else
            index_of_selected - @as(usize, @intCast(@abs(index_increment)))
    else
        index_of_selected +| @as(usize, @intCast(index_increment));

    new_cursor_index = new_cursor_index % parent_entry.children.?.items.len;
    const child = parent_entry.children.?.items[new_cursor_index];
    selector.setSelectedPathToSibling(child.name) catch |err| {
        log.err("Error setting selected path to sibling: {}", .{err});
        return err;
    };
}

fn navigateToFirstChild(selector: *DirectorySelector, selected_file_entry: *FileEntry) !void {
    if (selected_file_entry.kind != .directory) return;

    if (!selected_file_entry.is_open) {
        selected_file_entry.is_open = true;
        selected_file_entry.children = std.ArrayList(FileEntry).init(selector.arena.allocator());
        try selector.loadChildren(selected_file_entry, 0);
    }

    const first_child = if (selected_file_entry.children) |children|
        if (children.items.len > 0) children.items[0] else null
    else
        null;

    if (first_child == null) return;
    try selector.appendToSelectedPath(first_child.?.name);
}

fn navigateToParent(selector: *DirectorySelector) void {
    selector.popFromSelectedPath();
}

fn openFileInEditor(selector: *DirectorySelector, selected_file_entry: *FileEntry, alloc: std.mem.Allocator) !void {
    if (selected_file_entry.kind == .directory) return;

    var path_buf: [std.fs.max_path_bytes]u8 = undefined;
    @memcpy(path_buf[0..selector.selected_path_len], selector.selected_path[0..selector.selected_path_len]);
    const path_slice = path_buf[0..selector.selected_path_len];

    var child = std.process.Child.init(&.{ "nvim", path_slice }, alloc);
    _ = try child.spawnAndWait();
}

const Event = union(enum) {
    key_press: vaxis.Key,
    winsize: vaxis.Winsize,
    focus_in,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        if (deinit_status == .leak) {
            log.err("memory leak", .{});
        }
    }
    const alloc = gpa.allocator();

    var tty = try vaxis.Tty.init();
    defer tty.deinit();

    var vx = try vaxis.init(alloc, .{});
    defer vx.deinit(alloc, tty.anyWriter());

    var loop: vaxis.Loop(Event) = .{ .tty = &tty, .vaxis = &vx };
    try loop.init();
    try loop.start();
    defer loop.stop();

    try vx.enterAltScreen(tty.anyWriter());
    try vx.queryTerminal(tty.anyWriter(), 1 * std.time.ns_per_s);
    defer vx.exitAltScreen(tty.anyWriter()) catch {};

    const current_absolute_path = try std.fs.cwd().realpathAlloc(alloc, ".");
    defer alloc.free(current_absolute_path);

    var selector = try DirectorySelector.init(alloc, current_absolute_path);
    defer selector.deinit();

    var file_previews = try FilePreviews.init(alloc);
    defer file_previews.deinit();

    if (selector.root.children) |unwrapped_children| {
        if (unwrapped_children.items.len > 0) {
            const child_name = unwrapped_children.items[0].name;
            try selector.appendToSelectedPath(child_name);
        }
    }

    var color_idx: u8 = 0;
    while (true) {
        const event = loop.nextEvent();
        const children = selector.root.children;

        switch (event) {
            .key_press => |key| {
                color_idx = if (color_idx == 255) 0 else color_idx + 1;

                if (key.codepoint == 'c' and key.mods.ctrl) {
                    break;
                } else if (key.matches(vaxis.Key.tab, .{}) or key.codepoint == 'j') {
                    try navigateToSibling(&selector, 1, current_absolute_path);
                } else if (key.codepoint == 'k') {
                    try navigateToSibling(&selector, -1, current_absolute_path);
                } else if (key.codepoint == 'l') {
                    const selected_entry = selector.findEntryFromPath(selector.selected_path[current_absolute_path.len..]) orelse continue;
                    if (selected_entry.kind == .directory) {
                        try navigateToFirstChild(&selector, selected_entry);
                    } else {
                        try vx.exitAltScreen(tty.anyWriter());
                        try vx.resetState(tty.anyWriter());
                        loop.stop();
                        try openFileInEditor(&selector, selected_entry, alloc);
                        try loop.start();
                        try vx.enterAltScreen(tty.anyWriter());
                        try vx.enableDetectedFeatures(tty.anyWriter());
                        vx.queueRefresh();
                    }
                } else if (key.codepoint == 'h') {
                    const last_slash_index = std.mem.lastIndexOfScalar(u8, selector.selected_path[0..selector.selected_path_len], '/') orelse return error.NoParentDirectory;
                    if (last_slash_index == current_absolute_path.len) continue;

                    navigateToParent(&selector);
                    if (selector.findEntryFromPath(selector.selected_path[current_absolute_path.len..])) |selected_entry| {
                        if (selected_entry.kind == .directory) {
                            selected_entry.is_open = false;
                        }
                    }
                } else if (key.codepoint == 'r') {
                    if (selector.findEntryFromPath(selector.selected_path[current_absolute_path.len..])) |selected_entry| {
                        if (selected_entry.kind == .file) {
                            _ = try file_previews.loadPreviewForFilePath(selector.selected_path[0..selector.selected_path_len], true);
                        }
                    }
                } else if (key.codepoint == 'q') {
                    break;
                }
            },
            .winsize => |ws| {
                try vx.resize(alloc, tty.anyWriter(), ws);
            },
            else => {},
        }

        const win = vx.window();
        win.clear();

        var main_win = win.child(.{
            .x_off = 0,
            .y_off = 0,
            .width = .{ .limit = win.width / 2 },
            .height = .{ .limit = win.height },
        });

        const cur_path = selector.selected_path[current_absolute_path.len + 1 ..];
        _ = try printChildren(&main_win, children, 0, 0, cur_path);

        _ = win.child(.{
            .x_off = 0,
            .y_off = 0,
            .width = .{ .limit = win.width / 2 },
            .height = .{ .limit = 1 },
        });

        const preview_header_win = win.child(.{
            .x_off = win.width / 2,
            .y_off = 0,
            .width = .{ .limit = win.width },
            .height = .{ .limit = 1 },
        });

        const preview_win = win.child(.{
            .x_off = win.width / 2,
            .y_off = 2,
            .width = .{ .limit = win.width / 2 },
            .height = .{ .limit = win.height - (preview_header_win.height + 1) },
            .border = .{ .where = .all, .glyphs = .single_square },
        });

        var file_preview = file_previews.getPreviewForFilePath(selector.selected_path[0..selector.selected_path_len]);
        if (file_preview == null) {
            const selected_entry = selector.findEntryFromPath(selector.selected_path[current_absolute_path.len..]) orelse continue;
            if (selected_entry.kind == .file) {
                _ = try file_previews.loadPreviewForFilePath(selector.selected_path[0..selector.selected_path_len], true);
            }
            file_preview = file_previews.getPreviewForFilePath(selector.selected_path[0..selector.selected_path_len]);
        }

        const preview_text = file_preview orelse "No preview available";
        const preview_style: vaxis.Style = .{ .reverse = file_preview == null };
        var seg = [_]vaxis.Segment{.{
            .text = preview_text,
            .style = preview_style,
        }};
        _ = preview_win.print(&seg, .{ .row_offset = 0 }) catch {};

        try vx.render(tty.anyWriter());
    }
}
