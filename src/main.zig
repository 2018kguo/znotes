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

fn pathMatchesAtDepth(
    selected_path: []const u8,
    depth: usize,
    file_name: []const u8,
) bool {
    var it = std.mem.split(u8, selected_path, "/");
    var current_depth: usize = 0;
    while (it.next()) |segment| {
        if (current_depth == depth) {
            // Trim any trailing null bytes or spaces from the segment
            const trimmed_segment = std.mem.trimRight(u8, segment, &[_]u8{0});
            const matches = std.mem.eql(u8, trimmed_segment, file_name);
            return matches;
        }
        current_depth += 1;
    }
    return false;
}

fn printChildren(win: *vaxis.Window, children: ?std.ArrayList(FileEntry), depth: usize, offset: usize, cur_path: []u8) !usize {
    var new_offset = offset;
    //std.debug.print("cur_path: {s}\n", .{cur_path});
    if (children) |unwrapped_children| {
        for (unwrapped_children.items) |child| {
            var style: vaxis.Style = .{};
            if (child.is_open) {
                style.ul_style = .single;
            }
            if (child.kind == .directory) {
                style.fg = .{ .index = 4 };
            }
            //std.debug.print("cur_path: {s}, child name: {s}\n", .{ cur_path, child.name });
            if (pathMatchesAtDepth(cur_path, depth, child.name)) {
                style.reverse = true;
            }

            // Create indentation based on depth
            const indentation = 2 * depth;

            var seg = [_]vaxis.Segment{
                .{ .text = child.name, .style = style },
            };
            _ = win.print(&seg, .{ .row_offset = new_offset, .col_offset = indentation }) catch {};
            new_offset += 1;

            // Recursively print children if it's a directory and is open
            if (child.kind == .directory and child.is_open) {
                new_offset = try printChildren(win, child.children, depth + 1, new_offset, cur_path);
            }
        }
    }
    return new_offset;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
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

    // Optionally enter the alternate screen
    try vx.enterAltScreen(tty.anyWriter());
    try vx.queryTerminal(tty.anyWriter(), 1 * std.time.ns_per_s);

    const current_absolute_path = try std.fs.cwd().realpathAlloc(alloc, ".");
    defer alloc.free(current_absolute_path);

    var selector = try DirectorySelector.init(alloc, current_absolute_path);
    defer selector.deinit();

    const top_div_height = 1;
    const bottom_div_height = 1;

    // We'll adjust the color index every keypress
    var color_idx: u8 = 0;
    const msg = "Hello, world!";

    if (selector.root.children) |unwrapped_children| {
        if (unwrapped_children.items.len > 0) {
            const child_name = unwrapped_children.items[0].name;
            try selector.appendToSelectedPath(child_name);
            //std.debug.print("Selected path: {s}\n", .{selector.selected_path});
        }
    }
    var test_mode = false;

    // The main event loop. Vaxis provides a thread safe, blocking, buffered
    // queue which can serve as the primary event queue for an application
    while (true) {
        //std.debug.print("Selected path at start of loop: {s}\n", .{selector.selected_path});
        // nextEvent blocks until an event is in the queue
        const event = loop.nextEvent();
        // std.debug.print("Selected path: {s}\n", .{selector.selected_path});
        // log.debug("event: {}", .{event});
        // exhaustive switching ftw. Vaxis will send events if your Event
        // enum has the fields for those events (ie "key_press", "winsize")
        const children = selector.root.children;

        switch (event) {
            .key_press => |key| {
                color_idx = switch (color_idx) {
                    255 => 0,
                    else => color_idx + 1,
                };
                if (key.codepoint == 'c' and key.mods.ctrl) {
                    break;
                } else if (key.matches(vaxis.Key.tab, .{}) or key.codepoint == 'j') {
                    try navigateToSibling(&selector, 1, current_absolute_path);
                } else if (key.codepoint == 'k') {
                    try navigateToSibling(&selector, -1, current_absolute_path);
                } else if (key.codepoint == 'l') {
                    const selected_entry = selector.findEntryFromPath(selector.selected_path[current_absolute_path.len..]).?;
                    if (selected_entry.kind == .directory) {
                        try navigateToFirstChild(&selector, selected_entry);
                    } else {
                        try vx.exitAltScreen(tty.anyWriter());
                        try vx.resetState(tty.anyWriter());
                        loop.stop();
                        try openFileInEditor(&selector, selected_entry, alloc);
                        //std.debug.print("Selected path: {s}\n", .{selector.selected_path});
                        try loop.start();
                        try vx.enterAltScreen(tty.anyWriter());
                        try vx.enableDetectedFeatures(tty.anyWriter());
                        vx.queueRefresh();
                    }
                } else if (key.codepoint == 't') {
                    test_mode = !test_mode;
                    //if (children) |unwrapped_children| {
                    //    if (unwrapped_children.items.len > 0) {
                    //        const child = unwrapped_children.items[cursor_index];
                    //        try selector.toggleDirectoryState(child.uuid);
                    //    }
                    //}
                } else if (key.codepoint == 'h') {
                    const last_slash_index = std.mem.lastIndexOfScalar(u8, selector.selected_path[0..selector.selected_path_len], '/') orelse return error.NoParentDirectory;
                    if (last_slash_index == current_absolute_path.len) {
                        // We're at the root, do nothing
                        continue;
                    }
                    navigateToParent(&selector);
                    const selected_entry = selector.findEntryFromPath(selector.selected_path[current_absolute_path.len..]).?;
                    if (selected_entry.kind == .directory) {
                        selected_entry.is_open = false;
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

        // vx.window() returns the root window. This window is the size of the
        // terminal and can spawn child windows as logical areas. Child windows
        // cannot draw outside of their bounds
        const win = vx.window();
        // Clear the entire space because we are drawing in immediate mode.
        // vaxis double buffers the screen. This new frame will be compared to
        // the old and only updated cells will be drawn
        win.clear();

        // Create some child window. .expand means the height and width will
        // fill the remaining space of the parent. Child windows do not store a
        // reference to their parent: this is true immediate mode. Do not store
        // windows, always create new windows each render cycle
        var main_win = win.child(.{
            .x_off = 0,
            .y_off = 0,
            .width = .{ .limit = win.width / 2 },
            .height = .{ .limit = win.height },
            //.border = .{ .where = .all },
        });

        //std.debug.print("Selected path: {s}\n", .{selector.selected_path});
        const cur_path = selector.selected_path[current_absolute_path.len + 1 ..];

        _ = try printChildren(&main_win, children, 0, 0, cur_path);

        _ = win.child(.{
            .x_off = 0,
            .y_off = 0,
            .width = .{ .limit = win.width / 2 },
            .height = .{ .limit = bottom_div_height },
            //.border = .{ .where = .all },
        });

        const preview_header_win = win.child(.{
            .x_off = win.width / 2,
            .y_off = 0,
            .width = .{ .limit = win.width },
            .height = .{ .limit = top_div_height },
        });

        const preview_win = win.child(.{
            .x_off = win.width / 2,
            .y_off = top_div_height + 1,
            .width = .{ .limit = win.width / 2 },
            .height = .{ .limit = win.height - (preview_header_win.height + top_div_height) },
            .border = .{ .where = .all, .glyphs = .single_square },
        });

        // Loop through the message and print the cells to the screen
        for (msg, 0..) |_, i| {
            const cell: Cell = .{
                // each cell takes a _grapheme_ as opposed to a single
                // codepoint. This allows Vaxis to handle emoji properly,
                // particularly with terminals that the Unicode Core extension
                // (IE Mode 2027)
                .char = .{ .grapheme = msg[i .. i + 1] },
                .style = .{
                    .fg = .{ .index = color_idx },
                },
            };
            preview_win.writeCell(i, 0, cell);
        }
        //std.debug.print("Selected path at end of loop: {s}\n", .{selector.selected_path});
        // Render the screen
        try vx.render(tty.anyWriter());
    }
}

const MAX_HISTORY = 10; // Adjust as needed
const MAX_ENTRIES = 10000; // Maximum number of FileEntry items

const DirectoryState = struct {
    path: []const u8,
    open_uuids: std.ArrayList([36]u8),

    fn init(allocator: std.mem.Allocator, path: []const u8) !DirectoryState {
        return DirectoryState{
            .path = try allocator.dupe(u8, path),
            .open_uuids = std.ArrayList([36]u8).init(allocator),
        };
    }

    fn deinit(self: *DirectoryState) void {
        self.open_uuids.allocator.free(self.path);
        self.open_uuids.deinit();
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
        // zero out the rest of the buffer
        @memset(self.selected_path[path.len..], 0);
        self.selected_path_len = path.len;
    }

    pub fn setSelectedPathToSibling(self: *DirectorySelector, sibling_name: []const u8) !void {
        // Find the last slash in the current path
        const last_slash_index = std.mem.lastIndexOfScalar(u8, self.selected_path[0..self.selected_path_len], '/') orelse return error.NoParentDirectory;

        // Calculate the new length
        const new_len = last_slash_index + 1 + sibling_name.len;

        // Ensure the new path fits in the buffer
        if (new_len > self.selected_path.len) {
            return error.PathTooLong;
        }

        // Copy the sibling name after the last slash
        @memcpy(self.selected_path[last_slash_index + 1 .. new_len], sibling_name);

        // Clear any remaining characters from the old path
        if (new_len < self.selected_path_len) {
            @memset(self.selected_path[new_len..self.selected_path_len], 0);
        }

        // Update the length
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

    pub fn changeDirectory(self: *DirectorySelector, new_path: []const u8) !void {
        try self.saveCurrentState();

        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.history_allocator);
        self.entry_count = 0;

        self.root = try FileEntry.init(self.arena.allocator(), new_path, .directory);
        try self.setSelectedPath(new_path);
        try self.loadChildren(self.root, 0);
    }

    fn loadChildren(self: *DirectorySelector, entry: *FileEntry, depth: usize) !void {
        if (entry.kind != .directory or depth >= 2 or self.entry_count >= MAX_ENTRIES) return;

        var path_buf: [std.fs.max_path_bytes]u8 = undefined;
        const path_len = @min(self.selected_path_len, std.fs.max_path_bytes);
        @memcpy(path_buf[0..path_len], self.selected_path[0..path_len]);

        // Create a slice of the actual path data
        const path_slice = path_buf[0..path_len];

        var dir = std.fs.openDirAbsolute(path_slice, .{ .iterate = true }) catch |err| {
            if (err == error.FileNotFound) {
                std.debug.print("Directory not found: {s}\n", .{entry.name});
                return;
            }
            return err; // Propagate other errors
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
        var it = std.mem.split(u8, path, "/");
        var current_entry: ?*FileEntry = self.root;

        while (it.next()) |segment| {
            if (segment.len == 0) continue;

            if (current_entry) |entry| {
                if (entry.children) |children| {
                    var found = false;
                    for (children.items) |*child| {
                        if (std.mem.startsWith(u8, segment, child.name)) {
                            current_entry = child;
                            found = true;
                            break;
                        }
                    }
                    if (!found) {
                        return null; // Path segment not found
                    }
                } else {
                    return null; // Current entry is not a directory
                }
            } else {
                return null; // Current entry is null
            }
        }

        return current_entry;
    }
};

const FileEntry = struct {
    name: []const u8,
    kind: std.fs.File.Kind,
    children: ?std.ArrayList(FileEntry),
    uuid: [36]u8,
    is_open: bool,

    fn init(allocator: std.mem.Allocator, name: []const u8, kind: std.fs.File.Kind) !*FileEntry {
        var entry = try allocator.create(FileEntry);
        entry.* = FileEntry{
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
    const selected_entry = selector.findEntryFromPath(selector.selected_path[current_absolute_path.len..]).?;
    // Find the last slash in the current path
    const last_slash_index = std.mem.lastIndexOfScalar(u8, selector.selected_path[0..selector.selected_path_len], '/') orelse return error.NoParentDirectory;

    // The parent path is everything up to the last slash
    const parent_path = selector.selected_path[0..last_slash_index];
    const parent_path_minus_absolute = parent_path[current_absolute_path.len..];
    const parent_entry = selector.findEntryFromPath(parent_path_minus_absolute) orelse selector.root;

    const index_of_selected = findChildIndex(parent_entry, selected_entry) orelse {
        std.debug.print("Error finding index of selected\n", .{});
        return;
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
    selector.setSelectedPathToSibling(child.name) catch {
        std.debug.print("Error setting selected path to sibling\n", .{});
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
        if (children.items.len > 0)
            children.items[0]
        else
            null
    else
        null;
    if (first_child == null) {
        return;
    }
    try selector.appendToSelectedPath(first_child.?.name);
}

fn navigateToParent(selector: *DirectorySelector) void {
    selector.popFromSelectedPath();
}

fn openFileInEditor(selector: *DirectorySelector, selected_file_entry: *FileEntry, alloc: std.mem.Allocator) !void {
    if (selected_file_entry.kind == .directory) return;
    var path_buf: [std.fs.max_path_bytes]u8 = undefined;

    const path_slice = path_buf[0..selector.selected_path_len];
    @memcpy(path_buf[0..selector.selected_path_len], selector.selected_path[0..selector.selected_path_len]);

    const editor_slice = "nvim";

    var child = std.process.Child.init(&.{ editor_slice, path_slice }, alloc);
    _ = try child.spawnAndWait();
}

// Our Event. This can contain internal events as well as Vaxis events.
// Internal events can be posted into the same queue as vaxis events to allow
// for a single event loop with exhaustive switching. Booya
const Event = union(enum) {
    key_press: vaxis.Key,
    winsize: vaxis.Winsize,
    focus_in,
};
