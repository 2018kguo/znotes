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

    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    const current_absolute_path = try std.fs.cwd().realpathAlloc(alloc, ".");
    defer alloc.free(current_absolute_path);

    var selector = try DirectorySelector.init(alloc, current_absolute_path);
    defer selector.deinit();
    //const arena_alloc = arena.allocator();

    //const path = "."; // Current directory
    //var timer = try Timer.start();
    //_ = try iterateDir(arena_alloc, path, 0);
    //const elapsed2: f64 = @floatFromInt(timer.read());
    //print("Time elapsed is: {d:.3}ms\n", .{
    //    elapsed2 / time.ns_per_ms,
    //});

    //try printTree(arena_alloc, root, 0);

    const top_div_height = 1;
    const bottom_div_height = 1;

    // We'll adjust the color index every keypress
    var color_idx: u8 = 0;
    const msg = "Hello, world!";

    // The main event loop. Vaxis provides a thread safe, blocking, buffered
    // queue which can serve as the primary event queue for an application
    while (true) {
        // nextEvent blocks until an event is in the queue
        const event = loop.nextEvent();
        // log.debug("event: {}", .{event});
        // exhaustive switching ftw. Vaxis will send events if your Event
        // enum has the fields for those events (ie "key_press", "winsize")
        switch (event) {
            .key_press => |key| {
                color_idx = switch (color_idx) {
                    255 => 0,
                    else => color_idx + 1,
                };
                if (key.codepoint == 'c' and key.mods.ctrl) {
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
        _ = win.child(.{
            .x_off = 0,
            .y_off = 0,
            .width = .{ .limit = win.width / 2 },
            .height = .{ .limit = win.height },
            //.border = .{ .where = .all },
        });

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
        // Render the screen
        try vx.render(tty.anyWriter());
    }
}

const MAX_HISTORY = 10; // Adjust as needed
const MAX_ENTRIES = 5000; // Maximum number of FileEntry items

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
    selected_path: [1024]u8,
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

        self.root = try FileEntry.init(self.arena.allocator(), path, .directory);
        try self.loadChildren(self.root, 0);
        try self.setSelectedPath(path);

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
        self.selected_path_len = path.len;
    }

    pub fn changeDirectory(self: *DirectorySelector, new_path: []const u8) !void {
        try self.saveCurrentState();

        self.arena.deinit();
        self.arena = std.heap.ArenaAllocator.init(self.history_allocator);
        self.entry_count = 0;

        self.root = try FileEntry.init(self.arena.allocator(), new_path, .directory);
        try self.loadChildren(self.root, 0);
        try self.setSelectedPath(new_path);
    }

    fn loadChildren(self: *DirectorySelector, entry: *FileEntry, depth: usize) !void {
        if (entry.kind != .directory or depth >= 2 or self.entry_count >= MAX_ENTRIES) return;

        var dir = try std.fs.cwd().openDir(entry.name, .{ .iterate = true });
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
                if (entry.is_open and entry.children == null) {
                    entry.children = std.ArrayList(FileEntry).init(self.arena.allocator());
                    try self.loadChildren(entry, 0);
                }
            }
        }
    }

    fn findEntry(self: *DirectorySelector, uuid: [36]u8) ?*FileEntry {
        return self.root.findEntryRecursive(uuid);
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

//const DirectorySelector = struct {
//
//    idx: usize,
//
//    fn init(allocator: std.mem.Allocator, path: []const u8) !DirectorySelector {
//        const dir = try std.fs.cwd().openDir(path, .{ .iterate = true });
//        return DirectorySelector{ .dir = dir, .idx = 0 };
//    }
//
//    fn next() !std.fs.File {
//        const entry = try this.dir.iterate().next();
//        return entry;
//    }
//
//    fn deinit() void {
//        this.dir.close();
//    }
//};
//
//const FileEntry = struct {
//    name: []const u8,
//    kind: std.fs.File.Kind,
//    children: ?std.ArrayList(FileEntry),
//    uuid: [36]u8,
//
//    fn init(allocator: std.mem.Allocator, name: []const u8, kind: std.fs.File.Kind) !FileEntry {
//        const uuid = UUID.init();
//        var uuid_str: [36]u8 = undefined;
//        uuid.to_string(&uuid_str);
//        return FileEntry{
//            .name = try allocator.dupe(u8, name),
//            .kind = kind,
//            .children = if (kind == .directory) try std.ArrayList(FileEntry).initCapacity(allocator, 10) else null,
//            .uuid = uuid_str,
//        };
//    }
//
//    fn deinit(self: *FileEntry, allocator: std.mem.Allocator) void {
//        allocator.free(self.name);
//        if (self.children) |*children| {
//            for (children.items) |*child| {
//                child.deinit(allocator);
//            }
//            children.deinit();
//        }
//    }
//};
//
//fn iterateDir(allocator: std.mem.Allocator, path: []const u8, depth: usize) !FileEntry {
//    var dir = try std.fs.cwd().openDir(".", .{ .iterate = true });
//    defer dir.close();
//
//    var root = try FileEntry.init(allocator, path, .directory);
//    errdefer root.deinit(allocator);
//
//    var it = dir.iterate();
//    while (try it.next()) |entry| {
//        if (depth >= 2 and entry.kind == .directory) continue;
//
//        var file_entry = try FileEntry.init(allocator, entry.name, entry.kind);
//        errdefer file_entry.deinit(allocator);
//
//        if (entry.kind == .directory and depth < 2) {
//            const subpath = try std.fs.path.join(allocator, &[_][]const u8{ path, entry.name });
//            defer allocator.free(subpath);
//            file_entry = try iterateDir(allocator, subpath, depth + 1);
//        }
//
//        try root.children.?.append(file_entry);
//    }
//
//    return root;
//}

//fn printTree(allocator: std.mem.Allocator, entry: FileEntry, depth: usize) !void {
//    const indent_step = "  ";
//    const total_indent_len = depth * indent_step.len;
//
//    var indent = try allocator.alloc(u8, total_indent_len);
//    defer allocator.free(indent);
//
//    var i: usize = 0;
//    while (i < depth) : (i += 1) {
//        @memcpy(indent[i * indent_step.len .. (i + 1) * indent_step.len], indent_step);
//    }
//
//    try std.io.getStdOut().writer().print("{s}{s} {s}\n", .{ indent, @tagName(entry.kind), entry.name });
//
//    if (entry.children) |children| {
//        for (children.items) |child| {
//            try printTree(allocator, child, depth + 1);
//        }
//    }
//}

//fn printTree(entry: FileEntry, depth: usize) !void {
//    const indent = "  " ** depth;
//    std.debug.print("{s}{s} {s}\n", .{ indent, @tagName(entry.kind), entry.name });
//
//    if (entry.children) |children| {
//        for (children.items) |child| {
//            try printTree(child, depth + 1);
//        }
//    }
//}

// Our Event. This can contain internal events as well as Vaxis events.
// Internal events can be posted into the same queue as vaxis events to allow
// for a single event loop with exhaustive switching. Booya
const Event = union(enum) {
    key_press: vaxis.Key,
    winsize: vaxis.Winsize,
    focus_in,
};
