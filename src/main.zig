const std = @import("std");
const MB = 1024 * 1024;

var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);

pub fn main() !void {
    defer arena.deinit();

    const args = try std.process.argsAlloc(arena.allocator());
    if (args.len != 2) exitUsage();

    const path = args[1];

    std.log.info("Reading WAD '{s}'", .{path});
    const wad = std.fs.cwd().readFileAlloc(arena.allocator(), path, 30 * MB) catch |err| {
        std.log.err("{s}", .{@errorName(err)});
        exitUsage();
    };

    const header: *WadHeader = @alignCast(@ptrCast(wad[0..@sizeOf(WadHeader)]));
    const directory: []WadEntry = @as(
        [*]WadEntry,
        @alignCast(@ptrCast(&wad[@intCast(header.directory_offset)])),
    )[0..@intCast(header.num_entries)];

    var bw = std.io.bufferedWriter(std.io.getStdOut().writer());
    const writer = bw.writer();

    for (directory) |entry| {
        const tag = entry.getType() orelse continue;
        switch (tag) {
            inline .THINGS,
            .LINEDEFS,
            .SIDEDEFS,
            .VERTEXES,
            .SEGS,
            .SSECTORS,
            .NODES,
            .SECTORS,
            => |t| {
                try printEntryItems(
                    t.toType(),
                    t,
                    wad[@intCast(entry.start)..][0..@intCast(entry.size)],
                    writer,
                );
            },

            .REJECT,
            .BLOCKMAP,
            => std.log.warn("No format for {s}", .{@tagName(tag)}),

            else => std.debug.panic("Unimplemented WadEntry {s}\n", .{@tagName(tag)}),
        }
    }

    try bw.flush();
}

const WadHeader = packed struct(u96) {
    /// 4 byte string, `IWAD` or `PWAD`
    wad_type: u32,
    num_entries: i32,
    directory_offset: i32,

    pub fn getType(wh: WadHeader) enum { IWAD, PWAD } {
        const wtype: [4]u8 = @bitCast(wh.wad_type);
        return if (std.mem.eql(u8, &wtype, "IWAD"))
            .IWAD
        else if (std.mem.eql(u8, &wtype, "PWAD"))
            .PWAD
        else
            @panic("Invalid WAD type");
    }
};

const WadEntry = packed struct(u128) {
    start: i32,
    /// in bytes
    size: i32,
    /// max 8 byte string. Is null terminated if shorter than 8 bytes.
    name: u64,

    pub fn getType(we: WadEntry) ?Tag {
        const str: [8]u8 = @bitCast(we.name);
        return Tag.fromName(std.mem.sliceTo(&str, 0));
    }

    pub const Tag = enum {
        THINGS,
        LINEDEFS,
        SIDEDEFS,
        VERTEXES,
        SEGS,
        SSECTORS,
        NODES,
        SECTORS,
        REJECT,
        BLOCKMAP,
        BEHAVIOR,
        SCRIPTS,

        pub fn fromName(n: []const u8) ?Tag {
            inline for (@typeInfo(Tag).Enum.fields) |field| {
                if (std.mem.eql(u8, n, field.name))
                    return @enumFromInt(field.value);
            }
            return null;
        }

        pub fn toType(comptime t: Tag) type {
            return switch (t) {
                .THINGS => Lumps.Thing,
                .LINEDEFS => Lumps.LineDef,
                .SIDEDEFS => Lumps.SideDef,
                .VERTEXES => Lumps.Vertex,
                .SEGS => Lumps.Segment,
                .SSECTORS => Lumps.SubSector,
                .NODES => Lumps.Node,
                .SECTORS => Lumps.Sector,
                else => unreachable,
            };
        }
    };
};

const Lumps = struct {
    pub const Thing = packed struct(u80) {
        xpos: i16,
        ypos: i16,
        angle: u16,
        type: u16,
        // TODO: find a spec for these
        spawn_flags: u16,
    };

    pub const LineDef = packed struct(u112) {
        start_vertex: u16,
        end_vertex: u16,
        flags: u16,
        line_type: u16,
        sector_tag: u16,
        right_sidedef: u16,
        left_sidedef: u16,
    };

    pub const SideDef = packed struct(u240) {
        x_offset: i16,
        y_offset: i16,
        /// packed string
        upper_texture: u64,
        /// packed string
        lower_texture: u64,
        /// packed string
        middle_texture: u64,
        sector_id: u16,
    };

    pub const Vertex = packed struct(u32) {
        x: i16,
        y: i16,
    };

    pub const Segment = packed struct(u96) {
        start_vertex: u16,
        end_vertex: u16,
        angle: i16,
        line_def: u16,
        direction: i16,
        offset: i16,
    };

    pub const SubSector = packed struct(u32) {
        num_segments: u16,
        first_segment: u16,
    };

    pub const Node = packed struct(u224) {
        partition_x: i16,
        partition_y: i16,
        dx_to_partition_end: i16,
        dy_to_partition_end: i16,

        right_bbox: u64,
        left_bbox: u64,

        right_child: i16,
        left_child: i16,
    };

    pub const Sector = packed struct(u208) {
        floor_height: i16,
        ceiling_height: i16,
        floor_texture: u64,
        ceiling_texture: u64,
        light_level: i16,
        sector_special: u16,
        sector_tag: u16,
    };
};

fn printWad(header: WadHeader, directory: []WadEntry) !void {
    var bw = std.io.bufferedWriter(std.io.getStdOut().writer());
    const writer = bw.writer();
    try writer.print(
        "WAD type: {s}, Entries: {d}, Directory Offset: 0x{X}\n\n",
        .{ @as([4]u8, @bitCast(header.wad_type)), header.num_entries, header.directory_offset },
    );

    _ = try writer.write("Entries:\n");
    try bw.flush();

    try printDirectory(directory, writer);
    try bw.flush();
}

fn printDirectory(directory: []WadEntry, writer: anytype) !void {
    var entry_table = @import("Table.zig").Table(&.{ "Name", "Offset", "Size" }).init(arena.allocator());
    defer entry_table.deinit();

    for (directory) |entry| {
        const name = @as([8]u8, @bitCast(entry.name));
        const name_len = std.mem.indexOfScalar(u8, &name, 0) orelse name.len;

        try entry_table.entry(0, "{s}", .{name[0..name_len]});
        try entry_table.entry(1, "0x{X}", .{entry.start});
        try entry_table.entry(2, "{d}", .{entry.size});
    }
    try entry_table.print_out(writer);
}

fn exitUsage() noreturn {
    std.log.err("Usage: wawd <filepath>", .{});
    std.process.exit(1);
}

fn printEntryItems(comptime E: type, comptime tag: WadEntry.Tag, bytes: []u8, writer: anytype) !void {
    const S = @typeInfo(E).Struct;

    const titles = comptime blk: {
        var buf: [S.fields.len][]const u8 = undefined;
        inline for (S.fields, 0..) |f, i| {
            buf[i] = f.name;
        }
        break :blk buf;
    };

    var entry_table = @import("Table.zig").Table(&titles).init(arena.allocator());
    defer entry_table.deinit();

    // const items = std.mem.bytesAsSlice(E, bytes);
    // Compile error! Packed structs have a backing integer, which must be 2^n bytes!
    // @sizeOf(packed struct(u80)) == 16 bytes != 80 bits == 10 bytes
    const Packed = std.packed_int_array.PackedIntIo(S.backing_integer.?, .Little);
    const num_items = @divExact(bytes.len, @divExact(@typeInfo(S.backing_integer.?).Int.bits, 8));
    const items = Packed.slice(bytes, 0, 0, num_items);

    _ = try writer.write(@tagName(tag) ++ ":\n");
    for (0..num_items) |i| {
        inline for (S.fields, 0..) |f, j| {
            try entry_table.entry(
                j,
                "{any}",
                .{@field(@as(E, @bitCast(items.get(i))), f.name)},
            );
        }
    }

    try entry_table.print_out(writer);
    try writer.writeByte('\n');
}
