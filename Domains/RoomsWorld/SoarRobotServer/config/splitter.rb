def splitter(room, max, x, y, w, h, size)

    horiz = true
    remain = if w > h then 
        temp = w
        w = size
        temp
    else 
        temp = h 
        h = size
        horiz = false
        temp
    end

    until remain / size == 1 do
        printf("\t%3d = [ %3d, %3d, %3d, %3d ];\n", room, x, y, w, h)

        if horiz 
            x, y = x + size, y 
        else 
            x, y = x, y + size 
        end
        remain -= size
        room, max = max + 1, max + 1
    end
    if horiz 
        w, h = remain, h 
    else 
        w, h = w, remain 
    end
    printf("\t%3d = [ %3d, %3d, %3d, %3d ];\n", room, x, y, w, h)
    room
end

max = 198
max = splitter(72, max, 59, 456, 152, 8, 38)
max = splitter(113, max, 193, 137, 12, 212, 53)
max = splitter(17, max, 57, 174, 10, 161, 53)
max = splitter(101, max, 68, 350, 146, 15, 48)
max = splitter(37, max, 47, 366, 11, 89, 29)
max = splitter(99, max, 264, 369, 143, 10, 35)

