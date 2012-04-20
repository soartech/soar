function tiles = init_tiles

tile.shape = [...
    1 1 1 1;
    0 0 0 0;
    0 0 0 0;
    0 0 0 0];
tiles(1) = tile;

tile.shape = [...
    1 1 0 0;
    1 1 0 0;
    0 0 0 0;
    0 0 0 0];
tiles(2) = tile;

tile.shape = [...
    0 1 0 0;
    1 1 0 0;
    0 1 0 0;
    0 0 0 0];
tiles(3) = tile;

tile.shape = [...
    1 1 0 0;
    0 1 1 0;
    0 0 0 0;
    0 0 0 0];
tiles(4) = tile;

tile.shape = [...
    0 1 1 0;
    1 1 0 0;
    0 0 0 0;
    0 0 0 0];
tiles(5) = tile;

tile.shape = [...
    1 1 1 0;
    0 0 1 0;
    0 0 0 0;
    0 0 0 0];
tiles(6) = tile;

tile.shape = [...
    0 0 1 0;
    1 1 1 0;
    0 0 0 0;
    0 0 0 0];
tiles(7) = tile;


for z = 1:7,
    tile = tiles(z).shape;
    for rot = 1:4,
        while all(tile(:,1)==0),
            tile = tile(:,[2,3,4,1]);
        end;
        while all(tile(1,:)==0),
            tile = tile([2,3,4,1],:);
        end;
        tiles(z).rotshape{rot} = tile;
        
        for k = 1:4,
            t=-50;
            for j=1:4,
                if tile(j,k) == 1,
                    t=j;
                end;
            end;
            bottom(k) = t;
        end;
        tiles(z).bottom{rot} = bottom;
        tile = rot90(tile);
    end;
end;

