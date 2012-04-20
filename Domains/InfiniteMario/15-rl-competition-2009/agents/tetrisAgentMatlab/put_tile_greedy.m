function res = put_tile_greedy(piece)
%       Tries to find a good placement for a tetromino.
%       assigns values to "bestrot" and "bestpos"
%       Put your awesome learning algorithm here.
%       
%       piece:       the type of the tetromino to place. an integer from 1 to 7.
%       returns      the number of lines erased by the proposed placement,
%                    or -1 if the tile cannot be placed.

    global tetris_vars

    tetris_vars.skyline = get_skyline;
    bestvalue = -100000;
    bestrot = 0;
    bestpos = 0;
    % we calculate the value of each possible placement
    for rot=0:3,
        for pos=0:tetris_vars.width-1,
            [res, workboard] = put_tile(piece, rot, pos);            
            if(res >= 0)
                % we calculate the number of holes and column heights
                colheight = zeros(tetris_vars.width,1);
                n_holes = 0;
                for x = 1:tetris_vars.width,
                    nz = find(workboard(:,x+tetris_vars.PADDING));
                    colheight(x) = tetris_vars.height-tetris_vars.PADDING-nz(1)+1;
                    n_holes = n_holes + colheight(x) - length(nz)+1;
                end;
                maxh = max(colheight);
                
                % a very simple heuristic evaluation function... 
                % your algorithm should learn something better ;-)
                value = -maxh-0.1*n_holes;
                
            else
                value = -100000;
            end;
            if(value > bestvalue)
                bestvalue = value;
                bestrot = rot;
                bestpos = pos;
            end;
        end;
    end;

    % if the best placement is legal, then we do it on the real board, too.
    if(bestvalue > -100000)
        [res, workboard] = put_tile(piece, bestrot, bestpos);
    else
        res = -1;
    end;
    
    tetris_vars.bestpos = bestpos;
    tetris_vars.bestrot = bestrot;