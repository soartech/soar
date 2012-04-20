function [res,gridout] = put_tile(piece, rot, x_ofs)

global tetris_vars

PADDING = tetris_vars.PADDING;
tile = tetris_vars.tiles(piece).rotshape{rot+1};
bottom = tetris_vars.tiles(piece).bottom{rot+1};
gridout = tetris_vars.board;
x_ofs = double(x_ofs);
z = tetris_vars.skyline(PADDING+(1:4)+x_ofs) - bottom;
y_ofs = min(z)-1;
if y_ofs>=0,
    % "*10" is only here for debugging convenience: when printing the
    % board, it is easy to recognize the last placed piece. Any non-0
    % values are OK.
    gridout(y_ofs+(1:4),PADDING+x_ofs+(1:4)) = tetris_vars.board(y_ofs+(1:4),PADDING+x_ofs+(1:4)) + tile*10;
    [res, gridout] = erase_lines(gridout, y_ofs);
else
    res = -1;
end;
% disp(gridout)
% disp([piece, rot, x_ofs])
