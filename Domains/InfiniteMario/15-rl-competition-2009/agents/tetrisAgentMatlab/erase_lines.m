function [n_erased, grid] = erase_lines(grid,y_ofs)

global tetris_vars;

n_erased = 0; 

PADDING = tetris_vars.PADDING;
w = size(grid,2);

for j = y_ofs+1:y_ofs+4,
    % if the whole row is nonzero and there is a non-wall block, then it is
    % a full line.
    if all(grid(j,1:w)) && (grid(j,PADDING+2)~=tetris_vars.T_WALL),
        newline = ones(1,w)*tetris_vars.T_WALL;
        newline(PADDING+(1:tetris_vars.width)) = 0;
        grid(j,:) = [];
        grid = [newline;grid];
        n_erased = n_erased+1;
%        disp('Juhejj');
    end;
end;