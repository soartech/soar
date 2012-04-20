function top = get_skyline

global tetris_vars

for i = 1:size(tetris_vars.board,2),
    [m,k] = min(tetris_vars.board(:,i)==0);
    top(i) = k;
end;
