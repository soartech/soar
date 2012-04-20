function clear_board

global tetris_vars

h = tetris_vars.height;
w = tetris_vars.width;
PADDING = tetris_vars.PADDING;

tetris_vars.board(:) = tetris_vars.T_WALL;
tetris_vars.board(1:(PADDING+h),PADDING+(1:w)) = tetris_vars.T_EMPTY;

