
% TO USE THIS Agent on its own[order doesn't matter]
%   -  Start the rl_glue executable socket server on your computer
%   -  Run the SkeletonEnvironment and SkeletonExperiment from a
%   different codec (Python, Java, C, Lisp should all be fine)
%   -  Load this agent like:
%       >> theAgent=tetris_agent()
%       >> runAgent(theAgent);
%   NOTE: Type CTRL-C to abort the connection.
%

function theAgent=tetris_agent()
    theAgent.agent_init=@tetris_agent_init;
    theAgent.agent_start=@tetris_agent_start;
    theAgent.agent_step=@tetris_agent_step;
    theAgent.agent_end=@tetris_agent_end;
    theAgent.agent_cleanup=@tetris_agent_cleanup;
    theAgent.agent_message=@tetris_agent_message;
end

function tetris_agent_init(taskSpec)
    global tetris_vars;
    
    tetris_vars.MAXWIDTH = 20;
    tetris_vars.MAXHEIGHT = 40;
    tetris_vars.PADDING = 3;
    tetris_vars.T_WALL = 8;
    tetris_vars.T_EMPTY = 0;

    tetris_vars.firstActionOfEpisode = true;
    tetris_vars.TSO = org.rlcommunity.rlglue.codec.taskspec.TaskSpec(taskSpec);


%    tetris_vars.action = org.rlcommunity.rlglue.codec.types.Action(tetris_vars.TSO.getNumDiscreteActionDims(),tetris_vars.TSO.getNumDiscreteActionDims());	
    tetris_vars.totalRew = 0;
    tetris_vars.totalSteps = 0;
    
end    

function theAction=tetris_agent_start(o)
%This is a persistent struct we will use to store things
%that we want to keep around
    global tetris_vars;

    len = size(o.intArray,1);
    tetris_vars.height = o.intArray(len-2+1); % when converting form JAVA to MATLAB, indices need to be incremented by 1
    tetris_vars.width  = o.intArray(len-1+1);
    tetris_vars.board = zeros(tetris_vars.width + 2*tetris_vars.PADDING,tetris_vars.height + 2*tetris_vars.PADDING)';
    tetris_vars.workboard = zeros(tetris_vars.width + 2*tetris_vars.PADDING,tetris_vars.height + 2*tetris_vars.PADDING)';
    tetris_vars.tiles = init_tiles;
    %                         I   O   T   Z   S   J   L
    tetris_vars.posshift = [[ 0,  0, -1,  0, -1,  0, -2];
                            [ 2,  0, -1,  0, -1,  1, -2];
                            [ 0,  0,  0,  0, -1,  0, -2];
                            [ 2,  0, -1,  0, -1,  0, -1]];
    tetris_vars.totalSteps = 0;
    tetris_vars.totalRew = 0;
    %skyline = new int[width + 2*PADDING];
    %bestrot = 0;
    %bestpos = 0;
    
%     theAction = org.rlcommunity.rlglue.codec.types.Action();
% 	theAction.intArray=[4];

    theAction=tetris_agent_step(0, o);
    
% 	%Make copies (using Java methods) of the observation and action
% 	%Store in our persistent struct
% 	tetris_vars.lastObservation=o.duplicate();	
% 	tetris_vars.lastAction=theAction.duplicate();
end

function theAction=tetris_agent_step(theReward, o)
	%This is a persistent struct we will use to store things
	%that we want to keep around
	    global tetris_vars;

        % some bookkeeping 
        tetris_vars.totalSteps = tetris_vars.totalSteps+1;
        tetris_vars.totalRew = tetris_vars.totalRew + theReward;
        % shorthand variables
        len = size(o.intArray,1);
        w = tetris_vars.width;
        h = tetris_vars.height;
        PADDING = tetris_vars.PADDING;

        % get current piece from observation
        piece = find(o.intArray(len-9+(1:7)));
        % copy the current board config from observation
        clear_board;
        tetris_vars.board(PADDING+(1:h), PADDING+(1:w)) = reshape(o.intArray(1:end-9),w,h)';
        
        % if any item in the first line is 1, we got a new piece...
        isnewpiece = any( tetris_vars.board(PADDING+1, PADDING+(1:w))==1);
       
        if (isnewpiece)
            disp(tetris_vars.board)
            pos0 = find(tetris_vars.board(PADDING+1, PADDING+(1:w)))-1;
            pos0 = pos0(1);
            rot = 0;
            pos = pos0 + tetris_vars.posshift(rot+1,piece);
            % we erase the new shape from the board
            tetris_vars.board(PADDING+(1:4),pos+PADDING+(1:4)) = tetris_vars.board(PADDING+(1:4),pos+PADDING+(1:4)) - ...
                tetris_vars.tiles(piece).shape;
            %disp(tetris_vars.board)
            
            % this calculates bestrot and bestpos, and puts them into
            % tetris_vars
            put_tile_greedy(piece);
        else
            pos0 = tetris_vars.pos0;       
            rot = tetris_vars.rot;
        end;
        pos = pos0 + tetris_vars.posshift(rot+1,piece);
        bestpos = tetris_vars.bestpos;
        bestrot = tetris_vars.bestrot;
        
        % bestrot and bestpos is set by putTileGreedy, when a new piece arrives.
        % after that, we try to achieve them step-by-step with elementary moves
        % bestrot is a number between 0 and 3, but it may be larger than the 
        % number of _different_ rotations for a given tetromino. 
        % For example, a Z piece can have bestrot=+3, which we modify to +1.
        nrots = 4;
        if ((piece == 1) || (piece==4) || (piece==5)) 
            nrots = 2; %I, S, Z piece
            if (bestrot>=2), bestrot=bestrot-2; end;
        end;
        if (piece ==2) %O piece
            bestrot = 0;
        end;

        %[pos rot bestpos bestrot]
        
        % now we have bestpos and bestrot. this is translated to 
        % a sequence of primitive moves as follows:
        % 1. when the piece is in the first line, we make only right/left moves
        % 2. we rotate the piece to its final position
        % 3. we move the piece to its final position
        % 4. we drop it.
        if ((isnewpiece) && (pos > bestpos))
            a = 0; %left
        elseif ((isnewpiece) && (pos < bestpos))
            a = 1; %right
        elseif ((isnewpiece) && (pos == bestpos))
            a = 4; %do nothing
            % maybe we need to rotate later, but rotation is not always allowed in the first line.
        elseif ((rot ~= bestrot) && ((rot == bestrot+1) || (rot+nrots == bestrot+1)))
            a = 3; %rotate left
        elseif ((rot ~= bestrot) && ((rot == bestrot-1) || (rot-nrots == bestrot-1)))
            a = 2; %rotate right
        elseif ((rot ~= bestrot) && ((rot == bestrot+2) || (rot+nrots == bestrot+2)))
            a = 3; %need to rotate twice, we start by one rotate right
        elseif ((rot == bestrot) && (pos > bestpos))
            a = 0; %left
        elseif ((rot == bestrot) && (pos < bestpos))
            a = 1; %right
        elseif ((rot == bestrot) && (pos == bestpos))
            a = 5; %drop
        else 
            % there is some kind of problem. There should not be any.
            fprintf('PROBLEM WITH %d: (%d,%d) vs (%d,%d) \n', piece, pos,rot,bestpos,bestrot);
            a = 4; 
        end;
 	    theAction = org.rlcommunity.rlglue.codec.types.Action();
		theAction.intArray=[a];

%         * 0 - left
%         * 1 - right
%         * 2 - rotate left (counterclockwise)
%         * 3 - rotate right (clockwise)
%         * 4 - do nothing
%         * 5 - put down

        % depending on the action taken, we modify our own model of the game state
        switch (a)
            case 0,
                pos0 = pos0-1;
            case 1,
                pos0 = pos0+1;
            case 3,
                rot = rot-1;
                if (rot<0), rot = 3; end;
            case 2,
                rot = rot+1;
                if (rot>=4), rot = 0; end;
        end;

        if (((piece == 1) || (piece==4) || (piece==5)) && (rot>=2))
            rot = rot-2;
        end;
        if (piece==2)
            rot=0;
        end;
        
        % save computed values to persistent structure
        tetris_vars.pos0 = pos0;
        tetris_vars.rot = rot;
        
		%Make copies (using Java methods) of the observation and action
		%Store in our persistent struct
		tetris_vars.lastAction=theAction.duplicate();
		tetris_vars.lastObservation=o.duplicate();
end

function tetris_agent_end(theReward)
        global tetris_vars;
        
        tetris_vars.totalRew = tetris_vars.totalRew + theReward;
end

function returnMessage=tetris_agent_message(theMessageJavaObject)
%Java strings are objects, and we want a Matlab string
    inMessage=char(theMessageJavaObject);
	returnMessage='I don\''t know how to respond to your message';
end

function tetris_agent_cleanup()
    global tetris_vars;
     fprintf('Printing some stats: \t steps:%d \t reward:%.2f \n', tetris_vars.totalSteps, tetris_vars.totalRew );
end