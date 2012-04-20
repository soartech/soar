/* A Simple Tetris Agent 
* Copyright (C) 2009, Istvan Szita
* 
* This program is free software; you can redistribute it and/or
* modify it under the terms of the GNU General Public License
* as published by the Free Software Foundation; either version 2
* of the License, or (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA. */
package tetrisexample;

import java.util.Arrays;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.AgentInterface;
import org.rlcommunity.rlglue.codec.taskspec.TaskSpec;
import org.rlcommunity.rlglue.codec.util.AgentLoader;

public class SampleTetrisAgent implements AgentInterface {


        /* Quick reference: 
         * action:
         * 0 - left
         * 1 - right
         * 2 - rotate left (counterclockwise)
         * 3 - rotate right (clockwise)
         * 4 - do nothing
         * 5 - put down
         * 
         * 
         *  pieces:
         * 0 - I
         * 1 - O
         * 2 - T
         * 3 - S
         * 4 - Z
         * 5 - L
         * 6 - J
         */
	private Action action;
	private int numInts =1;
	private int numDoubles =0;

        boolean firstActionOfEpisode;
        int totalSteps;
        double totalRew;

        
         
        TaskSpec TSO=null;
	
        public static final int  MAXWIDTH = 20;
        public static final int  MAXHEIGHT = 40;
        public static final int PADDING = 3;
        public static final int T_WALL = 8;
        public static final int T_EMPTY = 0;
        int bestrot, bestpos;
        int width, height, piece;
        int[][][][] tiles = new int[7][4][4][4];
        int[][][] tilebottoms = new int[7][4][4];
                
                                    
        public SampleTetrisAgent(){
        }

        /**
         * Called just before unloading the current MDP.
         */
	public void agent_cleanup() {
            System.out.printf("Printing some stats: \t steps:%d \t reward:%.2f \n", totalSteps, totalRew );
	}

        /**
         * Called at the end of an episode (that is, when the board is filled up)
         * @param arg0  the reward received for the last step
         */
	public void agent_end(double arg0) {
            totalRew += arg0;
            
	}

        /**
         * Is never called.
         */
	public void agent_freeze() {

	}

        /**
         * Called when starting a new MDP.
         * 
         * @param taskSpec  the Task Specification Object of the current MDP.
         */
	public void agent_init(String taskSpec) {
            TSO = new TaskSpec(taskSpec);
            firstActionOfEpisode = true;
            

            action = new Action(TSO.getNumDiscreteActionDims(),TSO.getNumDiscreteActionDims());	
            totalRew = 0;
            totalSteps = 0;
	}

	public String agent_message(String arg0) {
            return null;
	}

        
        /**
         * Called when a new episode starts. 
         * We perform some initialization: find out the board width&height,
         * set up an empty board and the arrays that contain rotated versions 
         * of the 7 tetrominoes.
         * After initialization, we call agent_step() to generate a suitable action
         * 
         * @param o the first obrervation of the episode
         * @return  the first action of the episode
         */
	public Action agent_start(Observation o) {
            int len = o.intArray.length;
            height = o.intArray[len-2];
            width  = o.intArray[len-1];
            board = new int[width + 2*PADDING][height + 2*PADDING];
            workboard = new int[width + 2*PADDING][height + 2*PADDING];
            skyline = new int[width + 2*PADDING];
            bestrot = 0;
            bestpos = 0;
            for(int i = 0; i < 7; i++)
            {
                for(int j = 0; j < 4; j++)
                {
                    int tile[][] = generateTile(i, j);
                    for(int x = 0; x < 4; x++)
                    {
                        int last = -100;
                        for(int y = 0; y < 4; y++)
                        {
                            tiles[i][j][x][y] = tile[x][y];
                            if(tile[x][y] != 0)
                                last = y;
                        }

                        tilebottoms[i][j][x] = last;
                    }

                }

            }
            
            action.intArray[0] = 4;
            return agent_step(0, o);
	}


        /**
         * auxiliary array. When a new tetromino arrives at the board, we detect 
         * its type and the position of its upper-left corner. The positions of 
         * the four "minoes" are stored in this array.
         */
        int[][][] dpos = {  {{0,0},{1,0},{2,0},{3,0}},
                            {{0,0},{1,0},{0,1},{1,1}},
                            {{0,0},{-1,1},{0,1},{0,2}},
                            {{0,0},{1,0},{1,1},{2,1}},
                            {{0,0},{1,0},{-1,1},{0,1}},
                            {{0,0},{1,0},{2,0},{2,1}},
                            {{0,0},{0,1},{-1,1},{-2,1}}
        }; 
        
                
        /**
         * auxiliary array. When a new tetromio arrives, we detect its x position 
         * as its upper-left corner. To get the "true position", we have to modify it.
         * PosShift[rot][piece] is the shift required for "piece" in rotation "rot".
         */
        public static final int[][] PosShift     = {
            {0, 0,-1, 0,-1, 0,-2},
            {2, 0,-1, 0,-1, 0,-1},
            {0, 0, 0, 0,-1, 0,-2},
            {0, 0,-1, 0,-1, 1,-2}
        }; 
        // number of different rotations for the tetrominoes.
        public static final int[] nrots = {2,1,4,2,2,4,4}; 
    
        // rawboard contains the currently falling piece
        int[][] rawboard = new int[MAXWIDTH][MAXHEIGHT];
        // board does not contain the currently falling piece, and is padded with zeroes all around
        int[][] board;
        // a work copy of board[][], we try out new placements of tetrominoes here 
        int[][] workboard;
        // heights of columns
        int[] skyline;
        int pos=0, rot=0;
        int pos0 = -1;
        
        /**
         * It selects a primitive action depending on the current game state.
         * Note: the selection logic is in putTileGreedy()
         * 
         * @param arg0  the last reward
         * @param o     the current game state
         * @return      the action taken by the agent
         */
	public Action agent_step(double arg0, Observation o) {
            int len = o.intArray.length;
            int i, j, k, a;
            boolean isnewpiece;
            
            action.intArray[0] = 0;

            totalSteps++;
            totalRew += arg0;
            piece = -1;
            for (i=0; i<7; i++)
            {
                if (o.intArray[len-9+i]==1)
                    piece = i;
            }
            for (i=0; i<width; i++)
                for (j=0; j<height; j++)
                    rawboard[i][j] = o.intArray[j*width+i];

            isnewpiece = false;
            for (i=0; i<width; i++)
            {
                // if there is something in the first line, then we have a new tetromino
                if (rawboard[i][0]!=0)
                {
                    isnewpiece = true;
                    break;
                }
            }
            if (isnewpiece)
            {
                // we overwrite the new piece with a different "color" (2 instead of 1)
                // so that we can separate it from the rest of the board
                // we could also erase it...
                pos0 = i;
                for (k=0; k<4; k++)
                {
                    rawboard[pos0+dpos[piece][k][0]][dpos[piece][k][1]] = 2;
                }
            }   
            /////////////////////////////////////////////////
            
            if (firstActionOfEpisode)
            {
                firstActionOfEpisode = false;
                
            }
            if (isnewpiece)
            {
                rot = 0;
                pos   = pos0 + PosShift[rot][piece];
                clearBoard();
                for (i=0; i<width; i++)
                    for (j=0; j<height; j++)
                    {
                        // note: the new piece is not copied to board!
                        board[i+PADDING][j+PADDING] = 
                                (rawboard[i][j]==1) ? 1:0;
                    }
                // an up-to-date skyline is required for the analysis of the board.
                updateSkyline();
                // putTileGreedy() analyzes the board and sets bestrot and bestpos
                putTileGreedy(piece);
                //debugdrawBoard();
            }
            pos   = pos0 + PosShift[rot][piece];
            
           
            // bestrot and bestpos is set by putTileGreedy, when a new piece arrives.
            // after that, we try to achieve them step-by-step with elementary moves
            // bestrot is a number between 0 and 3, but it may be larger than the 
            // number of _different_ rotations for a given tetromino. 
            // For example, a Z piece can have bestrot=+3, which we modify to +1.
            int nrots = 4;
            if ((piece == 0) || (piece==3) || (piece==4)) nrots = 2; //I, S, Z piece
            if (((piece==0) || (piece==3) || (piece==4)) && (bestrot>=2))
                bestrot-=2;
            if (piece ==1) //O piece
                bestrot = 0;
            // now we have bestpos and bestrot. this is translated to 
            // a sequence of primitive moves as follows:
            // 1. when the piece is in the first line, we make only right/left moves
            // 2. we rotate the piece to its final position
            // 3. we move the piece to its final position
            // 4. we drop it.
            if ((isnewpiece) && (pos > bestpos))
                a = 0; //left
            else if ((isnewpiece) && (pos < bestpos))
                a = 1; //right
            else if ((isnewpiece) && (pos == bestpos))
                a = 4; //do nothing
                // maybe we need to rotate later, but rotation is not always allowed in the first line.
            else if ((rot != bestrot) && ((rot == bestrot+1) || (rot+nrots == bestrot+1)))
                a = 2; //rotate left
            else if ((rot != bestrot) && ((rot == bestrot-1) || (rot-nrots == bestrot-1)))
                a = 3; //rotate right
            else if ((rot != bestrot) && ((rot == bestrot+2) || (rot+nrots == bestrot+2)))
                a = 2; //need to rotate twice, we start by one rotate left
            else if ((rot == bestrot) && (pos > bestpos))
                a = 0; //left
            else if ((rot == bestrot) && (pos < bestpos))
                a = 1; //right
            else if ((rot == bestrot) && (pos == bestpos))
                a = 5; //drop
            else 
            {
                // there is some kind of problem. There should not be any.
                System.out.printf("%d: (%d,%d) vs (%d,%d) \n", piece, pos,rot,bestpos,bestrot);
                a = 4; 
            }
            action.intArray[0] = a;
            
            // print debug info
//                System.out.println(debug2DArrayToString(tiles[piece][rot])+piece);
//                System.out.printf("rot:%d, offset:%d\n\n", rot, PosShift[rot][piece]);

            // depending on the action taken, we modify our model of the game state
            switch (action.intArray[0])
            {
                case 0:
                    pos0--;
                    break;
                case 1:
                    pos0++;
                    break;
                case 2:
                    rot--;
                    if (rot<0) rot = 3;
                    break;
                case 3:
                    rot++;
                    if (rot>=4) rot = 0;
                    break;
                            
//         * 0 - left
//         * 1 - right
//         * 2 - rotate left (counterclockwise)
//         * 3 - rotate right (clockwise)
//         * 4 - do nothing
//         * 5 - put down
            }
            if (((piece == 0) || (piece==3) || (piece==4)) && (rot>=2))
                rot-=2;
            if (piece==1)
                rot=0;
            
            return action;
	}
        
                
    public void clearBoard()
    {
        for(int i = 0; i < board.length; i++)
        {
            for(int j = 0; j < board[i].length; j++)
                board[i][j] = T_WALL;
        }
        for(int i = 0; i < width; i++)
        {
            for(int j = 0; j < height + PADDING; j++)
                board[i + PADDING][j] = T_EMPTY;
        }
        updateSkyline();
    }
    
    public void updateSkyline()
    {
        int i, j;
        for(i=0; i<skyline.length; i++)
        {
            for(j=0; j<board[i].length; j++)
            {
                if (board[i][j] != 0)
                    break;
            }
            skyline[i] = j;
        }
    }

    /**
     * Copies board to workboard
     */
    public void copyWorkBoard()
    {
        for(int i = PADDING; i < width + PADDING; i++)
            System.arraycopy(board[i], 0, workboard[i], 0, height + 2*PADDING);

    }

    public String debug2DArrayToString(int[][] a)
    {
        int i,j;
        int h = a.length; 
        int w = a[0].length;
        String s = "";
        for (j=0; j<h; j++)
        {
            for (i=0; i<w; i++)
                s = s+a[i][j];
            s = s+"\n";
        }       
        return s;
    }
    
    /**
     * Prints an ASCII representation of the board.
     * useful for debugging.
     */
    public void debugdrawBoard()
    {
        for(int j = 0; j < height; j++)
        {
            for(int i = 0; i < width; i++)
                System.out.printf("%d", new Object[] {
                    Integer.valueOf(board[i + PADDING][j + PADDING])
                });

            System.out.println();
        }

        System.out.println("  ");
    }

    static final double MIN_VALUE = -1e10;

    /**
     * Tries to find a good placement for a tetromino of type "type".
     * assigns values to "bestrot" and "bestpos"
     * Put your awesome learning algorithm here.
     * 
     * @param type  the type of the tetromino to place. an integer from 0 to 6.
     * @return      the number of lines erased by the proposed placement,
     *              or -1 if the tile cannot be placed.
     */
    public int putTileGreedy(int type)
    {
        double bestvalue = MIN_VALUE;
        for(int rot = 0; rot < nrots[type]; rot++)
        {
            for(int pos = 0; pos < width; pos++)
            {
                copyWorkBoard();
                int res = putTile(workboard, type, rot, pos);
                double value;
                if(res >= 0)
                {
                    value = getValue(workboard);
                } 
                else
                {
                    value = MIN_VALUE;
                }
                if(value > bestvalue)
                {
                    bestvalue = value;
                    bestrot = rot;
                    bestpos = pos;
                }
            }

        }

        // if the best placement is legal, then we do it on the real board, too.
        if(bestvalue > MIN_VALUE)
        {
            int res = putTile(board, type, bestrot, bestpos);
            updateSkyline();
            return res;
        } else
        {
            return -1;
        }
    }

    /**
     * Puts a tetromino of type "type" on the board "b", wit rotation "rot" and position "pos".
     * @param b     the board to play on. Can be either "board" or "workboard"
     * @param type  type of tetromino to place
     * @param rot   rotation of tetromino
     * @param pos   position of tetromino
     * @return
     */
    public int putTile(int b[][], int type, int rot, int pos)
    {
        int tile[][] = tiles[type][rot];
        int ofs = 10000;
        for(int x = 0; x < 4; x++)
            ofs = Math.min(ofs, skyline[x + pos + PADDING] - tilebottoms[type][rot][x] - 1);

        if(ofs < PADDING)
            return -1;
//        System.out.println(debug2DArrayToString(tile)+type+"\n\n");
        for(int x = 0; x < 4; x++)
        {
            for(int y = 0; y < 4; y++)
                if(tile[x][y] != 0)
                    b[x + pos + PADDING][y + ofs] = type + 1;

        }
//        System.out.println(debug2DArrayToString(b)+"\n\n");

        int lastLandingHeight = ofs;
        int result = eraseLines(b);
        int nholes = 0;
        boolean started = false;
        for(int y = 0; y < height; y++)
        {
            if(b[PADDING][y + PADDING] != 0)
                started = true;
            if(started && b[PADDING][y + PADDING] != 0)
                nholes++;
        }

        int startrot = 0;
        int startpos = (width - 1) / 2;
        if(type == 0)
            startpos--;
        int nm = 1 + Math.abs(startpos - pos);
        if(type >= 3 && Math.abs(startrot - rot) == 2)
            nm += 2;
        if(startrot != rot)
            nm++;
        if(lastLandingHeight < nm + 1)
            result = -1;
        return result;
    }

    /**
     * Returns an array containing a tetromino of the required type and rotation.
     * 
     * @param type  Type of tetromino
     * @param rot   Rotation of tetromino
     * @return      array containing the tetromino
     */
    int [][] generateTile(int type, int rot)
    {
        int [][] t;
        // we copy the basic shape into t
        switch (type)
        {
            case 0: // I
                t = new int[][]{{1, 0, 0, 0}, {1, 0, 0, 0}, {1, 0, 0, 0}, {1, 0, 0, 0}}; 
                break;
            case 1: // O
                t = new int[][]{{1, 1, 0, 0}, {1, 1, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
                break; 
            case 2: // T
                t = new int[][]{{0, 1, 0, 0}, {1, 1, 1, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
                break;                
            case 3: // Z
                t = new int[][]{{1, 0, 0, 0}, {1, 1, 0, 0}, {0, 1, 0, 0}, {0, 0, 0, 0}}; 
                break;                
            case 4: // S
                t = new int[][]{{0, 1, 0, 0}, {1, 1, 0, 0}, {1, 0, 0, 0}, {0, 0, 0, 0}}; 
                break;                
            case 5: // J
                t = new int[][]{{1, 0, 0, 0}, {1, 0, 0, 0}, {1, 1, 0, 0}, {0, 0, 0, 0}}; 
                break;                
            case 6: // L
                t = new int[][]{{0, 1, 0, 0}, {0, 1, 0, 0}, {1, 1, 0, 0}, {0, 0, 0, 0}}; 
                break;                
            default: // non-existent piece
                t = new int[][]{{1, 1, 1, 1}, {1, 1, 0, 0}, {0, 0, 0, 0}, {0, 0, 0, 0}}; 
                break; 
        }
                 
        
        int[][] t2 = new int[4][4];
        // we rotate t to the required position and put it to t2
        int x, y;
        switch (rot)
        {
            case 0:
                for (x=0; x<4; x++)
                    for (y=0; y<4; y++)
                        t2[x][y] = t[x][y];
                break;
            case 1:
                // 1110      0000     
                // 0100      1000       
                // 0000  ->  1100          
                // 0000      1000          
                for (x=0; x<4; x++)
                    for (y=0; y<4; y++)
                        t2[x][y] = t[y][3-x];
                break;
            case 2:
                for (x=0; x<4; x++)
                    for (y=0; y<4; y++)
                        t2[x][y] = t[3-x][3-y];
                break;
            case 3:
                for (x=0; x<4; x++)
                    for (y=0; y<4; y++)
                        t2[x][y] = t[3-y][x];
                break;                
        }
                    
        int emptyrow = 0;
        int emptycol = 0;
        
        // determine number of empty columns
        outerloop1:
        for (x=0; x<4; x++)
        {
            for (y=0; y<4; y++)   
            {
                if (t2[x][y] != 0)
                    break outerloop1;
            }
            emptycol++;
        }
        
        // determine number of empty columns
        outerloop2:
        for (y=0; y<4; y++)
        {
            for (x=0; x<4; x++)   
            {
                if (t2[x][y] != 0)
                    break outerloop2;
            }
            emptyrow++;
        }
        
        //we shift t2 so that the array does not begin with an empty row or column
        int[][] t3 = new int[4][4];
        
        for (x=emptycol; x<4; x++)
        {
            for (y=emptyrow; y<4; y++)
                t3[x-emptycol][y-emptyrow] = t2[x][y];
        }
        return t3;
    }


    /**
     * Erase complete lines from the board, if there are any
     * 
     * @param b the board
     * @return  the number of lines erased
     */
    int eraseLines(int b[][])
    {
        int nErased = 0;
        int debugy = 0;
        for(int y = height - 1; y >= 0; y--)
        {
            debugy++;
            boolean isfull = true;
            int x = 0;
            do
            {
                if(x >= width)
                    break;
                if(b[x + 3][y + 3] == 0)
                {
                    isfull = false;
                    break;
                }
                x++;
            } while(true);
            if(!isfull)
                continue;
            for(int y2 = y; y2 >= 0; y2--)
                for(x = 0; x < width; x++)
                    b[x + 3][y2 + 3] = b[x + 3][(y2 - 1) + 3];


            y++;
            nErased++;
        }

        return nErased;
    }

    /**
     * Assigns a heuristic value to the game state represented by "b"
     * 
     * @param b the board
     * @return  a value estimation of "b"
     */
    double getValue(int b[][])
    {
        int heights[] = new int[width];
        int maxh = 0;
        int nholes = 0;
        for(int i = 0; i < width; i++)
        {
            int j;
            for(j=0; j<height; j++)
            {
                if (b[i+PADDING][j+PADDING] != 0)
                    break;
            }
            heights[i] = height-j;
            for(; j < height; j++)
                if(b[i + PADDING][j + PADDING] == 0)
                    nholes++;

            if(heights[i] > maxh)
                maxh = heights[i];
        }
        // a very simple heuristic evaluation function... 
        // your algorithm should learn something better ;-)
        double value = -maxh-0.1*nholes;

        return value;
    }
	    public static void main(String[] args){
	        AgentLoader L=new AgentLoader(new SampleTetrisAgent());
	        L.run();
	    }
     
}
  