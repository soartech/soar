/*
 * Copyright (C) 2007, Brian Tanner
 *
http://rl-glue-ext.googlecode.com/

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
 *
 *  $Revision: 633 $
 *  $Date: 2009-02-07 11:56:01 -0500 (Sat, 07 Feb 2009) $
 *  $Author: brian@tannerpages.com $
 *  $HeadURL: http://rl-glue-ext.googlecode.com/svn/trunk/projects/codecs/Java/src/org/rlcommunity/rlglue/codec/network/Network.java $
 *
 */
package org.rlcommunity.rlglue.codec.network;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

import java.util.logging.Level;
import java.util.logging.Logger;
import org.rlcommunity.rlglue.codec.types.Action;
import org.rlcommunity.rlglue.codec.types.Observation;
import org.rlcommunity.rlglue.codec.types.RL_abstract_type;
import org.rlcommunity.rlglue.codec.types.Reward_observation_terminal;

/**
 * This class does the heavy lifting of sendig and receiving data over the
 * network. It is used by both the Java and Matlab codecs.
 *
 * The Socket has been changed (Feb 7 2009) to be offer a NON-BLOCKING option.
 *
 * The Java codec is JUST FINE using BLOCKING.  NON-BLOCKING is for MATLAB.  You may
 * need NON-BLOCKING if you wanted to run an environment and/or agent and/or
 * experiment all from the same Thread in Java.  But why would you do that?
 * You would have them in different Threads or preferably different processes, so
 * none of this matters.
 * @author btanner
 */
public class Network {

    public static final int kExperimentConnection = 1;
    public static final int kAgentConnection = 2;
    public static final int kEnvironmentConnection = 3;
    public static final int kAgentInit = 4;
    public static final int kAgentStart = 5;
    public static final int kAgentStep = 6;
    public static final int kAgentEnd = 7;
    public static final int kAgentCleanup = 8;
    public static final int kAgentMessage = 10;
    public static final int kEnvInit = 11;
    public static final int kEnvStart = 12;
    public static final int kEnvStep = 13;
    public static final int kEnvCleanup = 14;
    public static final int kEnvMessage = 19;
    public static final int kRLInit = 20;
    public static final int kRLStart = 21;
    public static final int kRLStep = 22;
    public static final int kRLCleanup = 23;
    public static final int kRLReturn = 24;
    public static final int kRLNumSteps = 25;
    public static final int kRLNumEpisodes = 26;
    public static final int kRLEpisode = 27;
    public static final int kRLAgentMessage = 33;
    public static final int kRLEnvMessage = 34;
    public static final int kRLTerm = 35;
    public static final int kRLEnvStart = 36;
    public static final int kRLEnvStep = 37;
    public static final int kRLAgentStart = 38;
    public static final int kRLAgentStep = 39;
    public static final int kRLAgentEnd = 40;
    public static final String kDefaultHost = "127.0.0.1";
    public static final int kDefaultPort = 4096;
    public static final int kRetryTimeout = 2;
    protected static final int kByteBufferDefaultSize = 4096;
    public static final int kIntSize = 4;
    protected static final int kDoubleSize = 8;
    protected static final int kCharSize = 1;
    protected SocketChannel socketChannel = null;
    private InetSocketAddress theConnectAddress = null;
    private boolean blocking = true;
    private ByteBuffer recvBuffer;
    private ByteBuffer sendBuffer;
    private boolean debug = false;

    public Network() {
        recvBuffer = ByteBuffer.allocateDirect(kByteBufferDefaultSize);
        sendBuffer = ByteBuffer.allocateDirect(kByteBufferDefaultSize);
    }

    public void connect(String host, int port, int retryTimeout) {
        connect(host, port, retryTimeout, true);
    }

    /**
     * Support for NON-BLOCKING added. If you are using NON-BLOCKING, then be
     * sure to call ensureConnected() afterward until it returns true.
     * @param host
     * @param port
     * @param retryTimeout
     * @param blocking
     */
    public boolean connect(String host, int port, int retryTimeout, boolean blocking) {
        this.blocking = blocking;
        boolean didComplete = false;
        boolean actuallyDidConnect = false;

        while (!didComplete) {
            try {
                theConnectAddress = new InetSocketAddress(host, port);
                actuallyDidConnect = connect(theConnectAddress, blocking);
                didComplete = true;
            } catch (IOException ioException) {
                try {
                    Thread.sleep(retryTimeout);
                } catch (InterruptedException ieException) {
                }
            }
        }
        return actuallyDidConnect;
    }

    /**
     * This is a private convenience method that will try to connect.
     * It's meant to reduce duplicated code between methods.
     * @return
     * @throws java.io.IOException
     */
    private boolean connect(InetSocketAddress theAddress, boolean shouldBlock) throws IOException {
        boolean actuallyDidConnect = false;
        socketChannel = SocketChannel.open();
        socketChannel.configureBlocking(shouldBlock);
        actuallyDidConnect = socketChannel.connect(theAddress);
        return actuallyDidConnect;
    }

    /**
     * This can be used in NON-BLOCKING mode to be sure that the connection was made.
     * Probably call this in a loop.  In BLOCKING mode, you don't need to call this.
     * @since 2.02
     * @return
     * @throws java.io.IOException
     */
    public boolean ensureConnected() {
        assert (socketChannel != null);

        boolean canFinishConnectionOrConnected = false;

        try {
            canFinishConnectionOrConnected = socketChannel.finishConnect();
        } catch (Exception ex) {
            try {
                //This can happen if RL-Glue isn't running yet.  We should try to connect
                //again, return false, and hope that this works later.
                canFinishConnectionOrConnected = connect(theConnectAddress, this.blocking);
            } catch (IOException ex1) {
            }

        }
        return canFinishConnectionOrConnected;
    }

    public void close() throws IOException {
        socketChannel.close();
    }

    public int send() throws IOException {
        return socketChannel.write(sendBuffer);
    }

    /**
     *
     *
     * This method has been updated. It will work as it used to, before we added
     * NON-BLOCKING mode.  It will either return after reading size (or more) bytes
     *  or it will throw an exception.
     * @param size
     * @return
     * @throws java.io.IOException
     */
    public int recv(int size) throws IOException {
        if(size==0)return 0;
        int amountReceived = 0;

        amountReceived = recvNonBlock(size);
        while (amountReceived == 0) {
            Thread.yield();
            amountReceived = recvNonBlock(size);
        }
        return amountReceived;
    }

    /**
     *
     *
     * This method has been added.  If the socket is in non-blockig mode, the
     * It will return 0 if there is no data available.
     *
     * If you use the NON-BLOCKING option, it is possible that if there is no data available
     * (for example, other components have not yet connected)
     * then recv will return 0.  If recv gets ANYTHING when it first tries,
     * then it WILL continue and poll until it gets all the data it was asked for.
     *
     * It also will not throw a RLGlueDisconnectException if it socketChannel.read returns
     * -1 before size bytes have been read.  This is indicative that the other end of the
     * socket is closed (IE, RL-Glue was quit or crashed).
     * @param size
     * @return
     * @throws java.io.IOException
     * @since 2.02
     */
    public int recvNonBlock(int size) throws IOException {
        this.ensureRecvCapacityRemains(size);

        int recvTotal = 0;
        while (recvTotal < size) {
            int recvSize = 0;
            recvSize = socketChannel.read(recvBuffer);
            if (recvSize == -1) {
                close();
                throw new RLGlueDisconnectException("ERROR: Java Codec was expecting read: " + size + " bytes but only received: " + recvTotal + ".\n\tRL-Glue probably closed the connection.");
            } else {
                recvTotal += recvSize;
            }

            if (recvTotal == 0) {
                //This MUST be the first iteration of the loop because we return
                //if we receive 0 bytes on the first read.
                return 0;
            }
        }
        return recvTotal;
    }

    public boolean isConnected() {
        return socketChannel.isConnected();
    }

    public void clearSendBuffer() {
        sendBuffer.clear();
    }

    public void clearRecvBuffer() {
        recvBuffer.clear();
    }

    public void flipSendBuffer() {
        sendBuffer.flip();
    }

    public void flipRecvBuffer() {
        recvBuffer.flip();
    }

    public int[] getInts(int howMany) {
        int currentPosition = recvBuffer.position();
        int[] returnArray = new int[howMany];
        recvBuffer.asIntBuffer().get(returnArray);
        recvBuffer.position(currentPosition + howMany * 4);
        return returnArray;
    }

    public double[] getDoubles(int howMany) {
        int currentPosition = recvBuffer.position();
        double[] returnArray = new double[howMany];
        recvBuffer.asDoubleBuffer().get(returnArray);
        recvBuffer.position(currentPosition + howMany * 8);
        return returnArray;
    }

    public int getInt() {
        return recvBuffer.getInt();
    }

    public int getInt(int index) {
        return recvBuffer.getInt(index);
    }

    public double getDouble() {
        return recvBuffer.getDouble();
    }

    /**
     * @since 2.0
     * Pull a char off the buffer.  Slightly tricky because chars are unicode
     * (more than a byte) in Java, but only 1 byte in our network protocol.
     * <p>Thanks: http://bytes.com/forum/thread17160.html
     * @return ascii character encoded by the next byte.
     */
    public char getChar() {
        byte b = recvBuffer.get();
        return (char) (b & 0xFF);
    }

    /**
     * Used for getting task spec and env/agent messages. UNLIKE the
     * charArrays in observations/actions/etc, these strings are null
     * terminated?
     * @return Return a String from the network buffer
     */
    public String getString() {
        String returnString = "";
        int recvLength = this.getInt();
        byte[] recvByteBuffer = new byte[recvLength];
        recvBuffer.get(recvByteBuffer);
        try {

            returnString = new String(recvByteBuffer, "UTF-8");
        } catch (UnsupportedEncodingException ex) {
            System.err.println("Exception reading String from buffer: " + ex);
            ex.printStackTrace();
        }
        return returnString;

    }

    public Observation getObservation() {
        Observation returnVal = new Observation();
        fillAbstractType(returnVal);
        return returnVal;
    }

    public Action getAction() {
        Action returnVal = new Action();
        fillAbstractType(returnVal);
        return returnVal;
    }

    /*
     *
     * Hmm, this method might actually make it quite expensive to make abstract types because
     * we need to read them once, then make a copy immediately when we change them
     * into specialized supertypes...
     * @deprecated
     */
    private final RL_abstract_type getAbstractType() {
        final int numInts = this.getInt();
        final int numDoubles = this.getInt();
        final int numChars = this.getInt();

        RL_abstract_type key = new RL_abstract_type(numInts, numDoubles, numChars);

        key.intArray = getInts(numInts);
        key.doubleArray = getDoubles(numDoubles);

        for (int i = 0; i < numChars; ++i) {
            key.charArray[i] = this.getChar();
        }

        return key;
    }

    private final void fillAbstractType(RL_abstract_type toFill) {
        final int numInts = getInt();
        final int numDoubles = getInt();
        final int numChars = getInt();

        toFill.intArray = getInts(numInts);
        toFill.doubleArray = getDoubles(numDoubles);

        toFill.charArray = new char[numChars];
        for (int i = 0; i < numChars; ++i) {
            toFill.charArray[i] = this.getChar();
        }
    }

    /**
     * Experimental
     * @param values
     */
    public void putInts(int[] values) {
        if (values == null) {
            return;
        }
        this.ensureSendCapacityRemains(Network.kIntSize * values.length);
        int currentPosition = sendBuffer.position();
        sendBuffer.asIntBuffer().put(values);
        //Using the intBuffer view doesn't increment the position in the underlying
        //buffer so we have to push it along
        sendBuffer.position(currentPosition + values.length * 4);

    }

    /**
     * Experimental
     * @param values
     */
    public void putDoubles(double[] values) {
        if (values == null) {
            return;
        }
        this.ensureSendCapacityRemains(Network.kDoubleSize * values.length);
        int currentPosition = sendBuffer.position();
        sendBuffer.asDoubleBuffer().put(values);
        sendBuffer.position(currentPosition + values.length * 8);

    }

    public void putInt(int value) {
        this.ensureSendCapacityRemains(Network.kIntSize);
        this.sendBuffer.putInt(value);
    }

    public void putDouble(double value) {
        this.ensureSendCapacityRemains(Network.kDoubleSize);
        this.sendBuffer.putDouble(value);
    }

    /**
     * Brian Tanner adding this for RL-Glue 3.x compatibility
     * Converts unicode (> 1 byte) char to 1 byte network char protocol
     * <p>Thanks http://bytes.com/forum/thread17160.html
     * @since 2.0
     * @param c character to put into the buffer as byte
     */
    public void putChar(char c) {
        this.ensureSendCapacityRemains(Network.kCharSize);
        this.sendBuffer.put((byte) (c & 0xFF));
    }

    public void putString(String message) throws UnsupportedEncodingException {
        // We don't want to have to deal null...
        if (message == null) {
            message = "";
        }
        this.ensureSendCapacityRemains(Network.kIntSize + message.length());
        this.putInt(message.length());

        if (message.length() > 0) {
            sendBuffer.put(message.getBytes("UTF-8"));
        }
    }

    public final void putAbstractType(RL_abstract_type theObject) {
        this.ensureSendCapacityRemains(Network.sizeOf(theObject));

        int numInts = 0;
        int numDoubles = 0;
        int numChars = 0;

        if (theObject != null) {
            if (theObject.intArray != null) {
                numInts = theObject.intArray.length;
            }
            if (theObject.doubleArray != null) {
                numDoubles = theObject.doubleArray.length;
            }
            if (theObject.charArray != null) {
                numChars = theObject.charArray.length;
            }
        }


        this.putInt(numInts);
        this.putInt(numDoubles);
        this.putInt(numChars);
        putInts(theObject.intArray);
        putDoubles(theObject.doubleArray);

        //Not implementing a putChars because each char is currently
        //converted manually from a byte
        for (int i = 0; i < numChars; ++i) {
            this.putChar(theObject.charArray[i]);
        }
    }

    public void putObservation(Observation obs) {
        putAbstractType(obs);
    }

    public void putAction(Action action) {
        putAbstractType(action);
    }

    public void putRewardObservation(Reward_observation_terminal rewardObservation) {
        this.ensureSendCapacityRemains(Network.sizeOf(rewardObservation));

        this.putInt(rewardObservation.terminal);
        this.putDouble(rewardObservation.r);
        this.putObservation(rewardObservation.o);
    }

    protected void ensureSendCapacityRemains(int capacity) {
        if (sendBuffer.capacity() - sendBuffer.position() < capacity) {
            sendBuffer = Network.cloneWithCapacity(sendBuffer, sendBuffer.capacity() + capacity);
        }
    }

    protected void ensureRecvCapacityRemains(int capacity) {
        if (recvBuffer.capacity() - recvBuffer.position() < capacity) {
            recvBuffer = Network.cloneWithCapacity(recvBuffer, recvBuffer.capacity() + capacity);
        }
    }

    protected static ByteBuffer cloneWithCapacity(ByteBuffer original, int capacity) {
        ByteBuffer clone = ByteBuffer.allocateDirect(capacity);
        original.flip();
        clone.put(original);
        clone.position(original.position());
        return clone;
    }

    public static int sizeOf(int value) {
        return Network.kIntSize;
    }

    public static int sizeOf(double value) {
        return Network.kDoubleSize;
    }

    public static int sizeOf(String string) {
        int size = Network.kIntSize;
        if (string != null) {
            size += string.length();
        }
        return size;
    }

    public static int sizeOf(RL_abstract_type theObject) {
        int size = Network.kIntSize * 3;
        int intSize = 0;
        int doubleSize = 0;
        int charSize = 0;

        if (theObject != null) {
            if (theObject.intArray != null) {
                intSize = Network.kIntSize * theObject.intArray.length;
            }
            if (theObject.doubleArray != null) {
                doubleSize = Network.kDoubleSize * theObject.doubleArray.length;
            }
            if (theObject.charArray != null) {
                charSize = Network.kCharSize * theObject.charArray.length;
            }
        }
        return size + intSize + doubleSize + charSize;
    }

    public static int sizeOf(Reward_observation_terminal rewardObservation) {
        return Network.kIntSize + // terminal
                Network.kDoubleSize + // reward
                Network.sizeOf(rewardObservation.o);
    }
}

