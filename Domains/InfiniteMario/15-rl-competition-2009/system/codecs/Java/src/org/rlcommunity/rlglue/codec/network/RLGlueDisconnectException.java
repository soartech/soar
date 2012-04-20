/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.rlcommunity.rlglue.codec.network;

import java.io.IOException;

/**
 *
 * @author Brian Tanner
 */
public class RLGlueDisconnectException extends IOException {
    private static final long serialVersionUID = 1L;

    public RLGlueDisconnectException(String message){
        super(message);
    }
}
