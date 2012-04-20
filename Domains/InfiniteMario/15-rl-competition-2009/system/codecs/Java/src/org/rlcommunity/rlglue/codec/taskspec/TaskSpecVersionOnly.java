/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package org.rlcommunity.rlglue.codec.taskspec;

import java.util.StringTokenizer;

/**
 *
 * @author Brian Tanner
 */
public class TaskSpecVersionOnly extends TaskSpecDelegate {

    private String version = "unknown";

    /**
     * Parse a task spec string.
     * @param taskSpecString
     */
    public TaskSpecVersionOnly(String taskSpecString) {
        String tmpToken;
        //Default token is space, works for me.
        StringTokenizer T = new StringTokenizer(taskSpecString);

        tmpToken = T.nextToken();
        if (!tmpToken.equals("VERSION")) {
            throw new IllegalArgumentException("Expected VERSION token.  This task spec doesn't look like a fourth generation  task spec.");
        }

        version = T.nextToken();
    }

    /**
     * @see org.rlcommunity.rlglue.codec.taskspec.TaskSpec#getVersionString()
     */
    @Override
    public String getVersionString() {
        return version;
    }
}
