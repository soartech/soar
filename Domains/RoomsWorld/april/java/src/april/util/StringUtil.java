package april.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class StringUtil
{

    /**
     *  Method to replace occurrences of named environment variables
     *  in a string with their true values. Note that this method only
     *  accepts env. variables which are followed by non word
     *  character:

     * e.g. "$ENV_variable/" would replace the everything but the
     * slash with the value of the environment variable "ENV_variable"
     *
     */
    public static String replaceEnvironmentVariables(String value)
    {
        Pattern pattern = Pattern.compile("\\$\\w+\\W??$??");
        Matcher matcher = pattern.matcher(value);

        String resolved = value;

        while (matcher.find()) {
            String match = matcher.group();
            int start = matcher.start();
            int len = match.length();

            String variableName = match.split("\\W")[1];
            String variableValue = System.getenv(variableName);

            if (variableValue != null) {
                resolved = resolved.substring(0, start) + variableValue +
                    resolved.substring(start+len,resolved.length());
            } else {
                System.out.println("WRN: Ignoring unknown environment variable: >"+variableName+"<");
                break;
            }

            // Iteratively keep matching on the resolved string
            matcher = pattern.matcher(resolved);
        }
        return resolved;
    }

    public static void main(String args[])
    {
        if (args.length == 1)
            unittest(Integer.parseInt(args[0]));
        else
            unittest(-1);
    }

    private static void unittest( int specific)
    {
        // Test cases: Be sure to export USER_HOME before starting
        // unit test

        String user = "$USER";
        String user_home = "$USER_home";

        String true_user = System.getenv(user.split("\\W")[1]);
        if (true_user  == null)
            true_user = user;

        String true_user_home = System.getenv(user_home.split("\\W")[1]);
        if (true_user_home  == null)
            true_user_home = user_home;

        String examples[] = {user,user+" ", " "+user,
                             "$ USER", "$ USER/"+user, user+"/"+user,
                             "b"+user+" ", "b"+user, user+"b", user+"/b",
                             "b"+user_home, user_home+"b", user_home+"/b",
                             user_home+"/",user + " "+user_home};

        String answers[] = {true_user,true_user+" ", " "+true_user,
                             "$ USER", "$ USER/"+true_user, true_user+"/"+true_user,
                             "b"+true_user+" ", "b"+true_user, user+"b", true_user+"/b",
                             "b"+true_user_home, user_home+"b", true_user_home+"/b",
                            true_user_home+"/",true_user+" "+true_user_home};


        int start = 0;
        int end = examples.length;
        if (specific != -1) {
            start = specific;
            end = start + 1;
        }

        boolean allGood = true;
        for (int i = start; i < end; i++) {
            String src = examples[i];
            String ans = answers[i];
            String result = StringUtil.replaceEnvironmentVariables(examples[i]);

            System.out.print("Example "+i);
            if (ans.equals(result))
                System.out.print(" SUCCESS ");
            else{
                System.out.print(" FAILURE ");
                allGood = false;
            }
            System.out.print(" -- Source: >"+src+"< ");
            System.out.print("Result: >"+result+"< ");
            System.out.println("Answer: >"+ans+"<");
        }

        if (allGood)
            System.out.println("ALL SYSTEMS GO!");
        else
            System.out.println("HOUSTON, WE'VE GOT A PROBLEM!");

    }
}
