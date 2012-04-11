package edu.umich.soarrobot.SoarRobotTablet;

import java.util.ArrayList;

import android.app.Activity;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import edu.umich.soarrobot.SoarRobotTablet.network.RobotSession;
import edu.umich.soarrobot.SoarRobotTablet.network.RobotSession.TextMessageListener;

public class ChatActivity extends Activity implements TextMessageListener {

	private TextView tv;
	private SoarRobotTablet root;
	private EditText et;
	
	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle savedInstanceState) {
	    super.onCreate(savedInstanceState);
	    try {
		    setContentView(R.layout.chat);	    	
	    } catch (Exception e) {
	    	System.out.println(e);
	    }
	    tv = (TextView)findViewById(R.id.chatLogText);
	    root = SoarRobotTablet.getInstance();
	    et = (EditText) findViewById(R.id.chatCommandsEditText);
	    RobotSession session = root.getRobotSession();
	    session.addTextMessageListener(this);
	    ArrayList<String> messageHistory = root.getTextMessageHistory();
	    for (String message : messageHistory)
	    {
	    	tv.append(message + "\n");
	    }
        ((Button) findViewById(R.id.chatCommandsButton)).setOnClickListener(new OnClickListener() {
			public void onClick(View v) {
				final String message = ChatActivity.this.et.getText().toString();
				if (ChatActivity.this.root.sendTextMessage(message)) {
					ChatActivity.this.et.setText("");
				}
			}
		});
    }

	@Override
	public void textMessageReceived(String message) {
		tv.append(message + "\n");
		root.textMessageReceived(message);
	}

}
