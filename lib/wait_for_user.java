import ij.*;
import ij.plugin.frame.PlugInFrame;
import ij.gui.*;
//import ij.util.Tools;
import java.awt.*;
import java.awt.event.*;
//import java.util.*;

/**
*  ImageJ Plugin to ask a user to do some action during a macro
*  (e.g. select a roi).
*  Usage:
*  - Copy into ImageJ plugins folder or a subfolder thereof
*  - Compile with "Compile and run" and press "OK". Disregard any deprecation warning.
*  - Restart ImageJ
*  - Now the plugin is loaded into ImageJ and can be used in a macro, e.g.:
         run("Blobs (25K)");
         myImageID = getImageID();
         setTool(0); //Rectangle tool
         beep();
         run("Wait For User", "Select Rectangle to Invert");
         selectImage(myImageID); //make sure we have the same foreground image again
         if (selectionType() != 0) exit("Sorry, no rectangle");
         run("Invert");
*
*  Version 22-Nov-2007 Michael Schmid
*/
public class Wait_For_User extends PlugInFrame 
implements KeyListener, ActionListener {

    /** The constructor, preparing the window (panel) */
    public Wait_For_User() {
        super("User Action Required:");
        WindowManager.addWindow(this);
        String text = Macro.getOptions();
        Label textLabel = new Label(text);  //set up all the panel items
		Button okay = new Button("  OK  ");
        okay.addActionListener(this);
        okay.addKeyListener(this);
        GridBagLayout gridbag = new GridBagLayout();//set up the layout
        GridBagConstraints c = new GridBagConstraints();
        setLayout(gridbag);
        c.insets = new Insets(4,4,2,4);     //(top, left, bottom, right)
        c.gridx = 0; c.gridy = 0; c.anchor = GridBagConstraints.WEST;
        add(textLabel,c);                   //add panel items to this panel
        c.gridx = 0; c.gridy = 1;
        add(new Label("Thereafter, press OK                         "), c);
        c.gridx = 0; c.gridy = 2; c.anchor = GridBagConstraints.EAST;
        add(okay, c);
        this.pack();
        GUI.center(this);
        setResizable(false);
        show();
        synchronized(this) {                //wait for OK
            try {wait();}
            catch(InterruptedException e) {return;}
        }
    }
    
    public void close() {
        synchronized(this) { notify(); }
        super.close();
    }

	public void actionPerformed(ActionEvent e) {
		if (e.getSource() instanceof Button) close();
    }
	public void keyPressed(KeyEvent e) { 
		int keyCode = e.getKeyCode(); 
		if (keyCode==KeyEvent.VK_ENTER) close();
		else IJ.beep();
	}

    public void keyTyped(KeyEvent e) {}
    // listener for both the input fields of this panel and for the corresponding ImagePlus
    public void keyReleased(KeyEvent e) {}

}