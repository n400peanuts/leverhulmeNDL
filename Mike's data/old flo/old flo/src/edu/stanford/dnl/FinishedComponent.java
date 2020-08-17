package edu.stanford.dnl;

import java.awt.*;
import java.awt.event.KeyEvent;

/**
 * Displays 'Experiment Complete' and wait for 'Q' to be pressed
 * and then exits the program.
 * 
 * @author Samuel McClure
 *
 */
public class FinishedComponent extends MessageComponent {
	
	public FinishedComponent(ExperimentSequence es) {
		super(es, "Experiment Complete", 72);
	}
	
	public void keyPressed(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_Q) System.exit(0);
	}

}
