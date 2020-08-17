package edu.stanford.dnl;

import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.event.KeyEvent;

public class MessageComponent extends ExperimentComponent {
	private Font f = null;
	private FontMetrics fm;
	private String msg;
	
	/**
	 * Displays message and waits for 'Enter' to complete.
	 * 
	 * @param es	parent ExperimentSequence
	 * @param msg	message to display
	 * @param fsize	font size (font is Arial)
	 * @author Samuel McClure
	 */
	public MessageComponent(ExperimentSequence es, String msg, int fsize) {
		super(es);
		
		this.msg = msg;
		
		f = new Font("Arial", Font.PLAIN, fsize);
		fm = getFontMetrics(f);
		setBackground(Color.WHITE);
	}
	
	public void start() {
		super.start();
	}
	
	public void keyPressed(KeyEvent e) {
		if (e.getKeyCode() == KeyEvent.VK_ENTER) super.complete();
	}

	public void paintComponent(Graphics g) {
		super.paintComponent(g);

		g.setFont(f);
		g.setColor(Color.BLACK);
		g.drawString(
				msg,
				getWidth()/2 - fm.stringWidth(msg)/2,
				getHeight()/2 - fm.getAscent()/2);
	}
}
