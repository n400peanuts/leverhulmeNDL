package edu.stanford.dnl;

import java.awt.*;
import java.awt.event.*;
import javax.swing.*;


public class HTMLTextComponent extends ExperimentComponent {
	private int key;
	private ExperimentSequence seq;
	
	public HTMLTextComponent(ExperimentSequence es, String htmlText, int key) {
		super(es);
		this.key = key;
		this.seq = es;
		
		JLabel lbl = new JLabel(htmlText);
		lbl.setBackground(Color.RED);
		
		setLayout(new GridBagLayout());
		GridBagConstraints gbc = new GridBagConstraints();
		gbc.weightx = 1.0;
		gbc.weighty = 1.0;
		add(lbl, gbc);
	}
	
	public void keyReleased(KeyEvent e) {
		if ( (e.getKeyCode()&key) != 0) seq.runNextComponent();
	}
	
	public void start() {
		super.start();
	}

}
