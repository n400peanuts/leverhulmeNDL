package edu.stanford.dnl;

import javax.swing.*;
import java.awt.event.*;

public abstract class ExperimentComponent extends JPanel implements KeyListener {
	private ExperimentSequence es;
	
	public ExperimentComponent(ExperimentSequence es) {
		this.es = es;
		setFocusable(true);
	}
	
	public void start() {
		addKeyListener(this);
		requestFocus();
	}
	
	public void complete() {
		es.runNextComponent();
	}
	
	public ExperimentSequence getExperimentSequence() {
		return es;
	}
	
	public void keyPressed(KeyEvent e) { ; }
	public void keyReleased(KeyEvent e) { ; }
	public void keyTyped(KeyEvent e) { ; }
}
