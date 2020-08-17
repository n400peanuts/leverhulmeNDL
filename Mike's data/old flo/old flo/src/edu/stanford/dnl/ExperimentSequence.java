package edu.stanford.dnl;

import java.util.*;
import java.awt.*;

public class ExperimentSequence {
	private Vector<ExperimentComponent> sequence = new Vector<ExperimentComponent>();
	private int index;
	private Container parent;
	private Experiment expt;
	
	public ExperimentSequence(Experiment e, Container c) {
		index = 0;
		parent = c;
		expt = e;
	}
	
	public void addComponent(ExperimentComponent ec) {
		sequence.add(ec);
		
		if (sequence.size() == 1) {
			parent.add(ec, BorderLayout.CENTER);
		}
	}
	
	public void removeComponent(ExperimentComponent ec) {
		sequence.remove(ec);
	}
	
	public void gotoComponent(ExperimentComponent ec) {
		index = sequence.indexOf(ec);
	}
	
	public void runCurrentComponent() {
		parent.validate();
		parent.doLayout();
		parent.repaint();
		sequence.elementAt(index).start();
	}
	
	public void runNextComponent() {
		parent.remove(sequence.elementAt(index));
		index++;
		if (index < sequence.size())
			parent.add(sequence.elementAt(index), BorderLayout.CENTER);
		else {
			expt.finish();
			FinishedComponent fc = new FinishedComponent(this);
			addComponent(fc);
			parent.add(fc, BorderLayout.CENTER);
		}
		runCurrentComponent();
	}
}
