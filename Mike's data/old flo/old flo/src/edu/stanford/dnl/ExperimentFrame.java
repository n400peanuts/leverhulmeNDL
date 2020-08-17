package edu.stanford.dnl;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;

public class ExperimentFrame extends JFrame {
	private JTextField subjIDField;
	private static final long serialVersionUID = 1L;
	
	private JWindow win;
	private Experiment parentExpt;

	public ExperimentFrame(Experiment expt) {
		super("Register Subject");
		super.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		parentExpt = expt;
				
		subjIDField = new JTextField(30);
		
		Action begin = new AbstractAction("Begin") {
			private static final long serialVersionUID = 1L;

			public void actionPerformed(ActionEvent e) {
				win.setVisible(true);
				parentExpt.run(
						subjIDField.getText().trim()
				);
			}
		};
		
		getContentPane().setLayout(new BorderLayout(5,5));
		getContentPane().add(subjIDField, BorderLayout.CENTER);
		getContentPane().add(new JButton(begin), BorderLayout.SOUTH);
		pack();
		
		Dimension s = getSize();
		Dimension ss = Toolkit.getDefaultToolkit().getScreenSize();
		setLocation(ss.width/2-s.width/2, ss.height/2-s.height/2);
		
		setVisible(true);
		
		win = new JWindow(this);
		win.setSize(Toolkit.getDefaultToolkit().getScreenSize());
		//win.setSize(1024,768);
		win.setLocation(0,0);
	}
	
	public JWindow getExperimentWindow() {
		return win;
	}
}
