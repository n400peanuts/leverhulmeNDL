import edu.stanford.dnl.*;

import javax.swing.*;
import java.awt.*;
import java.awt.event.KeyEvent;
import java.io.*;

public class SXXSExperiment extends Experiment {

	private static String SUBJ_ID;
	public static DataOutputStream OUTPUT = null;
	
	private ExperimentSequence seq;
	
	
	public SXXSExperiment() {
		final String[] exptChoices = new String[]{"SX-SX", "SX-XS", "XS-SX"};
		int exptType = JOptionPane.showOptionDialog(null, "Select experiment to run:",
				"Experiment Selector", JOptionPane.YES_NO_CANCEL_OPTION, JOptionPane.QUESTION_MESSAGE,
				null, exptChoices, null);
		if (exptType == JOptionPane.CLOSED_OPTION) System.exit(0);

		ExperimentFrame ef = new ExperimentFrame(this);
		ef.getContentPane().setLayout(new BorderLayout());
		
		seq = new ExperimentSequence(this, ef.getExperimentWindow().getContentPane());
		
		if (exptChoices[exptType].equals(exptChoices[0])) { //SX-SX
			System.out.println(System.currentTimeMillis()+"\tSX-SX");
			seq.addComponent(new SXTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new SXTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new TestingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new SXTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new SXTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new TestingComponent(seq));
		} else if (exptChoices[exptType].equals(exptChoices[1])) { //SX-XS
			System.out.println(System.currentTimeMillis()+"\tSX-XS");
			seq.addComponent(new SXTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new SXTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new TestingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new XSTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new XSTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new TestingComponent(seq));
		} else if (exptChoices[exptType].equals(exptChoices[2])) { //XS-SX
			System.out.println(System.currentTimeMillis()+"\tXS-SX");
			seq.addComponent(new XSTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new XSTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new TestingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new SXTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new SXTrainingComponent(seq));
			seq.addComponent(new MessageComponent(seq, "Please Wait", 48));
			seq.addComponent(new TestingComponent(seq));
		}
	}
	
	
	public void run(String id) {
		setSubjID(id);
		seq.runCurrentComponent();
	}
	

	public void finish() {
		if (SXXSExperiment.OUTPUT == null) return;
		
		try {
			SXXSExperiment.OUTPUT.flush();
			SXXSExperiment.OUTPUT.close();
		} catch (Exception exc) {
			exc.printStackTrace();
		}
	}
	
	protected static boolean setSubjID(String id) {
		System.out.println("Subject ID: " + id);
		if (id.length() > 0)
			SXXSExperiment.SUBJ_ID = id;
		else
			SXXSExperiment.SUBJ_ID = "null";
		
		if (id.toLowerCase().equals("debug")) return true;
		
		try {
			String rootDir = ClassLoader.getSystemResource("data").getFile();
			if (rootDir.indexOf(".jar") >= 0)
				rootDir = System.getProperty("user.dir") + File.separator + "data";
			new File(rootDir).mkdirs();
			SXXSExperiment.OUTPUT = new DataOutputStream(
				new FileOutputStream(
					rootDir + File.separator + 
					SXXSExperiment.SUBJ_ID+".out"
				)
			);
			System.setOut(new PrintStream(SXXSExperiment.OUTPUT));
		} catch (Exception exc) {
			exc.printStackTrace();
			return false;
		}
		
		return true;
	}

	public static void main(String[] args) {
		try {
			UIManager.setLookAndFeel(
				UIManager.getSystemLookAndFeelClassName()
			);
		} catch (Exception exc) {
			exc.printStackTrace();
		}
		
		new SXXSExperiment();
	}

}
