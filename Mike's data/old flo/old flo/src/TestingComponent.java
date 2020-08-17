import edu.stanford.dnl.*;

import java.awt.*;
import java.awt.event.*;
import java.util.*;

public class TestingComponent extends ExperimentComponent {

	// this stuff comes straight from Flash
	private String[] nameindex = new String[] {"bim","dep","tob","wug"};
	private int[] correctanswers = new int[] {2,3,4,2,4,2,1,4,3,4,1,3,2,
			1,3,2,3,4,2,3,2,4,2,4,4,1,3,3,3,4,3,1,4,2,3,1,3,4,2,
			2,3,4,2,3,1,4,2,3,4,2,1,4,2,4,2,3};
	private String[] hilo = new String[] {"h","h","h","h","l","h","h","h",
			"l","h","h","h","h","h","l","l","h","h","h","l","l",
			"l","l","l","l","h","h","l","l","l","l","h","h","h",
			"h","h","h","h","l","l","l","l","l","h","h","l","h",
			"l","h","l","h","h","h","l","l","h"};
	
	// new Java variables
	private String [] names = {"wug", "dep", "bim", "tob"};
	private ImageBuffer imgBuffer;
	private boolean running = false;
	private int trialIndex = 0;
	private int response = -1;
	
	private Thread responseCountdown;
	private boolean responseMade = false;
	private int countdown = -1;
	
	final int STATE_BLANK = 0;
	final int STATE_CHOICE = 1;
	final int STATE_RESPONSE = 2;
	int state = STATE_BLANK;
	
	
	public TestingComponent(ExperimentSequence es) {
		super(es);
		setBackground(Color.WHITE);
		imgBuffer = new ImageBuffer();
	}
	
	public void start() {
		super.start();
	}
	
	private void startMeUp() {
		new Thread() {
			public void run() {
				System.out.println(System.currentTimeMillis()+"\tBEGIN RUN");
				try {
					sleep(5000);
					System.out.println(System.currentTimeMillis()+"\tBEGIN TEST");
				} catch (Exception exc) {
					exc.printStackTrace();
				}
				
				dumpTrialInfo();
				state = STATE_CHOICE;
				startResponseCountdown();
				repaint();
			}
		}.start();
	}
	
	public void keyPressed(KeyEvent e) {
		switch (e.getKeyCode()) {
		case KeyEvent.VK_1:
			responseMade(0);
			break;
		case KeyEvent.VK_2:
			responseMade(1);
			break;
		case KeyEvent.VK_3:
			responseMade(2);
			break;
		case KeyEvent.VK_4:
			responseMade(3);
			break;
		case KeyEvent.VK_5:
			if (!running) {
				startMeUp();
			}
			System.out.println(System.currentTimeMillis() + "\t" + "SCAN");
			break;
		case KeyEvent.VK_N:
			System.out.println(System.currentTimeMillis() + "\tTESTING SKIPPED");
			super.complete();
		}
	}
	
	private void responseMade(int r) {
		if (state != STATE_CHOICE) return;
		
		countdown = -1;
		trialIndex++;
		response = r;
		System.out.println(System.currentTimeMillis() + "\tRESP\t" + (response+1));
		
		state = STATE_RESPONSE;
		repaint();
		
		new Thread() {
			public void run() {
				try {
					sleep(250);
				} catch (Exception exc) {
					exc.printStackTrace();
				}
				imgBuffer.loadNextImage();
				
				if (imgBuffer.getCurrentImage() != null) {
					dumpTrialInfo();
					state = STATE_CHOICE;
					startResponseCountdown();
					repaint();
				} else {
					System.out.println(System.currentTimeMillis()+"\tEND TEST");
					state = STATE_BLANK;
					repaint();
					
					try {
						sleep(5000);
						System.out.println(System.currentTimeMillis()+"\tEND RUN");
					} catch (Exception exc) {
						exc.printStackTrace();
					}
					
					TestingComponent.this.complete();
				}
			}
		}.start();
	}
	
	private void dumpTrialInfo() {
		System.out.println(System.currentTimeMillis()+"\tTRIAL\t"+trialIndex);
		System.out.println(System.currentTimeMillis()+"\tHILO\t"+hilo[trialIndex]);
		System.out.println(System.currentTimeMillis()+"\tCORRECT\t"+correctanswers[trialIndex]);	
	}
	
	private void startResponseCountdown() {
		new Thread() {
			public void run() {
				int myTrialIndex = trialIndex;
				
				for (int i=0; i<4; i++) {
					try {
						sleep(1000);
						
						if (trialIndex != myTrialIndex) return;
						
						countdown = 3-i;
						repaint();
					} catch (Exception exc) {
						exc.printStackTrace();
					}
				}
				responseMade(-1);
			}
		}.start();
	}
	
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		
		if (state == STATE_BLANK) return;
		
		// this is set up to copy, as well as possible, the Flash program
		//
		// size of the Flash window was 900x600
		
		// amount we have to shift right so that 900x600 is centered on screen
		int xoff = this.getWidth()/2 - 450;
		// same for shift down
		int yoff = this.getHeight()/2 - 300;
		
		Font f = new Font("Serif", Font.PLAIN, 48);
		FontMetrics fm = this.getFontMetrics(f);
		g.setFont(f);
		g.setColor(Color.BLACK);
		
		int ansBoxW = 135;
		int ansBoxH = 75;
		int[] boxX = {45, 266, 489, 708};
		int boxY = 41;
		
		for (int i=0; i<4; i++) {
			if (state == STATE_RESPONSE && i==response)
				g.setColor(new Color(200,128,128));
			else
				g.setColor(Color.LIGHT_GRAY);
			g.fillOval(boxX[i]+xoff, boxY+yoff, ansBoxW, ansBoxH);
			g.setColor(Color.BLACK);
			g.drawOval(boxX[i]+xoff, boxY+yoff, ansBoxW, ansBoxH);
			g.drawString(names[i],
					boxX[i]+xoff + (ansBoxW - fm.stringWidth(names[i]))/2,
					boxY+yoff + ansBoxH/2 + fm.getAscent()/2);
		}
		
		g.drawImage(imgBuffer.getCurrentImage(), 193+xoff, 136+yoff, this);
		
		if (countdown != -1) {
			f = new Font("Serif", Font.PLAIN, 92);
			fm = this.getFontMetrics(f);
			g.setFont(f);
			g.setColor(Color.RED);
			
			g.drawString(""+countdown, this.getWidth()/2-fm.stringWidth(""+countdown)/2,
					504+yoff+fm.getAscent()/2);
		}
	}

	class ImageBuffer {
		private Image[] img = new Image[5];
		private int imgIndex = 0;
		
		public ImageBuffer() {
			MediaTracker mt = new MediaTracker(TestingComponent.this);
			for (int i=0; i<5; i++) {
				imgIndex++;
				Image newImg = Toolkit.getDefaultToolkit().createImage(
					ClassLoader.getSystemResource("img/test/t"+imgIndex+".JPG")
				);
				mt.addImage(newImg, i);
				img[i] = newImg;
			}
			try {
				mt.waitForAll();
			} catch (Exception exc) {
				exc.printStackTrace();
			}
		}
		
		public void loadNextImage() {
			imgIndex++;
			
			img[0].flush();
			img[0] = null;
			for (int i=1; i<5; i++) {
				img[i-1] = img[i];
			}
			java.net.URL imgURL = ClassLoader.getSystemResource("img/test/t"+imgIndex+".JPG");
			if (imgURL != null)
				img[4] = Toolkit.getDefaultToolkit().createImage(imgURL);
			else
				img[4] = null;
		}
		
		public Image getCurrentImage() {
			return img[0];
		}
	}
}
