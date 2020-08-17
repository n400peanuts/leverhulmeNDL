import edu.stanford.dnl.*;

import java.awt.*;
import java.awt.event.KeyEvent;


public class SXTrainingComponent extends ExperimentComponent {
	private ImageBuffer imgBuffer;
	
	private final int STATE_BLANK = 0;
	private final int STATE_IMAGE = 1;
	private int state = STATE_BLANK;
	
	private boolean running = false;
	private boolean skipped = false;
	
	public SXTrainingComponent(ExperimentSequence es) {
		super(es);
		setBackground(Color.WHITE);
		
		imgBuffer = new ImageBuffer();
	}
	
	public void start() {
		super.start();
	}
	
	public void startMeUp() {	
		new Thread() {
			public void run() {
				System.out.println(System.currentTimeMillis()+"\tBEGIN RUN");
				try {
					sleep(5000);
					System.out.println(System.currentTimeMillis()+"\tBEGIN SX TRAIN");
				} catch (Exception exc) {
					exc.printStackTrace();
				}
				
				int index = 0;
				while (imgBuffer.getCurrentImage() != null) {
					if (skipped) {
						SXTrainingComponent.this.complete();
						return;
					}
					
					state = STATE_IMAGE;
					System.out.println(System.currentTimeMillis()+"\tS\t" + index);
					repaint();
					
					try {
						sleep(1000);
					} catch (Exception exc) {
						exc.printStackTrace();
					}
					
					state = STATE_BLANK;
					imgBuffer.loadNextImage();
					repaint();
					System.out.println(System.currentTimeMillis()+"\tDELAY\t"+index);
					
					try {
						sleep(150);
					} catch (Exception exc) {
						exc.printStackTrace();
					}
					
					state = STATE_IMAGE;
					System.out.println(System.currentTimeMillis()+"\tX\t" + index);
					repaint();
					
					try {
						sleep(175);
					} catch (Exception exc) {
						exc.printStackTrace();
					}
					
					state = STATE_BLANK;
					imgBuffer.loadNextImage();
					repaint();
					System.out.println(System.currentTimeMillis()+"\tDELAY\t"+ index++);
					
					try {
						sleep(1000);
					} catch (Exception exc) {
						exc.printStackTrace();
					}
					
				}
				System.out.println(System.currentTimeMillis() + "\tEND SX TRAIN");
				
				try {
					sleep(5000);
				} catch (Exception exc) {
					exc.printStackTrace();
				}
				
				System.out.println(System.currentTimeMillis() + "\tEND RUN");
				complete();
			}
		}.start();
	}
	
	public void paintComponent(Graphics g) {
		super.paintComponent(g);
		
		if (state == STATE_BLANK) return;
		
		Image im = imgBuffer.getCurrentImage();
		if (im == null) return;
		g.drawImage(im, this.getWidth()/2-im.getWidth(this)/2,
				this.getHeight()/2-im.getHeight(this)/2, this);
	}
	
	public void keyPressed(KeyEvent e) {
		switch (e.getKeyCode()) {
		case KeyEvent.VK_N:
			System.out.println(System.currentTimeMillis() + "\tSX TRAIN SKIPPED");
			skipped = true;
			break;
		case KeyEvent.VK_5:
			if (!running) {
				startMeUp();
			}
			System.out.println(System.currentTimeMillis() + "\t" + "SCAN");
			break;
		}
	}
	
	class ImageBuffer {
		private Image[] img = new Image[10];
		private int imgIndex = 0;
		
		public ImageBuffer() {
			MediaTracker mt = new MediaTracker(SXTrainingComponent.this);
			for (int i=0; i<img.length; i++) {
				imgIndex += 2;
				Image newImg = Toolkit.getDefaultToolkit().createImage(
					ClassLoader.getSystemResource("img/sx/tSlide"+imgIndex+".PNG")
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
			imgIndex += 2;
			
			img[0].flush();
			img[0] = null;
			for (int i=1; i<img.length; i++) {
				img[i-1] = img[i];
			}
			java.net.URL imgURL = ClassLoader.getSystemResource("img/sx/tSlide"+imgIndex+".PNG");
			if (imgURL != null)
				img[img.length-1] = Toolkit.getDefaultToolkit().createImage(imgURL);
			else
				img[img.length-1] = null;
		}
		
		public Image getCurrentImage() {
			return img[0];
		}
	}
	
}
