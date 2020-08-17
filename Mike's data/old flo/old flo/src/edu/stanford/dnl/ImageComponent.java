package edu.stanford.dnl;

import java.awt.*;

public class ImageComponent extends ExperimentComponent {
	private Image image = null;
	
	// values of -1 indicate size unknown at all times
	private int imageW = -1;
	private int imageH = -1;
	
	public ImageComponent(ExperimentSequence es) {
		super(es);
	}
	
	public ImageComponent(ExperimentSequence es, Image image) {
		this(es);
		this.image = image;
		imageW = image.getWidth(this);
		imageH = image.getHeight(this);
	}
	
	public void paintComponent(Graphics g) {
		if (image == null) return;
		
		int w = this.getWidth();
		int h = this.getHeight();
		
		if (imageW == -1 || imageH == -1) {
			imageW = image.getWidth(this);
			imageH = image.getHeight(this);
		}
		
		// image still not loaded
		if (imageW == -1 || imageH == -1) return;
		
		g.drawImage(image, w/2 - imageW/2, h/2 - imageH/2, this);
	}
	
}
