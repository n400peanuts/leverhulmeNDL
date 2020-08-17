### label picture ####
    filename <- c("1label-3pictures")
    temp1 <- aggregate(labPic[labPic$learning=="FL" & labPic$correctFrequency=="high",]$acc, list(labPic[labPic$learning=="FL" & labPic$correctFrequency=="high",]$subjID), mean)
    names(temp1) <- c("sbj.id", "acc.FL.high");
    temp2 <- aggregate(labPic[labPic$learning=="FL" & labPic$correctFrequency=="low",]$acc, list(labPic[labPic$learning=="FL" & labPic$correctFrequency=="low",]$subjID), mean)
    names(temp2) <- c("sbj.id", "acc.FL.low");
    temp3 <- aggregate(labPic[labPic$learning=="LF" & labPic$correctFrequency=="high",]$acc, list(labPic[labPic$learning=="LF" & labPic$correctFrequency=="high",]$subjID), mean)
    names(temp3) <- c("sbj.id", "acc.LF.high");
    temp4 <- aggregate(labPic[labPic$learning=="LF" & labPic$correctFrequency=="low",]$acc, list(labPic[labPic$learning=="LF" & labPic$correctFrequency=="low",]$subjID), mean)
    names(temp4) <- c("sbj.id", "acc.LF.low");
    sbj.FL.diagnostics <- merge(temp1,temp2);
    sbj.LF.diagnostics <- merge(temp4,temp3);

    label.diagnostics <- aggregate(labPic[labPic$label!="bim",]$acc, list(labPic[labPic$label!="bim",]$label, labPic[labPic$label!="bim",]$correctFrequency, labPic[labPic$label!="bim",]$learning), mean) 
    names(label.diagnostics) <- c("label.id", "freq", "learning", "acc");
    temp1 <- aggregate(labPic[labPic$label!="bim",]$rt, list(labPic[labPic$label!="bim",]$label, labPic[labPic$label!="bim",]$correctFrequency, labPic[labPic$label!="bim",]$learning), mean) 
    names(temp1) <- c("label.id", "freq", "learning","rt");
    label.diagnostics <- merge(label.diagnostics, temp1)

    temp1 <- aggregate(labPic[labPic$learning=="FL" & labPic$correctFrequency=="high",]$rt, list(labPic[labPic$learning=="FL" & labPic$correctFrequency=="high",]$subjID), mean)
    names(temp1) <- c("sbj.id", "rt.FL.high");
    temp2 <- aggregate(labPic[labPic$learning=="FL" & labPic$correctFrequency=="low",]$rt, list(labPic[labPic$learning=="FL" & labPic$correctFrequency=="low",]$subjID), mean)
    names(temp2) <- c("sbj.id", "rt.FL.low");
    temp3 <- aggregate(labPic[labPic$learning=="LF" & labPic$correctFrequency=="high",]$rt, list(labPic[labPic$learning=="LF" & labPic$correctFrequency=="high",]$subjID), mean)
    names(temp3) <- c("sbj.id", "rt.LF.high");
    temp4 <- aggregate(labPic[labPic$learning=="LF" & labPic$correctFrequency=="low",]$rt, list(labPic[labPic$learning=="LF" & labPic$correctFrequency=="low",]$subjID), mean)
    names(temp4) <- c("sbj.id", "rt.LF.low");
    sbj.FL.diagnostics <- merge(temp1, sbj.FL.diagnostics);
    sbj.FL.diagnostics <- merge(temp2, sbj.FL.diagnostics);
    sbj.LF.diagnostics <- merge(temp3, sbj.LF.diagnostics);
    sbj.LF.diagnostics <- merge(temp4, sbj.LF.diagnostics);
    
    sbj.FL.diagnostics$sbj.id <- c(1:length(sbj.FL.diagnostics$sbj.id))
    sbj.LF.diagnostics$sbj.id <- c((length(sbj.FL.diagnostics$sbj.id)+1):(length(sbj.FL.diagnostics$sbj.id)+length(sbj.LF.diagnostics$sbj.id)))
    
    

    temp1 <- aggregate(labPic[labPic$learning=="FL" & labPic$correctFrequency=="high",]$acc, list(labPic[labPic$learning=="FL" & labPic$correctFrequency=="high",]$subjID), sd)
    for (i in 1:length(temp1)) temp1$x[i] <- (temp1$x[i] / sqrt(length(temp1$x)))
    names(temp1) <- c("sbj.id", "acc.FL.high");
    temp2 <- aggregate(labPic[labPic$learning=="FL" & labPic$correctFrequency=="low",]$acc, list(labPic[labPic$learning=="FL" & labPic$correctFrequency=="low",]$subjID), sd)
    for (i in 1:length(temp2)) temp2$x[i] <- (temp2$x[i] / sqrt(length(temp2$x)))
    names(temp2) <- c("sbj.id", "acc.FL.low");
    temp3 <- aggregate(labPic[labPic$learning=="LF" & labPic$correctFrequency=="high",]$acc, list(labPic[labPic$learning=="LF" & labPic$correctFrequency=="high",]$subjID), sd)
    for (i in 1:length(temp3)) temp3$x[i] <- (temp3$x[i] / sqrt(length(temp3$x)))
    names(temp3) <- c("sbj.id", "acc.LF.high");
    temp4 <- aggregate(labPic[labPic$learning=="LF" & labPic$correctFrequency=="low",]$acc, list(labPic[labPic$learning=="LF" & labPic$correctFrequency=="low",]$subjID), sd)
    for (i in 1:length(temp4)) temp4$x[i] <- (temp4$x[i] / sqrt(length(temp4$x)))
    names(temp4) <- c("sbj.id", "acc.LF.low");
    sbj.FL.diagnostics.sd <- merge(temp1,temp2);
    sbj.LF.diagnostics.sd <- merge(temp4,temp3);
    
    sbj.FL.diagnostics$se <- sbj.FL.diagnostics.sd
    sbj.LF.diagnostics$se <- sbj.LF.diagnostics.sd
    
    ymin <- min(min(sbj.FL.diagnostics$acc.FL.high),min(sbj.FL.diagnostics$acc.FL.low));
    ymax <- max(max(sbj.FL.diagnostics$acc.FL.high),max(sbj.FL.diagnostics$acc.FL.low));
    xmin <- min(min(sbj.FL.diagnostics$rt.FL.high),min(sbj.FL.diagnostics$rt.FL.low));
    xmax <- max(max(sbj.FL.diagnostics$rt.FL.high),max(sbj.FL.diagnostics$rt.FL.low));
    
    
    jpeg(paste(as.character(filename),".jpg",sep=""), res=300, height=2000, width=3500);
    layout(matrix(c(1,2,3,4), nrow=2, byrow=T), heights=c(2,1.2));	 

    plot(sbj.FL.diagnostics$rt.FL.low, sbj.FL.diagnostics$acc.FL.low, xlab="RT (ms)", ylab="% correct", main="Subjects FL", type="n", ylim=c(ymin,ymax), xlim=c(xmin,xmax));
    
    symbols(as.numeric(unlist(round(sbj.FL.diagnostics$rt.FL.low))), as.numeric(unlist(round(sbj.FL.diagnostics$acc.FL.low,2))), fg="red", add=T, inches=F, circles= as.numeric(unlist(sbj.FL.diagnostics$se[3]*100)));
    symbols(as.numeric(unlist(round(sbj.FL.diagnostics$rt.FL.high))), as.numeric(unlist(round(sbj.FL.diagnostics$acc.FL.high,2))), fg="blue", add=T, inches=F, circles= as.numeric(unlist(sbj.FL.diagnostics$se[2]*100)));
    text(round(sbj.FL.diagnostics$rt.FL.low), round(sbj.FL.diagnostics$acc.FL.low,2), as.character(sbj.FL.diagnostics$sbj.id), col="red");
    text(round(sbj.FL.diagnostics$rt.FL.high), round(sbj.FL.diagnostics$acc.FL.high,2), as.character(sbj.FL.diagnostics$sbj.id), col="blue");
    for (i in 1:nrow(sbj.FL.diagnostics)) lines(c(sbj.FL.diagnostics$rt.FL.low[i], sbj.FL.diagnostics$rt.FL.high[i]), c(sbj.FL.diagnostics$acc.FL.low[i], sbj.FL.diagnostics$acc.FL.high[i]), col=grey(.70));

    abline(v=mean(sbj.FL.diagnostics$rt.FL.low), col="red", lty=2, lwd=2);
    abline(v=mean(sbj.FL.diagnostics$rt.FL.high), col="blue", lty=2, lwd=2);
    abline(h=mean(sbj.FL.diagnostics$acc.FL.low), col="red", lty=2, lwd=2);
    abline(h=mean(sbj.FL.diagnostics$acc.FL.high), col="blue", lty=2, lwd=2);
    
    
    
    ymin <- min(min(sbj.LF.diagnostics$acc.LF.high),min(sbj.LF.diagnostics$acc.LF.low));
    ymax <- max(max(sbj.LF.diagnostics$acc.LF.high),max(sbj.LF.diagnostics$acc.LF.low));
    xmin <- min(min(sbj.LF.diagnostics$rt.LF.high),min(sbj.LF.diagnostics$rt.LF.low));
    xmax <- max(max(sbj.LF.diagnostics$rt.LF.high),max(sbj.LF.diagnostics$rt.LF.low));
    
    plot(sbj.LF.diagnostics$rt.LF.low, sbj.LF.diagnostics$acc.LF.low, xlab="RT (ms)", ylab="% correct", main="Subjects LF", type="n", ylim=c(ymin,ymax), xlim=c(xmin,xmax));
    
    symbols(as.numeric(unlist(round(sbj.LF.diagnostics$rt.LF.low))), as.numeric(unlist(round(sbj.LF.diagnostics$acc.LF.low,2))), fg="red", add=T, inches=F, circles= as.numeric(unlist(sbj.LF.diagnostics$se[3]*100)));
    symbols(as.numeric(unlist(round(sbj.LF.diagnostics$rt.LF.high))), as.numeric(unlist(round(sbj.LF.diagnostics$acc.LF.high,2))), fg="blue", add=T, inches=F, circles= as.numeric(unlist(sbj.LF.diagnostics$se[2]*100)));
    text(round(sbj.LF.diagnostics$rt.LF.low), round(sbj.LF.diagnostics$acc.LF.low,2), as.character(sbj.LF.diagnostics$sbj.id), col="red");
    text(round(sbj.LF.diagnostics$rt.LF.high), round(sbj.LF.diagnostics$acc.LF.high,2), as.character(sbj.LF.diagnostics$sbj.id), col="blue");
    for (i in 1:nrow(sbj.LF.diagnostics)) lines(c(sbj.LF.diagnostics$rt.LF.low[i], sbj.LF.diagnostics$rt.LF.high[i]), c(sbj.LF.diagnostics$acc.LF.low[i], sbj.LF.diagnostics$acc.LF.high[i]), col=grey(.70));
    
    abline(v=mean(sbj.LF.diagnostics$rt.LF.low), col="red", lty=2, lwd=2);
    abline(v=mean(sbj.LF.diagnostics$rt.LF.high), col="blue", lty=2, lwd=2);
    abline(h=mean(sbj.LF.diagnostics$acc.LF.low), col="red", lty=2, lwd=2);
    abline(h=mean(sbj.LF.diagnostics$acc.LF.high), col="blue", lty=2, lwd=2);
    
    
    plot(label.diagnostics$rt, label.diagnostics$acc, pch=19, xlab="RT (ms)", ylab="% correct", main="Labels", type="n");
    text(label.diagnostics$rt, label.diagnostics$acc, as.character(label.diagnostics$label.id));
    
    hist(labPic$rt, xlab="RT (ms)", ylab="Density", main="Individual datapoints", breaks=50);
    
    dev.off()
    
    
    ### picture label ####
    picLab <- na.omit(picLab)
    filename <- c("1picture-3labels")
    temp1 <- aggregate(picLab[picLab$learning=="FL" & picLab$correctFrequency=="high",]$acc, list(picLab[picLab$learning=="FL" & picLab$correctFrequency=="high",]$subjID), mean)
    names(temp1) <- c("sbj.id", "acc.FL.high");
    temp2 <- aggregate(picLab[picLab$learning=="FL" & picLab$correctFrequency=="low",]$acc, list(picLab[picLab$learning=="FL" & picLab$correctFrequency=="low",]$subjID), mean)
    names(temp2) <- c("sbj.id", "acc.FL.low");
    temp3 <- aggregate(picLab[picLab$learning=="LF" & picLab$correctFrequency=="high",]$acc, list(picLab[picLab$learning=="LF" & picLab$correctFrequency=="high",]$subjID), mean)
    names(temp3) <- c("sbj.id", "acc.LF.high");
    temp4 <- aggregate(picLab[picLab$learning=="LF" & picLab$correctFrequency=="low",]$acc, list(picLab[picLab$learning=="LF" & picLab$correctFrequency=="low",]$subjID), mean)
    names(temp4) <- c("sbj.id", "acc.LF.low");
    sbj.FL.diagnostics <- merge(temp1,temp2);
    sbj.LF.diagnostics <- merge(temp4,temp3);
    
    label.diagnostics <- aggregate(picLab[picLab$resp!="bim",]$acc, list(picLab[picLab$resp!="bim",]$resp, picLab[picLab$resp!="bim",]$correctFrequency, picLab[picLab$resp!="bim",]$learning), mean) 
    names(label.diagnostics) <- c("label.id", "freq", "learning", "acc");
    temp1 <- aggregate(picLab[picLab$resp!="bim",]$rt, list(picLab[picLab$resp!="bim",]$resp, picLab[picLab$resp!="bim",]$correctFrequency, picLab[picLab$resp!="bim",]$learning), mean) 
    names(temp1) <- c("label.id", "freq", "learning","rt");
    label.diagnostics <- merge(label.diagnostics, temp1)
    
    temp1 <- aggregate(picLab[picLab$learning=="FL" & picLab$correctFrequency=="high",]$rt, list(picLab[picLab$learning=="FL" & picLab$correctFrequency=="high",]$subjID), mean)
    names(temp1) <- c("sbj.id", "rt.FL.high");
    temp2 <- aggregate(picLab[picLab$learning=="FL" & picLab$correctFrequency=="low",]$rt, list(picLab[picLab$learning=="FL" & picLab$correctFrequency=="low",]$subjID), mean)
    names(temp2) <- c("sbj.id", "rt.FL.low");
    temp3 <- aggregate(picLab[picLab$learning=="LF" & picLab$correctFrequency=="high",]$rt, list(picLab[picLab$learning=="LF" & picLab$correctFrequency=="high",]$subjID), mean)
    names(temp3) <- c("sbj.id", "rt.LF.high");
    temp4 <- aggregate(picLab[picLab$learning=="LF" & picLab$correctFrequency=="low",]$rt, list(picLab[picLab$learning=="LF" & picLab$correctFrequency=="low",]$subjID), mean)
    names(temp4) <- c("sbj.id", "rt.LF.low");
    sbj.FL.diagnostics <- merge(temp1, sbj.FL.diagnostics);
    sbj.FL.diagnostics <- merge(temp2, sbj.FL.diagnostics);
    sbj.LF.diagnostics <- merge(temp3, sbj.LF.diagnostics);
    sbj.LF.diagnostics <- merge(temp4, sbj.LF.diagnostics);
    
    sbj.FL.diagnostics$sbj.id <- c(1:length(sbj.FL.diagnostics$sbj.id))
    sbj.LF.diagnostics$sbj.id <- c((length(sbj.FL.diagnostics$sbj.id)+1):(length(sbj.FL.diagnostics$sbj.id)+length(sbj.LF.diagnostics$sbj.id)))
    
    
    temp1 <- aggregate(picLab[picLab$learning=="FL" & picLab$correctFrequency=="high",]$acc, list(picLab[picLab$learning=="FL" & picLab$correctFrequency=="high",]$subjID), sd)
    for (i in 1:length(temp1)) temp1$x[i] <- (temp1$x[i] / sqrt(length(temp1$x)))
    names(temp1) <- c("sbj.id", "acc.FL.high");
    temp2 <- aggregate(picLab[picLab$learning=="FL" & picLab$correctFrequency=="low",]$acc, list(picLab[picLab$learning=="FL" & picLab$correctFrequency=="low",]$subjID), sd)
    for (i in 1:length(temp2)) temp2$x[i] <- (temp2$x[i] / sqrt(length(temp2$x)))
    names(temp2) <- c("sbj.id", "acc.FL.low");
    temp3 <- aggregate(picLab[picLab$learning=="LF" & picLab$correctFrequency=="high",]$acc, list(picLab[picLab$learning=="LF" & picLab$correctFrequency=="high",]$subjID), sd)
    for (i in 1:length(temp3)) temp3$x[i] <- (temp3$x[i] / sqrt(length(temp3$x)))
    names(temp3) <- c("sbj.id", "acc.LF.high");
    temp4 <- aggregate(picLab[picLab$learning=="LF" & picLab$correctFrequency=="low",]$acc, list(picLab[picLab$learning=="LF" & picLab$correctFrequency=="low",]$subjID), sd)
    for (i in 1:length(temp4)) temp4$x[i] <- (temp4$x[i] / sqrt(length(temp4$x)))
    names(temp4) <- c("sbj.id", "acc.LF.low");
    sbj.FL.diagnostics.sd <- merge(temp1,temp2);
    sbj.LF.diagnostics.sd <- merge(temp4,temp3);
    
    sbj.FL.diagnostics$se <- sbj.FL.diagnostics.sd
    sbj.LF.diagnostics$se <- sbj.LF.diagnostics.sd
    
    ymin <- min(min(sbj.FL.diagnostics$acc.FL.high),min(sbj.FL.diagnostics$acc.FL.low));
    ymax <- max(max(sbj.FL.diagnostics$acc.FL.high),max(sbj.FL.diagnostics$acc.FL.low));
    xmin <- min(min(sbj.FL.diagnostics$rt.FL.high),min(sbj.FL.diagnostics$rt.FL.low));
    xmax <- max(max(sbj.FL.diagnostics$rt.FL.high),max(sbj.FL.diagnostics$rt.FL.low));
    
    
    jpeg(paste(as.character(filename),".jpg",sep=""), res=300, height=2000, width=3500);
    layout(matrix(c(1,2,3,4), nrow=2, byrow=T), heights=c(2,1.2));	 
    
    
    plot(sbj.FL.diagnostics$rt.FL.low, sbj.FL.diagnostics$acc.FL.low, xlab="RT (ms)", ylab="% correct", main="Subjects FL", type="n", ylim=c(ymin,ymax), xlim=c(xmin,xmax));
    
    symbols(as.numeric(unlist(round(sbj.FL.diagnostics$rt.FL.low))), as.numeric(unlist(round(sbj.FL.diagnostics$acc.FL.low,2))), fg="red", add=T, inches=F, circles= as.numeric(unlist(sbj.FL.diagnostics$se[3]*100)));
    symbols(as.numeric(unlist(round(sbj.FL.diagnostics$rt.FL.high))), as.numeric(unlist(round(sbj.FL.diagnostics$acc.FL.high,2))), fg="blue", add=T, inches=F, circles= as.numeric(unlist(sbj.FL.diagnostics$se[2]*100)));
    text(round(sbj.FL.diagnostics$rt.FL.low), round(sbj.FL.diagnostics$acc.FL.low,2), as.character(sbj.FL.diagnostics$sbj.id), col="red");
    text(round(sbj.FL.diagnostics$rt.FL.high), round(sbj.FL.diagnostics$acc.FL.high,2), as.character(sbj.FL.diagnostics$sbj.id), col="blue");
    for (i in 1:nrow(sbj.FL.diagnostics)) lines(c(sbj.FL.diagnostics$rt.FL.low[i], sbj.FL.diagnostics$rt.FL.high[i]), c(sbj.FL.diagnostics$acc.FL.low[i], sbj.FL.diagnostics$acc.FL.high[i]), col=grey(.70));
    
    abline(v=mean(sbj.FL.diagnostics$rt.FL.low), col="red", lty=2, lwd=2);
    abline(v=mean(sbj.FL.diagnostics$rt.FL.high), col="blue", lty=2, lwd=2);
    abline(h=mean(sbj.FL.diagnostics$acc.FL.low), col="red", lty=2, lwd=2);
    abline(h=mean(sbj.FL.diagnostics$acc.FL.high), col="blue", lty=2, lwd=2);
    
    
    ymin <- min(min(sbj.LF.diagnostics$acc.LF.high),min(sbj.LF.diagnostics$acc.LF.low));
    ymax <- max(max(sbj.LF.diagnostics$acc.LF.high),max(sbj.LF.diagnostics$acc.LF.low));
    xmin <- min(min(sbj.LF.diagnostics$rt.LF.high),min(sbj.LF.diagnostics$rt.LF.low));
    xmax <- max(max(sbj.LF.diagnostics$rt.LF.high),max(sbj.LF.diagnostics$rt.LF.low));
    
    
    plot(sbj.LF.diagnostics$rt.LF.low, sbj.LF.diagnostics$acc.LF.low, xlab="RT (ms)", ylab="% correct", main="Subjects LF", type="n", ylim=c(ymin,ymax), xlim=c(xmin,xmax));
    
    symbols(as.numeric(unlist(round(sbj.LF.diagnostics$rt.LF.low))), as.numeric(unlist(round(sbj.LF.diagnostics$acc.LF.low,2))), fg="red", add=T, inches=F, circles= as.numeric(unlist(sbj.LF.diagnostics$se[3]*100)));
    symbols(as.numeric(unlist(round(sbj.LF.diagnostics$rt.LF.high))), as.numeric(unlist(round(sbj.LF.diagnostics$acc.LF.high,2))), fg="blue", add=T, inches=F, circles= as.numeric(unlist(sbj.LF.diagnostics$se[2]*100)));
    text(round(sbj.LF.diagnostics$rt.LF.low), round(sbj.LF.diagnostics$acc.LF.low,2), as.character(sbj.LF.diagnostics$sbj.id), col="red");
    text(round(sbj.LF.diagnostics$rt.LF.high), round(sbj.LF.diagnostics$acc.LF.high,2), as.character(sbj.LF.diagnostics$sbj.id), col="blue");
    for (i in 1:nrow(sbj.LF.diagnostics)) lines(c(sbj.LF.diagnostics$rt.LF.low[i], sbj.LF.diagnostics$rt.LF.high[i]), c(sbj.LF.diagnostics$acc.LF.low[i], sbj.LF.diagnostics$acc.LF.high[i]), col=grey(.70));
    
    abline(v=mean(sbj.LF.diagnostics$rt.LF.low), col="red", lty=2, lwd=2);
    abline(v=mean(sbj.LF.diagnostics$rt.LF.high), col="blue", lty=2, lwd=2);
    abline(h=mean(sbj.LF.diagnostics$acc.LF.low), col="red", lty=2, lwd=2);
    abline(h=mean(sbj.LF.diagnostics$acc.LF.high), col="blue", lty=2, lwd=2);
    
    
    plot(label.diagnostics$rt, label.diagnostics$acc, pch=19, xlab="RT (ms)", ylab="% correct", main="Labels", type="n");
    text(label.diagnostics$rt, label.diagnostics$acc, as.character(label.diagnostics$label.id));
    
    hist(picLab$rt, xlab="RT (ms)", ylab="Density", main="Individual datapoints", breaks=50);
    
    dev.off()
    
    
#### both tasks ####
    filename <- c("bothtasks")
    temp1 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="high",]$acc, list(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="high",]$subjID), mean)
    names(temp1) <- c("sbj.id", "acc.FL.high");
    temp2 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="low",]$acc, list(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="low",]$subjID), mean)
    names(temp2) <- c("sbj.id", "acc.FL.low");
    temp3 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="high",]$acc, list(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="high",]$subjID), mean)
    names(temp3) <- c("sbj.id", "acc.LF.high");
    temp4 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="low",]$acc, list(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="low",]$subjID), mean)
    names(temp4) <- c("sbj.id", "acc.LF.low");
    sbj.FL.diagnostics <- merge(temp1,temp2);
    sbj.LF.diagnostics <- merge(temp4,temp3);
    
    label.diagnostics <- aggregate(FLO_2tasks[FLO_2tasks$label!="bim",]$acc, list(FLO_2tasks[FLO_2tasks$label!="bim",]$label, FLO_2tasks[FLO_2tasks$label!="bim",]$correctFrequency, FLO_2tasks[FLO_2tasks$label!="bim",]$learning), mean) 
    names(label.diagnostics) <- c("label.id", "freq", "learning", "acc");
    temp1 <- aggregate(FLO_2tasks[FLO_2tasks$label!="bim",]$rt, list(FLO_2tasks[FLO_2tasks$label!="bim",]$label, FLO_2tasks[FLO_2tasks$label!="bim",]$correctFrequency, FLO_2tasks[FLO_2tasks$label!="bim",]$learning), mean) 
    names(temp1) <- c("label.id", "freq", "learning","rt");
    label.diagnostics <- merge(label.diagnostics, temp1)
    
    temp1 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="high",]$rt, list(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="high",]$subjID), mean)
    names(temp1) <- c("sbj.id", "rt.FL.high");
    temp2 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="low",]$rt, list(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="low",]$subjID), mean)
    names(temp2) <- c("sbj.id", "rt.FL.low");
    temp3 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="high",]$rt, list(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="high",]$subjID), mean)
    names(temp3) <- c("sbj.id", "rt.LF.high");
    temp4 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="low",]$rt, list(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="low",]$subjID), mean)
    names(temp4) <- c("sbj.id", "rt.LF.low");
    sbj.FL.diagnostics <- merge(temp1, sbj.FL.diagnostics);
    sbj.FL.diagnostics <- merge(temp2, sbj.FL.diagnostics);
    sbj.LF.diagnostics <- merge(temp3, sbj.LF.diagnostics);
    sbj.LF.diagnostics <- merge(temp4, sbj.LF.diagnostics);
    
    sbj.FL.diagnostics$sbj.id <- c(1:length(sbj.FL.diagnostics$sbj.id))
    sbj.LF.diagnostics$sbj.id <- c((length(sbj.FL.diagnostics$sbj.id)+1):(length(sbj.FL.diagnostics$sbj.id)+length(sbj.LF.diagnostics$sbj.id)))
    
    
    
    temp1 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="high",]$acc, list(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="high",]$subjID), sd)
    for (i in 1:length(temp1)) temp1$x[i] <- (temp1$x[i] / sqrt(length(temp1$x)))
    names(temp1) <- c("sbj.id", "acc.FL.high");
    temp2 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="low",]$acc, list(FLO_2tasks[FLO_2tasks$learning=="FL" & FLO_2tasks$correctFrequency=="low",]$subjID), sd)
    for (i in 1:length(temp2)) temp2$x[i] <- (temp2$x[i] / sqrt(length(temp2$x)))
    names(temp2) <- c("sbj.id", "acc.FL.low");
    temp3 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="high",]$acc, list(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="high",]$subjID), sd)
    for (i in 1:length(temp3)) temp3$x[i] <- (temp3$x[i] / sqrt(length(temp3$x)))
    names(temp3) <- c("sbj.id", "acc.LF.high");
    temp4 <- aggregate(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="low",]$acc, list(FLO_2tasks[FLO_2tasks$learning=="LF" & FLO_2tasks$correctFrequency=="low",]$subjID), sd)
    for (i in 1:length(temp4)) temp4$x[i] <- (temp4$x[i] / sqrt(length(temp4$x)))
    names(temp4) <- c("sbj.id", "acc.LF.low");
    sbj.FL.diagnostics.sd <- merge(temp1,temp2);
    sbj.LF.diagnostics.sd <- merge(temp4,temp3);
    
    sbj.FL.diagnostics$se <- sbj.FL.diagnostics.sd
    sbj.LF.diagnostics$se <- sbj.LF.diagnostics.sd
    
    ymin <- min(min(sbj.FL.diagnostics$acc.FL.high),min(sbj.FL.diagnostics$acc.FL.low));
    ymax <- max(max(sbj.FL.diagnostics$acc.FL.high),max(sbj.FL.diagnostics$acc.FL.low));
    xmin <- min(min(sbj.FL.diagnostics$rt.FL.high),min(sbj.FL.diagnostics$rt.FL.low));
    xmax <- max(max(sbj.FL.diagnostics$rt.FL.high),max(sbj.FL.diagnostics$rt.FL.low));
    
    
    jpeg(paste(as.character(filename),".jpg",sep=""), res=300, height=2000, width=3500);
    layout(matrix(c(1,2,3,4), nrow=2, byrow=T), heights=c(2,1.2));	 
    
    plot(sbj.FL.diagnostics$rt.FL.low, sbj.FL.diagnostics$acc.FL.low, xlab="RT (ms)", ylab="% correct", main="Subjects FL", type="n", ylim=c(ymin,ymax), xlim=c(xmin,xmax));
    
    symbols(as.numeric(unlist(round(sbj.FL.diagnostics$rt.FL.low))), as.numeric(unlist(round(sbj.FL.diagnostics$acc.FL.low,2))), fg="red", add=T, inches=F, circles= as.numeric(unlist(sbj.FL.diagnostics$se[3]*100)));
    symbols(as.numeric(unlist(round(sbj.FL.diagnostics$rt.FL.high))), as.numeric(unlist(round(sbj.FL.diagnostics$acc.FL.high,2))), fg="blue", add=T, inches=F, circles= as.numeric(unlist(sbj.FL.diagnostics$se[2]*100)));
    text(round(sbj.FL.diagnostics$rt.FL.low), round(sbj.FL.diagnostics$acc.FL.low,2), as.character(sbj.FL.diagnostics$sbj.id), col="red");
    text(round(sbj.FL.diagnostics$rt.FL.high), round(sbj.FL.diagnostics$acc.FL.high,2), as.character(sbj.FL.diagnostics$sbj.id), col="blue");
    for (i in 1:nrow(sbj.FL.diagnostics)) lines(c(sbj.FL.diagnostics$rt.FL.low[i], sbj.FL.diagnostics$rt.FL.high[i]), c(sbj.FL.diagnostics$acc.FL.low[i], sbj.FL.diagnostics$acc.FL.high[i]), col=grey(.70));
    
    abline(v=mean(sbj.FL.diagnostics$rt.FL.low), col="red", lty=2, lwd=2);
    abline(v=mean(sbj.FL.diagnostics$rt.FL.high), col="blue", lty=2, lwd=2);
    abline(h=mean(sbj.FL.diagnostics$acc.FL.low), col="red", lty=2, lwd=2);
    abline(h=mean(sbj.FL.diagnostics$acc.FL.high), col="blue", lty=2, lwd=2);
    
    
    
    ymin <- min(min(sbj.LF.diagnostics$acc.LF.high),min(sbj.LF.diagnostics$acc.LF.low));
    ymax <- max(max(sbj.LF.diagnostics$acc.LF.high),max(sbj.LF.diagnostics$acc.LF.low));
    xmin <- min(min(sbj.LF.diagnostics$rt.LF.high),min(sbj.LF.diagnostics$rt.LF.low));
    xmax <- max(max(sbj.LF.diagnostics$rt.LF.high),max(sbj.LF.diagnostics$rt.LF.low));
    
    plot(sbj.LF.diagnostics$rt.LF.low, sbj.LF.diagnostics$acc.LF.low, xlab="RT (ms)", ylab="% correct", main="Subjects LF", type="n", ylim=c(ymin,ymax), xlim=c(xmin,xmax));
    
    symbols(as.numeric(unlist(round(sbj.LF.diagnostics$rt.LF.low))), as.numeric(unlist(round(sbj.LF.diagnostics$acc.LF.low,2))), fg="red", add=T, inches=F, circles= as.numeric(unlist(sbj.LF.diagnostics$se[3]*100)));
    symbols(as.numeric(unlist(round(sbj.LF.diagnostics$rt.LF.high))), as.numeric(unlist(round(sbj.LF.diagnostics$acc.LF.high,2))), fg="blue", add=T, inches=F, circles= as.numeric(unlist(sbj.LF.diagnostics$se[2]*100)));
    text(round(sbj.LF.diagnostics$rt.LF.low), round(sbj.LF.diagnostics$acc.LF.low,2), as.character(sbj.LF.diagnostics$sbj.id), col="red");
    text(round(sbj.LF.diagnostics$rt.LF.high), round(sbj.LF.diagnostics$acc.LF.high,2), as.character(sbj.LF.diagnostics$sbj.id), col="blue");
    for (i in 1:nrow(sbj.LF.diagnostics)) lines(c(sbj.LF.diagnostics$rt.LF.low[i], sbj.LF.diagnostics$rt.LF.high[i]), c(sbj.LF.diagnostics$acc.LF.low[i], sbj.LF.diagnostics$acc.LF.high[i]), col=grey(.70));
    
    abline(v=mean(sbj.LF.diagnostics$rt.LF.low), col="red", lty=2, lwd=2);
    abline(v=mean(sbj.LF.diagnostics$rt.LF.high), col="blue", lty=2, lwd=2);
    abline(h=mean(sbj.LF.diagnostics$acc.LF.low), col="red", lty=2, lwd=2);
    abline(h=mean(sbj.LF.diagnostics$acc.LF.high), col="blue", lty=2, lwd=2);
    
    
    plot(label.diagnostics$rt, label.diagnostics$acc, pch=19, xlab="RT (ms)", ylab="% correct", main="Labels", type="n");
    text(label.diagnostics$rt, label.diagnostics$acc, as.character(label.diagnostics$label.id));
    
    hist(FLO_2tasks$rt, xlab="RT (ms)", ylab="Density", main="Individual datapoints", breaks=50);
    
    dev.off()
    
        
    
    