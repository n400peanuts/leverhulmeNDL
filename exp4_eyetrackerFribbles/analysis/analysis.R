#-----------------------------------------------------------#
#---------- this script takes the preprocessed data --------#
#---------- obtained by running preProcessing.R ------------#
#-------------------- and analyses it ----------------------#
#-----------------------------------------------------------#

rm(list=ls())

library(tidyverse)
library(ggpubr)
library(ggstatsplot)
library(wesanderson)

#----------- select the experiment ------------#
expeType <- "pilot1"


#### Set current working directory ####
localDirectory <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/")

# folder where we store the preprocessed data
input <- c(paste0(localDirectory,"preProcessed_data/",expeType,"/")) 

#folder where we save plots
output <- c(paste0(localDirectory,"analysis/",expeType,"/")) 


####--------------- load data ------------------####
#see what's in the folder
df <- list.files(input) 
df <- df[grepl("labPic|contingency",df)]

for (i in 1:length(df)){
  gsub(".csv$", "", df[i]) -> id #remove .csv
  
  if (id == "eyeTracker"){
    id <- "eyeTracker"
    assign("eyeTracker", data.frame()) #load into the environment with more intuitive names
    
  } else if (id == "labPic"){
    id <- "labPic"
    assign("labPic", data.frame()) 
    
  } else if (id == "contingency"){
    id <- "contingency"
    assign("contingency", data.frame())
    
  } else if (id == "ROIs"){
    id <- "ROIs"
    assign("ROIs", data.frame())
    
  } 
  
  read.csv(paste0(input, df[i]),
           na.strings=c("","NA"),
           stringsAsFactors = T)-> temp
  
  assign(paste0(id), temp)
  
}
rm(temp,id,i,df)

#### labPic ####
labPic <- labPic %>% mutate(frequency=recode(frequency, 
                                             "control_low"="control",
                                             "control_high"="control"))


labPic_eyetracker <- with(labPic, aggregate(acc ~ frequency + subjID, FUN=mean))

ggbetweenstats(
  data = labPic_eyetracker,
  x = frequency,
  y = acc,
  title = "2AFC - 4 pictures (pilot 1)",
  ggstatsplot.layer = FALSE,
  ggtheme = ggthemes::theme_clean(),
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)

ggsave(paste0(output, "labpic_pilot1.jpg"), height = 4, width = 5)

labPic$expeType <- as.factor(expeType)

labPic_p1<- labPic

bind_rows(labPic_p1, labPic) -> labPic
rm(labPic_p1)

#### contingency ####
contingency_eyetracker <- with(contingency, aggregate(resp ~ trialType + frequency + subjID, FUN=mean))

p2<-grouped_ggbetweenstats(
  data = contingency_eyetracker,
  x = frequency,
  y = resp,
  grouping.var = trialType,
  ggstatsplot.layer = FALSE,
  ggtheme = ggthemes::theme_clean(),
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE,
  annotation.args = list(title = "contingency task - pilot 1")
) 

ggsave(paste0(output, "contingency_pilot2.jpg"), height = 5, width = 9)

ggstatsplot::combine_plots(
  list(p2, p1),
  plotgrid.args = list(nrow = 2),
  annotation.args = list(
    title = "contingency task "
  )
)

ggsave(paste0(output, "contingency_pilot_comparison.jpg"), height = 5, width = 9)
contingency$expeType <- as.factor(expeType)

contingency_p1 <-contingency
bind_rows(contingency_p1, contingency) -> contingency
rm(contingency_p1)


####--------------- learning - eyetracker ------------------####
summary(eyeTracker) #looks good!
eyeTracker$subjID <- as.factor(eyeTracker$subjID)

# compute total proportion of looks in the ROIs
# these are the coordinates of our ROIs
summary(ROIs)

# First, I'm going to create a column for each ROI

eyeTracker$fribble <- 0
eyeTracker$cITI <- 0
eyeTracker$lITI <- 0
eyeTracker$rITI <- 0
eyeTracker$cLabel <- 0
eyeTracker$lLabel <- 0
eyeTracker$rLabel <- 0
eyeTracker$cButton <- 0
eyeTracker$lButton <- 0
eyeTracker$rButton <- 0

#then I'm going to cycle over the whole dataset to flag 1 or 0 the intersections
#
#it takes a lot of time, so I'm going to run this every time we have new participants separtely

read.csv(paste0(input,"eyeTracker_intersections.csv"), stringsAsFactors = T) -> eyeTracker_old

unique(eyeTracker_old$subjID)
unique(eyeTracker$subjID)

subset(eyeTracker, !(subjID %in% eyeTracker_old$subjID)) -> eyeTracker
droplevels(eyeTracker)-> eyeTracker
unique(eyeTracker$subjID)


for (i in 1:nrow(eyeTracker)){
  for (b in ROIs$ROI){
    print(paste0("row left: ", nrow(eyeTracker)-i,"/ROI: ",b))
    
    # define the area of the ROI
    round(ROIs[ROIs$ROI==b,]$x1, 2) -> Xmin
    round(ROIs[ROIs$ROI==b,]$x2, 2) -> Xmax
    round(ROIs[ROIs$ROI==b,]$y1, 2) -> Ymin
    round(ROIs[ROIs$ROI==b,]$y2, 2) -> Ymax
    
    # p is your point, p.x is the x coord, p.y is the y coord
    eyeTracker[i,]$x -> p.x
    eyeTracker[i,]$y -> p.y
    
    #if not within the area, give 0, otherwise 1
    eyeTracker[i,b] <- ifelse(p.x < Xmin || p.x > Xmax || p.y < Ymin || p.y > Ymax, 0,1) 

  }
}

rbind(eyeTracker_intersections,eyeTracker) -> eyeTracker
unique(eyeTracker$subjID)

#save file
write.csv(eyeTracker, paste0(input,"eyeTracker_intersections.csv"), row.names = F, quote = F)


summary(eyeTracker)

####-------------------- proportions of fixations across trials or BLOCKS by time -------------------------####
# load what we have done so far:
read.csv(paste0(input,"eyeTracker_intersections.csv"), stringsAsFactors = T) -> eyeTracker

#The column "position" stores the position in which the label has been showed 
expeType <- "pilot1"
if (expeType=="pilot1"){
  eyeTracker[eyeTracker$label=="dep",]$position <- as.factor("c")
  eyeTracker[eyeTracker$label=="tob",]$position <- as.factor("r")
  eyeTracker[eyeTracker$label=="wug",]$position <- as.factor("l")
}

table(eyeTracker$position, eyeTracker$frequency)

                         
#define the blocks
#pilot 1 and pilot2 have blocks in fixed positions and these are the same

eyeTracker$blocks <- 0

eyeTracker[eyeTracker$trial %in% sort(unique(eyeTracker$trial))[1:30],]$blocks <- 1
eyeTracker[eyeTracker$trial %in% sort(unique(eyeTracker$trial))[31:55],]$blocks <- 2
eyeTracker[eyeTracker$trial %in% sort(unique(eyeTracker$trial))[56:81],]$blocks <- 3
eyeTracker[eyeTracker$trial %in% sort(unique(eyeTracker$trial))[82:110],]$blocks <- 4
eyeTracker[eyeTracker$trial %in% sort(unique(eyeTracker$trial))[111:136],]$blocks <- 5
eyeTracker[eyeTracker$trial %in% sort(unique(eyeTracker$trial))[137:160],]$blocks <- 6 


#"y": compute proportions across trials
#"n": compute proportions across BLOCKS
avg_across_Trials <- "n" 

if (avg_across_Trials== "y"){ #"y": compute proportions across trials
  if (expeType == "pilot2"){ #pilot 2 has list in addition to pilot 1, that's why I'm splitting it
    cITI_fribble <- aggregate(cITI ~ list+ subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    lITI_fribble <- aggregate(lITI ~ list+ subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    rITI_fribble <- aggregate(rITI ~ list+ subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    cITI <- aggregate(cITI ~ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    lITI <- aggregate(lITI ~ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    rITI <- aggregate(rITI ~ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    cLabel <- aggregate(cLabel ~ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    lLabel <- aggregate(lLabel ~ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    rLabel <- aggregate(rLabel ~ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    cButton <- aggregate(cButton ~ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    lButton <- aggregate(lButton ~ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    rButton <- aggregate(rButton ~ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    
  } else {
    cITI_fribble <- aggregate(cITI ~  subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    lITI_fribble <- aggregate(lITI ~  subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    rITI_fribble <- aggregate(rITI ~  subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    cITI <- aggregate(cITI ~  subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    lITI <- aggregate(lITI ~  subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    rITI <- aggregate(rITI ~  subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    cLabel <- aggregate(cLabel ~  subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    lLabel <- aggregate(lLabel ~  subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    rLabel <- aggregate(rLabel ~  subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    cButton <- aggregate(cButton ~  subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    lButton <- aggregate(lButton ~  subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    rButton <- aggregate(rButton ~  subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    
  }
} else if (avg_across_Trials== "n"){ #compute proportions across BLOCKS
  if (expeType == "pilot2"){
    cITI_fribble <- aggregate(cITI ~ blocks+ list+ subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    lITI_fribble <- aggregate(lITI ~ blocks+ list+ subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    rITI_fribble <- aggregate(rITI ~ blocks+ list+ subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    cITI <- aggregate(cITI ~ blocks+ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    lITI <- aggregate(lITI ~ blocks+ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    rITI <- aggregate(rITI ~ blocks+ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    cLabel <- aggregate(cLabel ~ blocks+ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    lLabel <- aggregate(lLabel ~ blocks+ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    rLabel <- aggregate(rLabel ~ blocks+ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    cButton <- aggregate(cButton ~ blocks+ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    lButton <- aggregate(lButton ~ blocks+ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    rButton <- aggregate(rButton ~ blocks+ list+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    

  } else {
    cITI_fribble <- aggregate(cITI ~  blocks+ subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    lITI_fribble <- aggregate(lITI ~ blocks+ subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    rITI_fribble <- aggregate(rITI ~ blocks+ subjID + labelPresented + frequency + screen_index+ time+position, eyeTracker[eyeTracker$screen_index==2,], mean) 
    cITI <- aggregate(cITI ~ blocks+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    lITI <- aggregate(lITI ~ blocks+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    rITI <- aggregate(rITI ~ blocks+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==3,], mean) 
    cLabel <- aggregate(cLabel ~ blocks+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    lLabel <- aggregate(lLabel ~ blocks+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    rLabel <- aggregate(rLabel ~ blocks+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==4,], mean) 
    cButton <- aggregate(cButton ~ blocks+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    lButton <- aggregate(lButton ~ blocks+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    rButton <- aggregate(rButton ~ blocks+ subjID + labelPresented + frequency + screen_index + time+position, eyeTracker[eyeTracker$screen_index==5,], mean) 
    
  }
} 

if (avg_across_Trials== "y"){
  if (expeType=="pilot2"){
    #change format from wide to long:  SCREEN 3
    cITI <- merge(cITI,lITI, by=c("list","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI <- merge(cITI,rITI, by=c("list","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_long<-gather(cITI, ROI, prop, cITI:rITI, factor_key=TRUE)
    
    
    #change format from wide to long:  SCREEN 4 
    cLabel <- merge(cLabel,lLabel, by=c("list","subjID","labelPresented","frequency","time","screen_index","position"))
    cLabel <- merge(cLabel,rLabel, by=c("list","subjID","labelPresented","frequency","time","screen_index","position"))
    cLabel_long<-gather(cLabel, ROI, prop, cLabel:rLabel, factor_key=TRUE)
    
    #change format from wide to long:  SCREEN 5 
    cButton <- merge(cButton,lButton, by=c("list","subjID","labelPresented","frequency","time","screen_index","position"))
    cButton <- merge(cButton,rButton, by=c("list","subjID","labelPresented","frequency","time","screen_index","position"))
    cButton_long<-gather(cButton, ROI, prop, cButton:rButton, factor_key=TRUE)
    
    #change format from wide to long:  SCREEN 2 
    
    cITI_fribble <- merge(cITI_fribble,lITI_fribble, by=c("list","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_fribble <- merge(cITI_fribble,rITI_fribble, by=c("list","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_fribble_long<-gather(cITI_fribble, ROI, prop, cITI:rITI, factor_key=TRUE)
    
  } else {
    #change format from wide to long:  SCREEN 3 
    cITI <- merge(cITI,lITI, by=c("subjID","labelPresented","frequency","screen_index","time","position"))
    cITI <- merge(cITI,rITI, by=c("subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_long<-gather(cITI, ROI, prop, cITI:rITI, factor_key=TRUE)
    
    
    #change format from wide to long:  SCREEN 4 
    cLabel <- merge(cLabel,lLabel, by=c("subjID","labelPresented","frequency","time","screen_index","position"))
    cLabel <- merge(cLabel,rLabel, by=c("subjID","labelPresented","frequency","time","screen_index","position"))
    cLabel_long<-gather(cLabel, ROI, prop, cLabel:rLabel, factor_key=TRUE)
    
    #change format from wide to long:  SCREEN 5 
    cButton <- merge(cButton,lButton, by=c("subjID","labelPresented","frequency","time","screen_index","position"))
    cButton <- merge(cButton,rButton, by=c("subjID","labelPresented","frequency","time","screen_index","position"))
    cButton_long<-gather(cButton, ROI, prop, cButton:rButton, factor_key=TRUE)
    
    #change format from wide to long:  SCREEN 2 
    
    cITI_fribble <- merge(cITI_fribble,lITI_fribble, by=c("subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_fribble <- merge(cITI_fribble,rITI_fribble, by=c("subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_fribble_long<-gather(cITI_fribble, ROI, prop, cITI:rITI, factor_key=TRUE)
    
  }
} else if (avg_across_Trials== "n"){
  if (expeType=="pilot2"){
    #change format from wide to long:  SCREEN 3
    cITI <- merge(cITI,lITI, by=c("blocks","list","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI <- merge(cITI,rITI, by=c("blocks","list","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_long<-gather(cITI, ROI, prop, cITI:rITI, factor_key=TRUE)
    
    
    #change format from wide to long:  SCREEN 4 
    cLabel <- merge(cLabel,lLabel, by=c("blocks","list","subjID","labelPresented","frequency","time","screen_index","position"))
    cLabel <- merge(cLabel,rLabel, by=c("blocks","list","subjID","labelPresented","frequency","time","screen_index","position"))
    cLabel_long<-gather(cLabel, ROI, prop, cLabel:rLabel, factor_key=TRUE)
    
    #change format from wide to long:  SCREEN 5 
    cButton <- merge(cButton,lButton, by=c("blocks","list","subjID","labelPresented","frequency","time","screen_index","position"))
    cButton <- merge(cButton,rButton, by=c("blocks","list","subjID","labelPresented","frequency","time","screen_index","position"))
    cButton_long<-gather(cButton, ROI, prop, cButton:rButton, factor_key=TRUE)
    
    #change format from wide to long:  SCREEN 2 
    
    cITI_fribble <- merge(cITI_fribble,lITI_fribble, by=c("blocks","list","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_fribble <- merge(cITI_fribble,rITI_fribble, by=c("blocks","list","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_fribble_long<-gather(cITI_fribble, ROI, prop, cITI:rITI, factor_key=TRUE)
    
  } else {
    #change format from wide to long:  SCREEN 3 
    cITI <- merge(cITI,lITI, by=c("blocks","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI <- merge(cITI,rITI, by=c("blocks","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_long<-gather(cITI, ROI, prop, cITI:rITI, factor_key=TRUE)
    
    
    #change format from wide to long:  SCREEN 4 
    cLabel <- merge(cLabel,lLabel, by=c("blocks","subjID","labelPresented","frequency","time","screen_index","position"))
    cLabel <- merge(cLabel,rLabel, by=c("blocks","subjID","labelPresented","frequency","time","screen_index","position"))
    cLabel_long<-gather(cLabel, ROI, prop, cLabel:rLabel, factor_key=TRUE)
    
    #change format from wide to long:  SCREEN 5 
    cButton <- merge(cButton,lButton, by=c("blocks","subjID","labelPresented","frequency","time","screen_index","position"))
    cButton <- merge(cButton,rButton, by=c("blocks","subjID","labelPresented","frequency","time","screen_index","position"))
    cButton_long<-gather(cButton, ROI, prop, cButton:rButton, factor_key=TRUE)
    
    #change format from wide to long:  SCREEN 2 
    
    cITI_fribble <- merge(cITI_fribble,lITI_fribble, by=c("blocks","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_fribble <- merge(cITI_fribble,rITI_fribble, by=c("blocks","subjID","labelPresented","frequency","screen_index","time","position"))
    cITI_fribble_long<-gather(cITI_fribble, ROI, prop, cITI:rITI, factor_key=TRUE)
    
  }
}


rbind(cITI_fribble_long, cITI_long,cLabel_long,cButton_long)-> eyeTracker_propFixations
eyeTracker_propFixations$ROI <- as.factor(eyeTracker_propFixations$ROI)

#save proportion of looks
write.csv(eyeTracker_propFixations, paste0(input, "eyeTracker_proportions_longFormat_blocks.csv"), row.names = F, quote = F)


####-------------------- proportions of fixations by target vs non target ROI -------------------------####
#load proportions of looks
read.csv(paste0(input,"eyeTracker_proportions_longFormat.csv"), stringsAsFactors = T)-> prop_of_fix
read.csv(paste0(input,"eyeTracker_proportions_longFormat_blocks.csv"), stringsAsFactors = T)-> prop_of_fix

expeType <- "pilot2"

input2 <- c(paste0(localDirectory,"preProcessed_data/",expeType,"/")) 

read.csv(paste0(input2,"eyeTracker_proportions_longFormat.csv"), stringsAsFactors = T)-> prop_of_fix2
read.csv(paste0(input2,"eyeTracker_proportions_longFormat_blocks.csv"), stringsAsFactors = T)-> prop_of_fix2

prop_of_fix$expeType<- as.factor("pilot1")
prop_of_fix$list <- 1

prop_of_fix2$expeType<- as.factor("pilot2")

rbind(prop_of_fix,prop_of_fix2) -> prop_of_fix ; rm(prop_of_fix2)

# EW: 
# We need graphs where the lines represent looks to the target versus foil, 
# rather than to looks to particular parts of the screen. 
# So: focus on proportion of looks to the target versus any foil merged across labels


prop_of_fix[prop_of_fix$screen_index==4 & prop_of_fix$time <=1150,] -> screen_4
prop_of_fix[prop_of_fix$screen_index==3 & prop_of_fix$time <=300,] -> screen_3
prop_of_fix[prop_of_fix$screen_index==2 & prop_of_fix$time <=400,] -> screen_2


# ---------------- screen 4 (label screen) -----------------#
screen_4$where <- "altro"
screen_4[screen_4$ROI=="cLabel" & screen_4$position == "c",]$where <- "target"
screen_4[screen_4$ROI=="rLabel" & screen_4$position == "r",]$where <- "target"
screen_4[screen_4$ROI=="lLabel" & screen_4$position == "l",]$where <- "target"

# ---------------- screen 3 (ITI screen) -----------------#
screen_3$where <- "altro"
screen_3[screen_3$ROI=="cITI" & screen_3$position == "c",]$where <- "target"
screen_3[screen_3$ROI=="rITI" & screen_3$position == "r",]$where <- "target"
screen_3[screen_3$ROI=="lITI" & screen_3$position == "l",]$where <- "target"

# ---------------- screen 2 (fribble screen) -----------------#
screen_2$where <- "altro"
screen_2[screen_2$ROI=="cITI" & screen_2$position == "c",]$where <- "target"
screen_2[screen_2$ROI=="rITI" & screen_2$position == "r",]$where <- "target"
screen_2[screen_2$ROI=="lITI" & screen_2$position == "l",]$where <- "target"

table(as.factor(screen_3$where), screen_3$list, screen_3$expeType)

targetbyLocation <- "n"

if (targetbyLocation == "y"){
  
  # ---------------- screen 4 (label screen) -----------------#
  screen_4[screen_4$ROI=="cLabel" & screen_4$position == "c",]$where <- "target-c"
  screen_4[screen_4$ROI=="rLabel" & screen_4$position == "r",]$where <- "target-r"
  screen_4[screen_4$ROI=="lLabel" & screen_4$position == "l",]$where <- "target-l"
  
  # ---------------- screen 3 (ITI screen) -----------------#
  screen_3[screen_3$ROI=="cITI" & screen_3$position == "c",]$where <- "target-c"
  screen_3[screen_3$ROI=="rITI" & screen_3$position == "r",]$where <- "target-r"
  screen_3[screen_3$ROI=="lITI" & screen_3$position == "l",]$where <- "target-l"
  
  # ---------------- screen 2 (fribble screen) -----------------#
  screen_2[screen_2$ROI=="cITI" & screen_2$position == "c",]$where <- "target-c"
  screen_2[screen_2$ROI=="rITI" & screen_2$position == "r",]$where <- "target-r"
  screen_2[screen_2$ROI=="lITI" & screen_2$position == "l",]$where <- "target-l"
  
}
# ---------------- screen 4 (label screen) -----------------#
# ------------------------- LIST 1 -------------------------#
#
#foils - control
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="lLabel" & screen_4$position == "r" & screen_4$list==1,]$where <- "foil-l"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="cLabel" & screen_4$position == "r" & screen_4$list==1,]$where <- "foil-c"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="lLabel" & screen_4$position == "c" & screen_4$list==1,]$where <- "foil-l"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="rLabel" & screen_4$position == "c" & screen_4$list==1,]$where <- "foil-r"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="rLabel" & screen_4$position == "l" & screen_4$list==1,]$where <- "foil-r"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="cLabel" & screen_4$position == "l" & screen_4$list==1,]$where <- "foil-c"

# - target - mismatch 1
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="l" & screen_4$ROI=="rLabel"  & screen_4$list==1,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="h" & screen_4$ROI=="lLabel"  & screen_4$list==1,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="l" & screen_4$ROI=="lLabel"  & screen_4$list==1,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="h" & screen_4$ROI=="cLabel"  & screen_4$list==1,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="l" & screen_4$ROI=="cLabel"  & screen_4$list==1,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="h" & screen_4$ROI=="rLabel"  & screen_4$list==1,]$where <- "foil-mismatch1"

# - target - mismatch 2
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="l" & screen_4$ROI=="lLabel"  & screen_4$list==1,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="h" & screen_4$ROI=="rLabel"  & screen_4$list==1,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="l" & screen_4$ROI=="cLabel"  & screen_4$list==1,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="h" & screen_4$ROI=="lLabel"  & screen_4$list==1,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="l" & screen_4$ROI=="rLabel"  & screen_4$list==1,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="h" & screen_4$ROI=="cLabel"  & screen_4$list==1,]$where <- "foil-mismatch2"

# ---------------- screen 3 (ITI screen) -----------------#
#
#foils - control
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="lITI" & screen_3$position == "r" & screen_3$list==1,]$where <- "foil-l"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="cITI" & screen_3$position == "r" & screen_3$list==1,]$where <- "foil-c"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="lITI" & screen_3$position == "c" & screen_3$list==1,]$where <- "foil-l"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="rITI" & screen_3$position == "c" & screen_3$list==1,]$where <- "foil-r"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="rITI" & screen_3$position == "l" & screen_3$list==1,]$where <- "foil-r"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="cITI" & screen_3$position == "l" & screen_3$list==1,]$where <- "foil-c"

# - target - mismatch 1
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="l" & screen_3$ROI=="rITI"  & screen_3$list==1,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="h" & screen_3$ROI=="lITI"  & screen_3$list==1,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="l" & screen_3$ROI=="lITI"  & screen_3$list==1,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="h" & screen_3$ROI=="cITI"  & screen_3$list==1,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="l" & screen_3$ROI=="cITI"  & screen_3$list==1,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="h" & screen_3$ROI=="rITI"  & screen_3$list==1,]$where <- "foil-mismatch1"

# - target - mismatch 2
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="l" & screen_3$ROI=="lITI"  & screen_3$list==1,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="h" & screen_3$ROI=="rITI"  & screen_3$list==1,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="l" & screen_3$ROI=="cITI"  & screen_3$list==1,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="h" & screen_3$ROI=="lITI"  & screen_3$list==1,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="l" & screen_3$ROI=="rITI"  & screen_3$list==1,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="h" & screen_3$ROI=="cITI"  & screen_3$list==1,]$where <- "foil-mismatch2"

# ---------------- screen 2 (fribble screen) -----------------#
#
#foils - control
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="lITI" & screen_2$position == "r" & screen_2$list==1,]$where <- "foil-l"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="cITI" & screen_2$position == "r" & screen_2$list==1,]$where <- "foil-c"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="lITI" & screen_2$position == "c" & screen_2$list==1,]$where <- "foil-l"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="rITI" & screen_2$position == "c" & screen_2$list==1,]$where <- "foil-r"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="rITI" & screen_2$position == "l" & screen_2$list==1,]$where <- "foil-r"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="cITI" & screen_2$position == "l" & screen_2$list==1,]$where <- "foil-c"

#foils  - mismatch 1
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="l" & screen_2$ROI=="rITI"  & screen_2$list==1,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="h" & screen_2$ROI=="lITI"  & screen_2$list==1,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="l" & screen_2$ROI=="lITI"  & screen_2$list==1,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="h" & screen_2$ROI=="cITI"  & screen_2$list==1,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="l" & screen_2$ROI=="cITI"  & screen_2$list==1,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="h" & screen_2$ROI=="rITI"  & screen_2$list==1,]$where <- "foil-mismatch1"

#foils  - mismatch 2
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="l" & screen_2$ROI=="lITI"  & screen_2$list==1,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="h" & screen_2$ROI=="rITI"  & screen_2$list==1,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="l" & screen_2$ROI=="cITI"  & screen_2$list==1,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="h" & screen_2$ROI=="lITI"  & screen_2$list==1,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="l" & screen_2$ROI=="rITI"  & screen_2$list==1,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="h" & screen_2$ROI=="cITI"  & screen_2$list==1,]$where <- "foil-mismatch2"

# ---- LIST 2 ---- #
#
#foils - control
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="lLabel" & screen_4$position == "r" & screen_4$list==2,]$where <- "foil-l"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="cLabel" & screen_4$position == "r" & screen_4$list==2,]$where <- "foil-c"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="lLabel" & screen_4$position == "c" & screen_4$list==2,]$where <- "foil-l"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="rLabel" & screen_4$position == "c" & screen_4$list==2,]$where <- "foil-r"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="rLabel" & screen_4$position == "l" & screen_4$list==2,]$where <- "foil-r"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="cLabel" & screen_4$position == "l" & screen_4$list==2,]$where <- "foil-c"

#foils  - mismatch 1
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="l" & screen_4$ROI=="cLabel"  & screen_4$list==2,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="h" & screen_4$ROI=="rLabel"  & screen_4$list==2,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="l" & screen_4$ROI=="rLabel"  & screen_4$list==2,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="h" & screen_4$ROI=="lLabel"  & screen_4$list==2,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="l" & screen_4$ROI=="lLabel"  & screen_4$list==2,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="h" & screen_4$ROI=="cLabel"  & screen_4$list==2,]$where <- "foil-mismatch1"

#foils  - mismatch 2
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="l" & screen_4$ROI=="rLabel"  & screen_4$list==2,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="h" & screen_4$ROI=="cLabel"  & screen_4$list==2,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="l" & screen_4$ROI=="lLabel"  & screen_4$list==2,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="h" & screen_4$ROI=="rLabel"  & screen_4$list==2,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="l" & screen_4$ROI=="cLabel"  & screen_4$list==2,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="h" & screen_4$ROI=="lLabel"  & screen_4$list==2,]$where <- "foil-mismatch2"

# ---------------- screen 3 (ITI screen) -----------------#
#
#foils - control
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="lITI" & screen_3$position == "r" & screen_3$list==2,]$where <- "foil-l"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="cITI" & screen_3$position == "r" & screen_3$list==2,]$where <- "foil-c"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="lITI" & screen_3$position == "c" & screen_3$list==2,]$where <- "foil-l"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="rITI" & screen_3$position == "c" & screen_3$list==2,]$where <- "foil-r"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="rITI" & screen_3$position == "l" & screen_3$list==2,]$where <- "foil-r"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="cITI" & screen_3$position == "l" & screen_3$list==2,]$where <- "foil-c"

#foils - target - mismatch 1
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="l" & screen_3$ROI=="cITI"  & screen_3$list==2,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="h" & screen_3$ROI=="rITI"  & screen_3$list==2,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="l" & screen_3$ROI=="rITI"  & screen_3$list==2,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="h" & screen_3$ROI=="lITI"  & screen_3$list==2,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="l" & screen_3$ROI=="lITI"  & screen_3$list==2,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="h" & screen_3$ROI=="cITI"  & screen_3$list==2,]$where <- "foil-mismatch1"

#foils - target - mismatch 2
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="l" & screen_3$ROI=="rITI"  & screen_3$list==2,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="h" & screen_3$ROI=="cITI"  & screen_3$list==2,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="l" & screen_3$ROI=="lITI"  & screen_3$list==2,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="h" & screen_3$ROI=="rITI"  & screen_3$list==2,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="l" & screen_3$ROI=="cITI"  & screen_3$list==2,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="h" & screen_3$ROI=="lITI"  & screen_3$list==2,]$where <- "foil-mismatch2"

# ---------------- screen 2 (fribble screen) -----------------#
#
#foils - control
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="lITI" & screen_2$position == "r" & screen_2$list==2,]$where <- "foil-l"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="cITI" & screen_2$position == "r" & screen_2$list==2,]$where <- "foil-c"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="lITI" & screen_2$position == "c" & screen_2$list==2,]$where <- "foil-l"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="rITI" & screen_2$position == "c" & screen_2$list==2,]$where <- "foil-r"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="rITI" & screen_2$position == "l" & screen_2$list==2,]$where <- "foil-r"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="cITI" & screen_2$position == "l" & screen_2$list==2,]$where <- "foil-c"

#foils  - mismatch 1
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="l" & screen_2$ROI=="cITI"  & screen_2$list==2,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="h" & screen_2$ROI=="rITI"  & screen_2$list==2,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="l" & screen_2$ROI=="rITI"  & screen_2$list==2,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="h" & screen_2$ROI=="lITI"  & screen_2$list==2,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="l" & screen_2$ROI=="lITI"  & screen_2$list==2,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="h" & screen_2$ROI=="cITI"  & screen_2$list==2,]$where <- "foil-mismatch1"

#foils  - mismatch 2
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="l" & screen_2$ROI=="rITI"  & screen_2$list==2,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="h" & screen_2$ROI=="cITI"  & screen_2$list==2,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="l" & screen_2$ROI=="lITI"  & screen_2$list==2,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="h" & screen_2$ROI=="rITI"  & screen_2$list==2,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="l" & screen_2$ROI=="cITI"  & screen_2$list==2,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="h" & screen_2$ROI=="lITI"  & screen_2$list==2,]$where <- "foil-mismatch2"

# ---- LIST 3 ---- #
#
#foils - control
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="lLabel" & screen_4$position == "r" & screen_4$list==3,]$where <- "foil-l"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="cLabel" & screen_4$position == "r" & screen_4$list==3,]$where <- "foil-c"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="lLabel" & screen_4$position == "c" & screen_4$list==3,]$where <- "foil-l"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="rLabel" & screen_4$position == "c" & screen_4$list==3,]$where <- "foil-r"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="rLabel" & screen_4$position == "l" & screen_4$list==3,]$where <- "foil-r"
screen_4[screen_4$labelPresented == "bim" & screen_4$ROI=="cLabel" & screen_4$position == "l" & screen_4$list==3,]$where <- "foil-c"

#foils  - mismatch 1
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="l" & screen_4$ROI=="lLabel"  & screen_4$list==3,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="h" & screen_4$ROI=="cLabel"  & screen_4$list==3,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="l" & screen_4$ROI=="cLabel"  & screen_4$list==3,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="h" & screen_4$ROI=="rLabel"  & screen_4$list==3,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="l" & screen_4$ROI=="rLabel"  & screen_4$list==3,]$where <- "foil-mismatch1"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="h" & screen_4$ROI=="lLabel"  & screen_4$list==3,]$where <- "foil-mismatch1"

#foils  - mismatch 2
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="l" & screen_4$ROI=="cLabel"  & screen_4$list==3,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "dep" & screen_4$frequency=="h" & screen_4$ROI=="lLabel"  & screen_4$list==3,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="l" & screen_4$ROI=="rLabel"  & screen_4$list==3,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "tob" & screen_4$frequency=="h" & screen_4$ROI=="cLabel"  & screen_4$list==3,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="l" & screen_4$ROI=="lLabel"  & screen_4$list==3,]$where <- "foil-mismatch2"
screen_4[screen_4$labelPresented == "wug" & screen_4$frequency=="h" & screen_4$ROI=="rLabel"  & screen_4$list==3,]$where <- "foil-mismatch2"

# ---------------- screen 3 (ITI screen) -----------------#
#
#foils - control
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="lITI" & screen_3$position == "r" & screen_3$list==3,]$where <- "foil-l"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="cITI" & screen_3$position == "r" & screen_3$list==3,]$where <- "foil-c"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="lITI" & screen_3$position == "c" & screen_3$list==3,]$where <- "foil-l"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="rITI" & screen_3$position == "c" & screen_3$list==3,]$where <- "foil-r"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="rITI" & screen_3$position == "l" & screen_3$list==3,]$where <- "foil-r"
screen_3[screen_3$labelPresented == "bim" & screen_3$ROI=="cITI" & screen_3$position == "l" & screen_3$list==3,]$where <- "foil-c"

#foils  - mismatch 1
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="l" & screen_3$ROI=="lITI"  & screen_3$list==3,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="h" & screen_3$ROI=="cITI"  & screen_3$list==3,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="l" & screen_3$ROI=="cITI"  & screen_3$list==3,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="h" & screen_3$ROI=="rITI"  & screen_3$list==3,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="l" & screen_3$ROI=="rITI"  & screen_3$list==3,]$where <- "foil-mismatch1"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="h" & screen_3$ROI=="lITI"  & screen_3$list==3,]$where <- "foil-mismatch1"

#foils  - mismatch 2
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="l" & screen_3$ROI=="cITI"  & screen_3$list==3,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "dep" & screen_3$frequency=="h" & screen_3$ROI=="lITI"  & screen_3$list==3,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="l" & screen_3$ROI=="rITI"  & screen_3$list==3,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "tob" & screen_3$frequency=="h" & screen_3$ROI=="cITI"  & screen_3$list==3,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="l" & screen_3$ROI=="lITI"  & screen_3$list==3,]$where <- "foil-mismatch2"
screen_3[screen_3$labelPresented == "wug" & screen_3$frequency=="h" & screen_3$ROI=="rITI"  & screen_3$list==3,]$where <- "foil-mismatch2"

# ---------------- screen 2 (fribble screen) -----------------#
#
#foils - control
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="lITI" & screen_2$position == "r" & screen_2$list==3,]$where <- "foil-l"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="cITI" & screen_2$position == "r" & screen_2$list==3,]$where <- "foil-c"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="lITI" & screen_2$position == "c" & screen_2$list==3,]$where <- "foil-l"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="rITI" & screen_2$position == "c" & screen_2$list==3,]$where <- "foil-r"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="rITI" & screen_2$position == "l" & screen_2$list==3,]$where <- "foil-r"
screen_2[screen_2$labelPresented == "bim" & screen_2$ROI=="cITI" & screen_2$position == "l" & screen_2$list==3,]$where <- "foil-c"

#foils  - mismatch 1
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="l" & screen_2$ROI=="lITI"  & screen_2$list==3,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="h" & screen_2$ROI=="cITI"  & screen_2$list==3,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="l" & screen_2$ROI=="cITI"  & screen_2$list==3,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="h" & screen_2$ROI=="rITI"  & screen_2$list==3,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="l" & screen_2$ROI=="rITI"  & screen_2$list==3,]$where <- "foil-mismatch1"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="h" & screen_2$ROI=="lITI"  & screen_2$list==3,]$where <- "foil-mismatch1"

#foils  - mismatch 2
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="l" & screen_2$ROI=="cITI"  & screen_2$list==3,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "dep" & screen_2$frequency=="h" & screen_2$ROI=="lITI"  & screen_2$list==3,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="l" & screen_2$ROI=="rITI"  & screen_2$list==3,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "tob" & screen_2$frequency=="h" & screen_2$ROI=="cITI"  & screen_2$list==3,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="l" & screen_2$ROI=="lITI"  & screen_2$list==3,]$where <- "foil-mismatch2"
screen_2[screen_2$labelPresented == "wug" & screen_2$frequency=="h" & screen_2$ROI=="rITI"  & screen_2$list==3,]$where <- "foil-mismatch2"



table(as.factor(screen_3$where), screen_3$list )

screen_4$where <- as.factor(screen_4$where)
screen_3$where <- as.factor(screen_3$where)
screen_2$where <- as.factor(screen_2$where)


screen_3$time +max(round(screen_2$time)) -> screen_3$time
screen_4$time +max(round(screen_3$time)) -> screen_4$time

rbind(screen_2,screen_3, screen_4)-> targetLooks

targetLooks <- targetLooks %>% mutate(where=recode(where, 
                                             "foil-c"="foil",
                                             "foil-r"="foil",
                                             "foil-l"="foil"))

table(targetLooks$where, targetLooks$list, targetLooks$expeType)

write.csv(targetLooks, paste0(output, "eyetracker_bothPilots_aggByTrials.csv"), row.names = F, quote = F)
write.csv(targetLooks, paste0(output, "eyetracker_bothPilots_aggByBlocks.csv"), row.names = F, quote = F)


#### --------------- PLOT by trial or by block ------------ ####
targetLooks_minimal<- aggregate(prop ~ frequency + where + time,targetLooks, mean)
targetLooks_minimal_expeType<- aggregate(prop ~ frequency + where + time+expeType,targetLooks, mean)
targetLooks_minimal_blocks<- aggregate(prop ~ frequency + where + time+blocks,targetLooks, mean)

ggplot(targetLooks_minimalp[targetLooks_minimalp$expeType=="pilot2",], 
       aes(x= time, y = prop, color = where)) + 
  facet_wrap( blocks ~ frequency)+
  stat_summary_bin(geom = "point", fun = "mean", bins = 30) +  
  stat_summary_bin(geom = "line", fun = "mean", bins = 30) +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  geom_vline(xintercept = c(400,700,1670))+
  scale_color_manual(values=c(wes_palette("Darjeeling1", 5, type = c("discrete"))))+
  scale_x_continuous(limits = range(targetLooks_minimal$time))+
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0,0.8)) 

ggsave(paste0(output, "pilot2.jpg"), height = 4, width = 9)


ggplot(targetLooks_minimal, 
       aes(x= time, y = prop, color = where)) + 
  facet_wrap( ~ frequency)+
  stat_summary_bin(geom = "point", fun = "mean", bins = 30) +  
  stat_summary_bin(geom = "line", fun = "mean", bins = 30) +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  geom_vline(xintercept = c(400,700,1670))+
  scale_x_continuous(limits = range(targetLooks_minimal$time))+
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0,0.5)) 

ggsave(paste0(output, "pilot1_foils_1target.jpg"), height = 6, width = 13)




rois <- c("foil-r", "foil-l", "foil-c", "target-r", "target-l", "target-c")
foil <- c("foil-r", "foil-l", "foil-c")
target <- c("target-r", "target-l", "target-c")

ggplot(screen3_minimalb[screen3_minimalb$frequency=="control" & screen3_minimalb$where %in% foil,], #[screen3_minimalb$where %in% foils,]
       aes(x= time, y = prop, color = where)) + 
  #facet_wrap( ~ frequency)+
  stat_summary_bin(geom = "point", fun = "mean") +  
  stat_summary_bin(geom = "line", fun = "mean") +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  scale_x_continuous(limits = range(screen3_minimalb$time))+
  # stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  #geom_vline(xintercept = 300)+
  theme_minimal()+
  theme(legend.position="bottom")


####-------------------- learners vs non learners -------------------------####
labPic<- bind_rows(read.csv(paste0(localDirectory,"preProcessed_data/pilot1/labPic.csv"), stringsAsFactors = T),
          read.csv(paste0(localDirectory,"preProcessed_data/pilot2/labPic.csv"), stringsAsFactors = T))

contingency <- bind_rows(read.csv(paste0(localDirectory,"preProcessed_data/pilot1/contingency.csv"), stringsAsFactors = T),
                   read.csv(paste0(localDirectory,"preProcessed_data/pilot2/contingency.csv"), stringsAsFactors = T))

labPic %>%
  group_by(subjID, expeType,list) %>%
  tally()

table_labPic<-labPic %>%
  group_by(subjID, expeType, type_of_resp) %>%
  tally() %>%
  pivot_wider(names_from = type_of_resp, values_from = n, values_fill = 0)%>% 
  rowwise() %>% 
  mutate(total = sum(c_across(match:`errorControl-low`)),
         match_Preference = round((match/(match+`mismatch-type1`)),2),
         mismatch1_Preference = round((`mismatch-type1`/(`mismatch-type1`+`mismatch-type2`)),2))

table_labPic$zScore_matchPreference <- scale(table_labPic$match_Preference)
table_labPic$task <- as.factor("2AFC")

contingency %>%
  group_by(subjID, expeType, trialType) %>%
  summarise(mean = mean(resp)) %>%
  pivot_wider(names_from = trialType, values_from = mean) %>% 
  rowwise() %>% 
  mutate(diff = (match-`mismatch-type1`)) -> contingency_table


contingency_table$zScore_diff <- scale(contingency_table$diff)[,1]
contingency_table$task <- as.factor("contingency")

merge(contingency_table[,c("subjID", "expeType","diff","zScore_diff","task")], 
      table_labPic[,c("subjID", "expeType","match_Preference","zScore_matchPreference","task")], 
      by = c("subjID","expeType"), all.x = T) -> temp

summary(temp)

temp %>%
  replace_na(list(zScore_matchPreference = 0))  %>% 
  rowwise() %>%
  mutate(combined_z = round(sum(zScore_diff,zScore_matchPreference)/2,2)) %>%
  arrange(desc(combined_z)) -> temp

head(table_labPic)

write.csv(table_labPic , paste0(localDirectory,"preProcessed_data/pivot_labPic.csv"), row.names = F, quote = F, na = "")
write.csv(contingency_table , paste0(localDirectory,"preProcessed_data/pivot_contingency.csv"), row.names = F, quote = F, na = "")
write.csv(temp , paste0(localDirectory,"preProcessed_data/pivot_bothTasks.csv"), row.names = F, quote = F, na = "")

#### read pivot table about learners vs non learners ####

read.csv(paste0(localDirectory,"preProcessed_data/pivot_bothTasks.csv"), stringsAsFactors=T)-> learnersTable

learnersTable[learnersTable$diff>.01,]$subjID -> learners
learnersTable[learnersTable$diff<.01,]$subjID -> nonlearners

#### --------------- PLOT by learners or by over generalizers ------------ ####
output <- c(paste0(localDirectory,"analysis/")) 

# by trials
targetLooks_minimal_learners<- aggregate(prop ~ frequency + where + time,
                                         targetLooks[targetLooks$subjID %in% learners,], mean)
targetLooks_minimal_nonlearners<- aggregate(prop ~ frequency + where + time,
                                            targetLooks[targetLooks$subjID %in% nonlearners,], mean)


p1<-ggplot(targetLooks_minimal_learners, 
       aes(x= time, y = prop, color = where)) + 
  facet_wrap( ~ frequency)+
  stat_summary_bin(geom = "point", fun = "mean", bins = 30) +  
  stat_summary_bin(geom = "line", fun = "mean", bins = 30) +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  geom_vline(xintercept = c(400,700,1670))+
  scale_color_manual(values=c(wes_palette("Darjeeling1", 5, type = c("discrete"))))+
  scale_x_continuous(limits = range(targetLooks_minimal$time))+
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0,0.6)) 
p1

p2<-ggplot(targetLooks_minimal_nonlearners, 
           aes(x= time, y = prop, color = where)) + 
  facet_wrap( ~ frequency)+
  stat_summary_bin(geom = "point", fun = "mean", bins = 30) +  
  stat_summary_bin(geom = "line", fun = "mean", bins = 30) +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  geom_vline(xintercept = c(400,700,1670))+
  scale_color_manual(values=c(wes_palette("Darjeeling1", 5, type = c("discrete"))))+
  scale_x_continuous(limits = range(targetLooks_minimal$time))+
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0,0.6)) 
p2



ggarrange(p1,p2, nrow = 2,common.legend =T, legend="bottom")->p3
p3

ggsave(paste0(output, "learners_and_nonlearners_byTrials.jpg"),height = 9, width = 15)

#by blocks
targetLooks_minimal_blocks<- aggregate(prop ~ frequency + where + time+blocks,targetLooks, mean)
targetLooks_minimal_learners_blocks<- aggregate(prop ~ frequency + where + time+blocks,
                                         targetLooks[targetLooks$subjID %in% learners,], mean)
targetLooks_minimal_nonlearners_blocks<- aggregate(prop ~ frequency + where + time+blocks,
                                            targetLooks[targetLooks$subjID %in% nonlearners,], mean)


ggplot(targetLooks_minimal_nonlearners_blocks[targetLooks_minimal_nonlearners_blocks$frequency=="l",], 
       aes(x= time, y = prop, color = where)) + 
  facet_wrap( ~ blocks)+
  stat_summary_bin(geom = "point", fun = "mean", bins = 30) +  
  stat_summary_bin(geom = "line", fun = "mean", bins = 30) +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  geom_vline(xintercept = c(400,700,1670))+
  scale_color_manual(values=c(wes_palette("Darjeeling1", 5, type = c("discrete"))))+
  scale_x_continuous(limits = range(targetLooks_minimal$time))+
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0,0.6)) +
  ggtitle("low frequency ~ blocks")

ggsave(paste0(output, "overGener_l_byBlocks.jpg"),height = 9, width = 15)


p1<-ggplot(targetLooks_minimal_learners_blocks[targetLooks_minimal_learners_blocks$frequency=="control",], 
           aes(x= time, y = prop, color = where)) + 
  facet_wrap( ~ frequency)+
  stat_summary_bin(geom = "point", fun = "mean", bins = 30) +  
  stat_summary_bin(geom = "line", fun = "mean", bins = 30) +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  geom_vline(xintercept = c(400,700,1670))+
  scale_color_manual(values=c(wes_palette("Darjeeling1", 5, type = c("discrete"))))+
  scale_x_continuous(limits = range(targetLooks_minimal$time))+
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0,0.6)) 
p1

p2<-ggplot(targetLooks_minimal_nonlearners_blocks, 
           aes(x= time, y = prop, color = where)) + 
  facet_wrap( ~ frequency)+
  stat_summary_bin(geom = "point", fun = "mean", bins = 30) +  
  stat_summary_bin(geom = "line", fun = "mean", bins = 30) +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  geom_vline(xintercept = c(400,700,1670))+
  scale_color_manual(values=c(wes_palette("Darjeeling1", 5, type = c("discrete"))))+
  scale_x_continuous(limits = range(targetLooks_minimal$time))+
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0,0.6)) 
p2


ggsave(paste0(output, "learners_and_nonlearners_byBlocks.jpg"),height = 9, width = 15)
