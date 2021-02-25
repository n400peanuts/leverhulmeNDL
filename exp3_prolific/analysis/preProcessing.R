library(tidyverse);
library(ggplot2)
library(ggpubr)
library(lme4)
rm(list = ls())

#### set local directory ####
#Set your local working directory. This should be (and is assumed to be in the rest of the code) the highest point in your local folder:

localGitDir <- 'C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/fribbles'

#### load stimuli set ####
fribbleSet<-readxl::read_xlsx(paste(localGitDir, "/exp3_prolific/stimuli/testing.xlsx", sep = ""),
                              sheet = 1)


fribbleSet_conJudg<-readxl::read_xlsx(paste(localGitDir, "/exp3_prolific/stimuli/testing.xlsx", sep = ""),
                                      sheet = 3)

fribbleSet_learning<-read.csv(paste(localGitDir, "/exp3_prolific/stimuli/learning.csv", sep = ""), stringsAsFactors = T)

#### load data ####
df <- list.files(paste(localGitDir, "/exp3_prolific/data/", sep = "")); 

dataFLO <- data.frame()
for (i in 1:length(df)){
  read.csv(paste(localGitDir, "/exp3_prolific/data/", df[i], sep = "", dec = "."),
           na.strings=c("","NA"),
           stringsAsFactors = T
           )-> temp
  dataFLO <-rbind(dataFLO, temp)
};

rm(temp, df, i);

#### columns and rows selection ####
raw_data<- dataFLO[c('subject', 'trial_index', 'trial_type' ,  'rt', 'prolificID',
                     'test_part', 'key_press', 'learningType',
                     'presentedImage', 'presentedLabel', 'response')]

rowsIwantTokeep <- c("learningBlock1", "learningBlock2","labelPictures", "pictureLabels",
                     "conJudg")

raw_data <- raw_data %>% 
  filter(test_part %in% rowsIwantTokeep ) %>%
  rename(subjID = subject, 
         learning = learningType,
         task = test_part, 
         fribbleID = presentedImage,
         label = presentedLabel, 
         resp = key_press, 
         plugin = trial_type)

rm(rowsIwantTokeep, dataFLO);

length(unique(raw_data$subjID))
length(na.omit(unique(raw_data[raw_data$learning=="FL",]$subjID)))
length(na.omit(unique(raw_data[raw_data$learning=="LF",]$subjID)))


#### rows cleaning ####
as.factor(gsub("learning/", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub("test/", "", raw_data$fribbleID))-> raw_data$fribbleID

as.factor(gsub(".JPG$", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub(".jpg$", "", raw_data$fribbleID))-> raw_data$fribbleID

as.factor(gsub("test/", "", raw_data$resp))-> raw_data$resp

as.factor(gsub(".JPG$", "", raw_data$resp))-> raw_data$resp
as.factor(gsub(".jpg$", "", raw_data$resp))-> raw_data$resp
as.factor(gsub('<div style = "font-size:40px;">|"', "", raw_data$label))-> raw_data$label 
as.factor(gsub('<br><br><br></div>|"', "", raw_data$label))-> raw_data$label 
as.factor(gsub('[[:punct:]]|"', "", raw_data$label))-> raw_data$label 
as.factor(gsub('[[:punct:]]|"', "", raw_data$fribbleID))-> raw_data$fribbleID 
as.factor(gsub("JPG", "_", raw_data$fribbleID))-> raw_data$fribbleID
raw_data <- droplevels(raw_data)

#### picture labels ####
picLab <- raw_data %>%
  filter(task == 'pictureLabels') 
picLab$response <- NULL # remove this column. This is informative only in the contingency judgment task
picLab <- droplevels(picLab);

#rt are stored as character, let's convert them into integers
#will give you a warning, but check that NAs are the same number of resp's NAs
picLab$rt<-as.integer(levels(picLab$rt)[as.integer(picLab$rt)]) 

#fribbleID in fribbleSet refers to the fribble that we have presented 
merge(picLab, fribbleSet, by = c("fribbleID"), all.x = T)->temp
temp$trialType <- as.factor(temp$trialType)
temp$body <- as.factor(temp$body)
temp$correctLabel <- as.factor(temp$correctLabel)

#we code accuracy by comparing the columns resp (your response) with the column correctLabel (from the fribbleSet file that codes the choice you should have made)
ifelse(temp$resp == temp$correctLabel, 1, 0)-> temp$acc

summary(temp) #check that the file looks readable and makes sense
picLab <- temp #if so, push it into picLab variable

#recode this variables in a way that is more understandable
picLab$correctFrequency<-recode(picLab$correctFrequency, "25"="low", "75" = "high", "100" = "control")
picLab$correctFrequency <- as.factor(picLab$correctFrequency) #needs to be a factor

#aggregate(acc ~ correctFrequency, data = picLab, mean) 

#save the preprocessed file where you want
write.csv(picLab, paste(localGitDir, "/exp3_prolific/preProcessed_data/picLab_prolific.csv", sep = ""), row.names = F, quote = F)

#### label pictures ####

labPic <- raw_data %>%
  filter(task == 'labelPictures') 
labPic <- droplevels(labPic);
labPic$response <- NULL # remove this column. This is informative only in the contingency judgment task

# in this task the labels are always the same, but your choice instead depends on the fribbles that have been presented to you
# we start by renaming the column fribbleID in fribbleSet as "resp", like the column in your task file.
colnames(fribbleSet)[1]<-'resp'
fribbleSet$resp <- as.factor(fribbleSet$resp)
fribbleSet$correctLabel <- as.factor(fribbleSet$correctLabel)
fribbleSet$trialType <- as.factor(fribbleSet$trialType)

#we merge now fribbleSet and labPic by the resp column so that we have detailed info about what you have chosen in the task
merge(labPic, fribbleSet, by = c("resp"), all.x = T)->temp
temp$correctLabel <- as.factor(temp$correctLabel)
temp$trialType <- as.factor(temp$trialType)
temp$body <- as.factor(temp$body)

#now that we have the info about the fribble selected, we are going to compare 
#the column "label" that is the label that has been presented to you during the experiment
# and the column "correctLabel", that is the correct label of the fribble that you have selected
# accuracy is computed based on whether those two match.

ifelse(temp$label == temp$correctLabel, 1, 0)-> temp$acc
summary(temp) #always control that things make sense to you. There are no weird NAs, variables have the right code etc.

labPic <- temp #if satisfied push this into labPic

#recode this variables in a way that is more understandable
labPic$correctFrequency<-recode(labPic$correctFrequency, "25"="low", "75" = "high", "100" = "control")
labPic$correctFrequency <- as.factor(labPic$correctFrequency)
labPic$correctLabel <- as.factor(labPic$correctLabel)

# In the fribbleSet file there is a spreadsheet that not only is coding the type of fribble and its caracteristics
# but also in which sets of trials was presented.
# we present high and low frequency fribbles separately, meaning that an high frequency fribble
# will be always presented with high frequency fillers among which you have to choose, and viceversa.
# however, that is not the case for control trials.
# blue bims are presented 8 times, but randomly with either low or high frequency fribbles.
# This means that we can't rely on responses alone in order to understand what type of trials you chose because
# sometimes you could have chosen a blue bim.
# Anyway we need to store in trialType this info. So what we do below is 
# to reassign control, high, low frequency trials to triaType based on what type of trials fribbles where presented
# we know this info in fribbleID (x_x1_x2_x3), we know whether these were high, control or low frequency trials
# in the fribbleSet spreadsheet, and for simplicity I've added this info manually.
# I've also recoded correctFrequency based on this.


#Run this part below 
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="1_2_3_7"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="11_9_5_16"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="14_10_6_12"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="26_23_25_28"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="38_45_53_27"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="4_37_36_8"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="46_15_40_32"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="51_48_42_43"),]$trialType <- c("control")

labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="49_47_26_56"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="52_12_6_11"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="52_47_51_12"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="6_3_36_44"),]$trialType <- c("exp")# high
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="14_54_29_21"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="36_50_5_20"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="40_5_51_20"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="9_21_30_51"),]$trialType <- c("exp") #low

labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="17_18_7_13"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="33_34_35_7"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="36_56_52_19"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="44_47_51_49"),]$trialType <- c("exp")# high
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="26_22_21_20"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="29_11_30_39"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="31_50_26_54"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="41_14_54_50"),]$trialType <- c("exp") #low


labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="14_53_10_37"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="47_56_3_7"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="51_52_13_12"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="52_13_12_36"),]$trialType <- c("exp")# high
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="26_48_40_22"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="45_9_25_43"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="5_21_29_36"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="5_23_48_51"),]$trialType <- c("exp") #low
labPic$trialType <- as.factor(labPic$trialType)

labPic[labPic$fribbleID=="14_10_6_12",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="14_53_10_37",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="14_54_29_21",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="17_18_7_13",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="26_22_21_20",]$correctFrequency <- c("low")
labPic[labPic$fribbleID=="26_23_25_28",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="26_48_40_22",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="29_11_30_39",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="31_50_26_54",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="33_34_35_7",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="36_50_5_20",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="36_56_52_19",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="38_45_53_27",]$correctFrequency <- c("high")
labPic[labPic$fribbleID=="4_37_36_8",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="40_5_51_20",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="41_14_54_50",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="44_47_51_49",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="45_9_25_43",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="46_15_40_32",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="47_56_3_7",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="49_47_26_56",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="5_21_29_36",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="5_23_48_51",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="51_48_42_43",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="51_52_13_12",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="52_12_6_11",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="52_13_12_36",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="52_47_51_12",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="6_3_36_44",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="9_21_30_51",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="1_2_3_7",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="11_9_5_16",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="11_9_5_16",]$correctFrequency <- c("low") 

#aggregate(acc ~ trialType + correctFrequency, data = labPic, mean)

write.csv(labPic, paste(localGitDir, "/exp3_prolific/preProcessed_data/labPic_prolific.csv", sep = ""), row.names = F, quote = F)

#### contingency judgment ####
# we apply the same reasoning as above for contingency judgment
conJudg <- raw_data %>%
  filter(task == 'conJudg') 
conJudg <- droplevels(conJudg);


colnames(fribbleSet_conJudg)[4]<-'fribbleID'
merge(conJudg, fribbleSet_conJudg, by = "fribbleID")->temp
temp$response <- NULL
temp$fribblePresented <- as.factor(temp$fribblePresented)
conJudg <- temp

conJudg$correctFrequency <- as.factor(conJudg$correctFrequency)
conJudg$correctLabel <- as.factor(conJudg$correctLabel)
conJudg$body <- as.factor(conJudg$body)
conJudg$labelPresented <- as.factor(conJudg$labelPresented)
conJudg$trialType <- as.factor(conJudg$trialType)
conJudg$frequency <- as.factor(conJudg$frequency)

conJudg$correctFrequency<-recode(conJudg$correctFrequency, "25"="low", "75" = "high")
conJudg$rt<-as.integer(levels(conJudg$rt)[as.integer(conJudg$rt)]) 
conJudg$resp<-as.integer(levels(conJudg$resp)[as.integer(conJudg$resp)]) 
#aggregate(resp ~ trialType + correctFrequency, data = conJudg, mean)

write.csv(conJudg, paste(localGitDir, "/exp3_prolific/preProcessed_data/contingency_prolific.csv", sep = ""), row.names = F, quote = F)

