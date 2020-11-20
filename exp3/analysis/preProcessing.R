library(tidyverse);
library(ggplot2)
library(ggpubr)
library(lme4)
rm(list = ls())

#### set local directory ####
#Set your local working directory. This should be (and is assumed to be in the rest of the code) the highest point in your local folder:

localGitDir <- 'C:/Users/eva_v/Documents/GitHub/leverhulmeNDL'

#### load stimuli set ####
fribbleSet<-readxl::read_xlsx(paste(localGitDir, "/exp3/stimuli/testing.xlsx", sep = ""),
                              sheet = 1)


fribbleSet_conJudg<-readxl::read_xlsx(paste(localGitDir, "/exp3/stimuli/testing.xlsx", sep = ""),
                                      sheet = 3)

fribbleSet_learning<-read.csv(paste(localGitDir, "/exp3/stimuli/learning.csv", sep = ""), stringsAsFactors = T)

#### load data ####
df <- list.files(paste(localGitDir, "/exp3/data/", sep = "")); 

for (i in 1:length(df)){
  gsub(".csv$", "", df[i]) -> id
  assign(id, data.frame())
  read.csv(paste(localGitDir, "/exp3/data/", df[i], sep = ""),
           na.strings=c("","NA"),
           stringsAsFactors = F,
           colClasses=c("presentedLabel"="factor",
                        "presentedImage"="factor",
                        "learningType"="factor",
                        "Trial.Type"="factor",
                        "Test.Part"="factor",
                        "Key.Press"="factor"
           ))-> temp
  assign(paste0(id), temp)
};

rm(temp, df, i, id);

rbind(`data-pilot`)-> dataFLO
rm(`data-pilot`)

#### columns and rows selection ####
raw_data<- dataFLO[c('Participant.Private.ID', 'randomiser.cu6n', 'Test.Part' , 
                     'presentedImage', 'presentedLabel', 'Reaction.Time', "Key.Press",
                     'Trial.Type', 'Trial.Index', 'Correct', 'examID')]

rowsIwantTokeep <- c("learningBlock1", "learningBlock2","labelPictures", "pictureLabels",
                     "conJudg")

raw_data <- raw_data %>% 
  filter(Test.Part %in% rowsIwantTokeep ) %>%
  rename(subjID = Participant.Private.ID, 
         learning = randomiser.cu6n,
         task = Test.Part, 
         fribbleID = presentedImage,
         label = presentedLabel, 
         rt = Reaction.Time, 
         resp = Key.Press, 
         plugin = Trial.Type,
         trialIndex = Trial.Index,
         acc = Correct,
         completionCode = examID)
rm(rowsIwantTokeep, dataFLO);

length(unique(raw_data$subjID))
length(unique(raw_data[raw_data$learning=="FL",]$subjID))
length(unique(raw_data[raw_data$learning=="LF",]$subjID))

#### rows cleaning from Gorilla's info ####
as.factor(gsub("/task/129839/2/asset/", "", raw_data$fribbleID))-> raw_data$fribbleID

as.factor(gsub(".JPG$", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub(".jpg$", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub('[[:punct:]]|"', "", raw_data$fribbleID))-> raw_data$fribbleID 
as.factor(gsub("JPG", "_", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub("jpg", "_", raw_data$fribbleID))-> raw_data$fribbleID

as.factor(gsub("/task/129839/2/asset/", "", raw_data$resp))-> raw_data$resp

as.factor(gsub(".JPG$", "", raw_data$resp))-> raw_data$resp
as.factor(gsub(".jpg$", "", raw_data$resp))-> raw_data$resp
as.factor(gsub('[[:punct:]]|"', "", raw_data$label))-> raw_data$label 
as.factor(raw_data$resp) -> raw_data$resp


#### picture labels ####
picLab <- raw_data %>%
  filter(task == 'pictureLabels') 
picLab <- droplevels(picLab);


merge(picLab, fribbleSet, by = c("fribbleID"), all.x = T)->temp
temp$trialType <- as.factor(temp$trialType)
temp$body <- as.factor(temp$body)
temp$correctLabel <- as.factor(temp$correctLabel)

ifelse(temp$resp == temp$correctLabel, 1, 0)-> temp$acc

picLab <- temp
picLab$correctFrequency<-recode(picLab$correctFrequency, "25"="low", "75" = "high", "100" = "control")
picLab$correctFrequency <- as.factor(picLab$correctFrequency)

write.csv(picLab, paste(localGitDir, "/exp3/preProcessed_data/picLab.csv", sep = ""), row.names = F, quote = F)

#### label pictures ####

labPic <- raw_data %>%
  filter(task == 'labelPictures') 
labPic <- droplevels(labPic);

colnames(fribbleSet)[1]<-'resp'
fribbleSet$resp <- as.factor(fribbleSet$resp)
fribbleSet$correctLabel <- as.factor(fribbleSet$correctLabel)
fribbleSet$trialType <- as.factor(fribbleSet$trialType)

merge(labPic, fribbleSet, by = c("resp"), all.x = T)->temp
temp$correctLabel <- as.factor(temp$correctLabel)
temp$trialType <- as.factor(temp$trialType)

ifelse(temp$label == temp$correctLabel, 1, 0)-> temp$acc


labPic <- temp
labPic$correctFrequency<-recode(labPic$correctFrequency, "25"="low", "75" = "high", "100" = "control")
labPic$correctFrequency <- as.factor(labPic$correctFrequency)
labPic$correctLabel <- as.factor(labPic$correctLabel)


labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="1_2_3_7_"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="11_9_5_16_"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="14_10_6_12_"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="26_23_25_28_"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="38_45_53_27_"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="4_37_36_8_"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="46_15_40_32_"),]$trialType <- c("control")
labPic[na.omit(labPic$label=="bim" & labPic$fribbleID=="51_48_42_43_"),]$trialType <- c("control")

labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="49_47_26_56_"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="52_12_6_11_"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="52_47_51_12_"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="6_3_36_44_"),]$trialType <- c("exp")# high
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="14_54_29_21_"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="36_50_5_20_"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="40_5_51_20_"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="dep" & labPic$fribbleID=="9_21_30_51_"),]$trialType <- c("exp") #low

labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="17_18_7_13_"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="33_34_35_7_"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="36_56_52_19_"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="44_47_51_49_"),]$trialType <- c("exp")# high
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="26_22_21_20_"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="29_11_30_39_"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="31_50_26_54_"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="tob" & labPic$fribbleID=="41_14_54_50_"),]$trialType <- c("exp") #low


labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="14_53_10_37_"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="47_56_3_7_"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="51_52_13_12_"),]$trialType <- c("exp") #high
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="52_13_12_36_"),]$trialType <- c("exp")# high
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="26_48_40_22_"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="45_9_25_43_"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="5_21_29_36_"),]$trialType <- c("exp") #low
labPic[na.omit(labPic$label=="wug" & labPic$fribbleID=="5_23_48_51_"),]$trialType <- c("exp") #low
labPic$trialType <- as.factor(labPic$trialType)

labPic[labPic$fribbleID=="14_10_6_12_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="14_53_10_37_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="14_54_29_21_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="17_18_7_13_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="26_22_21_20_",]$correctFrequency <- c("low")
labPic[labPic$fribbleID=="26_23_25_28_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="26_48_40_22_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="29_11_30_39_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="31_50_26_54_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="33_34_35_7_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="36_50_5_20_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="36_56_52_19_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="38_45_53_27_",]$correctFrequency <- c("high")
labPic[labPic$fribbleID=="4_37_36_8_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="40_5_51_20_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="41_14_54_50_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="44_47_51_49_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="45_9_25_43_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="46_15_40_32_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="47_56_3_7_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="49_47_26_56_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="5_21_29_36_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="5_23_48_51_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="51_48_42_43_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="51_52_13_12_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="52_12_6_11_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="52_13_12_36_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="52_47_51_12_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="6_3_36_44_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="9_21_30_51_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="1_2_3_7_",]$correctFrequency <- c("high") 
labPic[labPic$fribbleID=="11_9_5_16_",]$correctFrequency <- c("low") 
labPic[labPic$fribbleID=="11_9_5_16_",]$correctFrequency <- c("low") 

write.csv(labPic, paste(localGitDir, "/exp3/preProcessed_data/labPic.csv", sep = ""), row.names = F, quote = F)

#### contingency judgment ####

conJudg <- raw_data %>%
  filter(task == 'conJudg') 
conJudg <- droplevels(conJudg);


colnames(fribbleSet_conJudg)[4]<-'fribbleID'
merge(conJudg, fribbleSet_conJudg, by = "fribbleID")->temp


as.numeric(as.character(temp$resp))
temp$rt <- as.numeric(temp$rt)
temp$resp<- as.integer(levels(temp$resp)[as.integer(temp$resp)])
conJudg <- temp
conJudg$correctFrequency<-recode(conJudg$correctFrequency, "25"="low", "75" = "high")
conJudg$correctFrequency <- as.factor(conJudg$correctFrequency)

write.csv(conJudg, paste(localGitDir, "/exp3/preProcessed_data/contingency.csv", sep = ""), row.names = F, quote = F)

# convert to factor everything that is labeled as "character"
picLab[sapply(picLab, is.character)] <- 
  lapply(picLab[sapply(picLab, is.character)], as.factor)

labPic[sapply(labPic, is.character)] <- 
  lapply(labPic[sapply(labPic, is.character)], as.factor)

conJudg[sapply(conJudg, is.character)] <- 
  lapply(conJudg[sapply(conJudg, is.character)], as.factor)

summary(picLab)
summary(labPic)
summary(conJudg)
