library(tidyverse);
library(ggplot2)
library(ggpubr)
library(lme4)
rm(list = ls())

#### set local directory ####
#Set your local working directory. This should be (and is assumed to be in the rest of the code) the highest point in your local folder:

localGitDir <- 'C:/Users/eva_v/Documents/GitHub/leverhulmeNDL'

#### load stimuli set ####
fribbleSet<-readxl::read_xlsx(paste(localGitDir, "/exp2/stimuli/testing.xlsx", sep = ""),
                       sheet = 1)


fribbleSet_conJudg<-readxl::read_xlsx(paste(localGitDir, "/exp2/stimuli/testing.xlsx", sep = ""),
                                        sheet = 3)

fribbleSet_learning<-read.csv(paste(localGitDir, "/exp2/stimuli/learning.csv", sep = ""), stringsAsFactors = T)

#### load data ####
df <- list.files(paste(localGitDir, "/exp2/data/", sep = "")); 

for (i in 1:length(df)){
  gsub(".csv$", "", df[i]) -> id
  assign(id, data.frame())
  read.csv(paste(localGitDir, "/exp2/data/", df[i], sep = ""),
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

rbind(`data_exp_15519-v20_task-19t9`, `data_exp_15519-v20_task-mmu4`,
      `data_exp_15519-v21_task-19t9`, `data_exp_15519-v21_task-mmu4`,
      `data_exp_15519-v22_task-19t9`, `data_exp_15519-v22_task-mmu4`,
      `data_exp_15519-v23_task-19t9`, `data_exp_15519-v23_task-mmu4`,
      `data_exp_15519-v24_task-19t9`, `data_exp_15519-v24_task-mmu4`,
      `data_exp_15519-v25_task-19t9`, `data_exp_15519-v25_task-mmu4`,
      `data_exp_15519-v26_task-19t9`, `data_exp_15519-v26_task-mmu4`)->dataFLO

rm(`data_exp_15519-v20_task-19t9`, `data_exp_15519-v20_task-mmu4`, `data_exp_15519-v20_questionnaire-8f8c`, `data_exp_15519-v20_questionnaire-mkjr`,
   `data_exp_15519-v21_task-19t9`, `data_exp_15519-v21_task-mmu4`, `data_exp_15519-v21_questionnaire-8f8c`, `data_exp_15519-v21_questionnaire-mkjr`,
   `data_exp_15519-v22_task-19t9`, `data_exp_15519-v22_task-mmu4`, `data_exp_15519-v22_questionnaire-8f8c`, `data_exp_15519-v22_questionnaire-mkjr`,
   `data_exp_15519-v23_task-19t9`, `data_exp_15519-v23_task-mmu4`, `data_exp_15519-v23_questionnaire-8f8c`, `data_exp_15519-v23_questionnaire-mkjr`,
   `data_exp_15519-v24_task-19t9`, `data_exp_15519-v24_task-mmu4`, `data_exp_15519-v24_questionnaire-8f8c`, `data_exp_15519-v24_questionnaire-mkjr`,
   `data_exp_15519-v25_task-19t9`, `data_exp_15519-v25_task-mmu4`, `data_exp_15519-v25_questionnaire-8f8c`, `data_exp_15519-v25_questionnaire-mkjr`,
   `data_exp_15519-v26_task-19t9`, `data_exp_15519-v26_task-mmu4`, `data_exp_15519-v26_questionnaire-8f8c`, `data_exp_15519-v26_questionnaire-mkjr`)


#### columns and rows selection ####
raw_data<- dataFLO[c('Participant.Private.ID', 'learningType', 'Test.Part' , 
                      'presentedImage', 'presentedLabel', 'Reaction.Time', "Key.Press",
                      'Trial.Type', 'Trial.Index', 'Correct')]

rowsIwantTokeep <- c("learningBlock1", "learningBlock2","labelPictures", "pictureLabels",
                     "conJudg")

raw_data <- raw_data %>% 
  filter(Test.Part %in% rowsIwantTokeep ) %>%
  rename(subjID = Participant.Private.ID, 
         learning = learningType,
         task = Test.Part, 
         fribbleID = presentedImage,
         label = presentedLabel, 
         rt = Reaction.Time, 
         resp = Key.Press, 
         plugin = Trial.Type,
         trialIndex = Trial.Index,
         acc = Correct)
rm(rowsIwantTokeep, dataFLO);

length(unique(raw_data$subjID))
length(unique(raw_data[raw_data$learning=="FL",]$subjID))
length(unique(raw_data[raw_data$learning=="LF",]$subjID))

#### rows cleaning ####
as.factor(gsub("/task/90393/9/asset/", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub("/task/90393/10/asset/", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub("/task/90393/11/asset/", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub("/task/90393/12/asset/", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub("/task/90393/13/asset/", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub("/task/90393/14/asset/", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub("/task/90393/15/asset/", "", raw_data$fribbleID))-> raw_data$fribbleID

as.factor(gsub(".JPG$", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub(".jpg$", "", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub('[[:punct:]]|"', "", raw_data$fribbleID))-> raw_data$fribbleID 
as.factor(gsub("JPG", "_", raw_data$fribbleID))-> raw_data$fribbleID
as.factor(gsub("jpg", "_", raw_data$fribbleID))-> raw_data$fribbleID

as.factor(gsub("/task/90393/9/asset/", "", raw_data$resp))-> raw_data$resp
as.factor(gsub("/task/90393/10/asset/", "", raw_data$resp))-> raw_data$resp
as.factor(gsub("/task/90393/11/asset/", "", raw_data$resp))-> raw_data$resp
as.factor(gsub("/task/90393/12/asset/", "", raw_data$resp))-> raw_data$resp
as.factor(gsub("/task/90393/13/asset/", "", raw_data$resp))-> raw_data$resp
as.factor(gsub("/task/90393/14/asset/", "", raw_data$resp))-> raw_data$resp
as.factor(gsub("/task/90393/15/asset/", "", raw_data$resp))-> raw_data$resp

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

write.csv(picLab, paste(localGitDir, "/exp2/preProcessed_data/picLab.csv", sep = ""), row.names = F, quote = F)

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

write.csv(labPic, paste(localGitDir, "/exp2/preProcessed_data/labPic.csv", sep = ""), row.names = F, quote = F)

#### contingency judgment ####

conJudg <- raw_data %>%
  filter(task == 'conJudg') 
conJudg <- droplevels(conJudg);


colnames(fribbleSet_conJudg)[4]<-'fribbleID'
merge(conJudg, fribbleSet_conJudg, by = "fribbleID")->temp


as.numeric(as.character(temp$resp))
temp$resp<-as.integer(levels(temp$resp)[as.integer(temp$resp)])
conJudg <- temp
conJudg$correctFrequency<-recode(conJudg$correctFrequency, "25"="low", "75" = "high")
conJudg$correctFrequency <- as.factor(conJudg$correctFrequency)

write.csv(conJudg, paste(localGitDir, "/exp2/preProcessed_data/contingency.csv", sep = ""), row.names = F, quote = F)

