library(tidyverse);
library(ggplot2)
library(ggpubr)
library(lme4)
library(lmerTest)

rm(list = ls())

#### set local directory ####
#Set your local working directory. This should be (and is assumed to be in the rest of the code) the highest point in your local folder:

localGitDir <- 'C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/fribbles'


#### load functions from the lab repo ####
source(paste(localGitDir, "/tools/loadFunctionsGithub.R", sep = "")) 
urlFolder <- 'https://api.github.com/repos/n400peanuts/languagelearninglab/git/trees/master?recursive=1'
urlRaw <- 'https://raw.githubusercontent.com/n400peanuts/languagelearninglab/master/tools/'

listFunctions <- c( "inverse_log_odd.R", "myCenter.R", 
                    "lizCenter.R", "getmode.R", "lizCenter2.R", "deleteRandomRows.R", "Bf.R",
                    "adjust_intercept_model.R")

loadFunctionsGithub(urlFolder, urlRaw, listFunctions)


#### 1) load data ####
#all tasks have been cleaned from Gorilla's https info and are ready to be loaded 
df <- list.files(paste(localGitDir, "/exp3_prolific/preProcessed_data/", sep = "")); 
df<- c(df[df=="contingency_prolific.csv"], df[df=="labPic_prolific.csv"], df[df=="picLab_prolific.csv"])

for (i in 1:length(df)){
  gsub(".csv$", "", df[i]) -> id #remove .csv
  assign(id, data.frame()) #load into the environment
  read.csv(paste(localGitDir, "/exp3_prolific/preProcessed_data/", df[i], sep = ""),
           na.strings=c("","NA"),
           stringsAsFactors = T,
           colClasses=c("label"="factor",
                        "correctLabel"="factor",
                        "learning"="factor"
           ))-> temp
  assign(paste0(id), temp)
}; rm(temp, df, i, id);

#labPic: 2AFC- 4 pictures test
#picLab: 2AFC- 4 labels test
#contingency: contingency judgment task, 1 label and 1 picture to rate from -100 to +100 

#### 2) exclude participants ####
#Exclude participants that score less than 80% in the control category (blue bims)
listSubj.labPic_prolific <-aggregate(acc ~ subjID, labPic_prolific[labPic_prolific$label=='bim',] ,mean)
badSubj <-unique(listSubj.labPic_prolific[listSubj.labPic_prolific$acc<.8,]$subjID); 

listSubj.picLab_prolific <-aggregate(acc ~ subjID, picLab_prolific[picLab_prolific$trialType=='control',] ,mean)
badSubj2 <-unique(listSubj.picLab_prolific[listSubj.picLab_prolific$acc<.8,]$subjID); 

#How many participants have been excluded in total?
round((length(badSubj)+length(badSubj2)) / (nrow(listSubj.labPic_prolific)+nrow(listSubj.picLab_prolific)) *100, 1)


labPic_prolific.clean <- labPic_prolific[!(labPic_prolific$subjID %in% badSubj),]; 
picLab_prolific.clean <- picLab_prolific[!(picLab_prolific$subjID %in% badSubj2),]; 
contingency_prolific <-contingency_prolific[!(contingency_prolific$subjID %in% badSubj),]
contingency_prolific <-contingency_prolific[!(contingency_prolific$subjID %in% badSubj2),]

#accuracy on controls after badsubjs removal 
round(mean(na.omit(labPic_prolific.clean[labPic_prolific.clean$label=="bim",]$acc))*100,1)
round(mean(na.omit(picLab_prolific.clean[picLab_prolific.clean$trialType=="control",]$acc))*100,1)

#remove control trials
labPic_prolific.clean <- labPic_prolific.clean[labPic_prolific.clean$label!='bim',]
picLab_prolific.clean <- picLab_prolific.clean[picLab_prolific.clean$trialType!='control',]

picLab_prolific.clean<-droplevels(picLab_prolific.clean)
labPic_prolific.clean<-droplevels(labPic_prolific.clean)

#### 3) raw means ####
#total number of participants after exclusion criterion
length(unique(labPic_prolific.clean$subjID)) + length(unique(picLab_prolific.clean$subjID))

length(unique(picLab_prolific$subjID)) #initial number
length(unique(picLab_prolific.clean$subjID)) #actual number
length(unique(labPic_prolific$subjID)) #initial number
length(unique(labPic_prolific.clean$subjID)) #actual number


#how many participants per learning?
#group1
length(unique(labPic_prolific.clean[labPic_prolific.clean$learning=="FL",]$subjID))
#group2
length(unique(labPic_prolific.clean[labPic_prolific.clean$learning=="LF",]$subjID))
#group3
length(unique(picLab_prolific.clean[picLab_prolific.clean$learning=="FL",]$subjID))
#group4
length(unique(picLab_prolific.clean[picLab_prolific.clean$learning=="LF",]$subjID))

#### Ramscar et al., (2010) glmer model ####
read.table(paste(localGitDir, "/exp2/preReg/fmri.txt", sep = ""), header = T, stringsAsFactors = T)-> fmri
fmri<-fmri[!(fmri$subjID==1  & fmri$learning=="sx") &
             !(fmri$subjID==14 & fmri$learning=="sx") &
             !(fmri$subjID==22 & fmri$learning=="sx") &
             !(fmri$subjID==3 & fmri$learning=="sx") &
             !(fmri$subjID==4 & fmri$learning=="xs") &
             !(fmri$subjID==7 & fmri$learning=="xs") &
             !(fmri$subjID==10 & fmri$learning=="xs")&
             !(fmri$subjID==12 & fmri$learning=="xs"),]

fmri <- fmri[(fmri$trialType!="control"),]

#center variables, we don't have NAs so I'm using lizCenter
fmri<-lizCenter(fmri, listfname = list("learning", "frequency"))

## --------------------------------- original model ------------------------------------##
fmriglmer1_V1<-glmer(acc ~  frequency.ct*learning.ct + (frequency.ct|subjID), 
                     data = fmri, 
                     family="binomial",
                     control=glmerControl(optimizer = "bobyqa"))


round(summary(fmriglmer1_V1)$coefficients,4)

## --------------------------------- simple effects ------------------------------------##
fmriglmer1_V2<-glmer(acc ~  frequency.ct+ frequency: learning.ct + (frequency.ct|subjID), 
                     data = fmri, 
                     family="binomial",
                     control=glmerControl(optimizer = "bobyqa"))

round(summary(fmriglmer1_V2)$coefficients,4)



beta1 = summary(fmriglmer1_V1)$coeff["frequency.ct", "Estimate"]
beta2 = summary(fmriglmer1_V1)$coeff["learning.ct", "Estimate"]
beta3 = summary(fmriglmer1_V1)$coeff["frequency.ct:learning.ct", "Estimate"]
simple_effect = summary(fmriglmer1_V2)$coeff["frequencyl:learning.ct", "Estimate"]
simple_effecth = summary(fmriglmer1_V2)$coeff["frequencyh:learning.ct", "Estimate"]

beta1 # note this is negative, FREQUENCY
beta2 # LEARNING
beta3 #INTERACTION
simple_effect #LOW FREQUENCY
simple_effecth #HIGH FREQUENCY


#### 2AFC- 4 labels test: ####
#letters follow Table 1 of the preregistration analysis
#### D) frequency ####
picLab_prolific.clean$learning<- relevel(picLab_prolific.clean$learning, ref = "LF")
picLab_prolific.clean<-lizCenter(picLab_prolific.clean, listfname = list("learning", "correctFrequency"))

repFLO<-glmer(acc ~  correctFrequency.ct*learning.ct + (correctFrequency.ct|subjID), 
              data = picLab_prolific.clean, 
              family="binomial",
              control=glmerControl(optimizer = "bobyqa"))

output <- round(summary(repFLO)$coefficients,4)
output

main_effect <- output["correctFrequency.ct", "Estimate"] #that is negative
main_se <- output["correctFrequency.ct", "Std. Error"]

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta1,2), 
   tail = 1)

#### C) learning ####
main_effect <- output["learning.ct", "Estimate"] #that is negative
main_se <- output["learning.ct", "Std. Error"]

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta2,2), 
   tail = 1)

#### A) interaction ####
main_effect <- output["correctFrequency.ct:learning.ct", "Estimate"] #that is negative
main_se <- output["correctFrequency.ct:learning.ct", "Std. Error"]

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta3,2), 
   tail = 1)

#### A.1) simple effect (low frequency X learning) ####

repFLO_V2<-glmer(acc ~  correctFrequency.ct+ correctFrequency: learning.ct + (correctFrequency.ct|subjID), 
                 data = picLab_prolific.clean, 
                 family="binomial",
                 control=glmerControl(optimizer = "bobyqa"))

output_simpleEffect <- round(summary(repFLO_V2)$coefficients,4)
output_simpleEffect

main_effect <- output_simpleEffect["correctFrequencylow:learning.ct", "Estimate"] #that is negative
main_se <- output_simpleEffect["correctFrequencylow:learning.ct", "Std. Error"]


Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect,2), 
   tail = 1)

#### A.2) simple effect (high frequency X learning) ####

main_effect <- output_simpleEffect["correctFrequencyhigh:learning.ct", "Estimate"] #that is negative
main_se <- output_simpleEffect["correctFrequencyhigh:learning.ct", "Std. Error"]

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effecth,2), 
   tail = 1)


#### 2AFC- 4pictures test: ####
labPic_prolific.clean$learning<- relevel(labPic_prolific.clean$learning, ref = "LF")
labPic_prolific.clean<-lizCenter(labPic_prolific.clean, listfname = list("learning", "correctFrequency"))

repFLO<-glmer(acc ~  correctFrequency.ct*learning.ct + (correctFrequency.ct|subjID), 
              data = labPic_prolific.clean, 
              family="binomial",
              control=glmerControl(optimizer = "bobyqa"))

output <- round(summary(repFLO)$coefficients,4)
output 

#### D) frequency ####
main_effect <- output["correctFrequency.ct", "Estimate"] #that is negative
main_se <- output["correctFrequency.ct", "Std. Error"]

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta1,2), 
   tail = 1)

#### D) learning ####
main_effect <- output["learning.ct", "Estimate"] #that is negative
main_se <- output["learning.ct", "Std. Error"]

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta2,2), 
   tail = 1)

#### A) interaction ####
main_effect <- output["correctFrequency.ct:learning.ct", "Estimate"] #that is negative
main_se <- output["correctFrequency.ct:learning.ct", "Std. Error"]

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta3,2), 
   tail = 1)

#### A.1) simple effect (low frequency X learning) ####

repFLO_V2<-glmer(acc ~  correctFrequency.ct+ correctFrequency: learning.ct  + (correctFrequency.ct|subjID), 
                 data = labPic_prolific.clean, 
                 family="binomial",
                 control=glmerControl(optimizer = "bobyqa"))

output_simpleEffect <- round(summary(repFLO_V2)$coefficients,4)
output_simpleEffect

main_effect <- output_simpleEffect["correctFrequencylow:learning.ct", "Estimate"] #that is negative
main_se <- output_simpleEffect["correctFrequencylow:learning.ct", "Std. Error"]


Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect,2), 
   tail = 1)

#### A.1) simple effect (high frequency X learning) ####
main_effect <- output_simpleEffect["correctFrequencyhigh:learning.ct", "Estimate"] #that is negative
main_se <- output_simpleEffect["correctFrequencyhigh:learning.ct", "Std. Error"]


Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effecth,2), 
   tail = 1)

#### Both 2AFC tasks together ####
rbind(labPic_prolific.clean[,c("correctFrequency","learning","acc","subjID")], picLab_prolific.clean[,c("correctFrequency","learning","acc","subjID")])->FLO_tasks

FLO_tasks$learning<- relevel(FLO_tasks$learning, ref = "LF")
FLO_tasks<-lizCenter(FLO_tasks, listfname = list("learning", "correctFrequency"))

repFLO<-glmer(acc ~  correctFrequency.ct*learning.ct + (correctFrequency.ct|subjID), 
              data = FLO_tasks, 
              family="binomial",
              control=glmerControl(optimizer = "bobyqa"))

output <- round(summary(repFLO)$coefficients,4)
output 

#### D) frequency ####
main_effect <- output["correctFrequency.ct", "Estimate"] #that is negative
main_se <- output["correctFrequency.ct", "Std. Error"]

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta1,2), 
   tail = 1)

#### D) learning ####
main_effect <- output["learning.ct", "Estimate"] #that is negative
main_se <- output["learning.ct", "Std. Error"]

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta2,2), 
   tail = 1)

#### A) interaction ####
main_effect <- output["correctFrequency.ct:learning.ct", "Estimate"] #that is negative
main_se <- output["correctFrequency.ct:learning.ct", "Std. Error"]

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta3,2), 
   tail = 1)

#### A.1) simple effect (low frequency X learning) ####

repFLO_V2<-glmer(acc ~  correctFrequency.ct+ correctFrequency: learning.ct  + (correctFrequency.ct|subjID), 
                 data = FLO_tasks, 
                 family="binomial",
                 control=glmerControl(optimizer = "bobyqa"))

output_simpleEffect <- round(summary(repFLO_V2)$coefficients,4)
output_simpleEffect

#### A.2) simple effect (low frequency X learning) ####

main_effect <- output_simpleEffect["correctFrequencylow:learning.ct", "Estimate"] #that is negative
main_se <- output_simpleEffect["correctFrequencylow:learning.ct", "Std. Error"]

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect,2), 
   tail = 1)

#### A.2) simple effect (high frequency X learning) ####

main_effect <- output_simpleEffect["correctFrequencyhigh:learning.ct", "Estimate"] #that is negative
main_se <- output_simpleEffect["correctFrequencyhigh:learning.ct", "Std. Error"]


Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effecth,2), 
   tail = 1)



#### Plot FLO tasks ####
p1<-ggpubr::ggbarplot(labPic_prolific.clean, x = "learning", y = "acc",
                  add = c("mean_se"),
                  facet.by = c("correctFrequency"),
                  title = "2AFC - 4pictures")+ geom_hline(yintercept = .33, linetype=2)

p2<-ggpubr::ggbarplot(picLab_prolific.clean, x = "learning", y = "acc",
                  add = c("mean_se"),
                  facet.by = c("correctFrequency"),
                  title = "2AFC - 4labels")+ geom_hline(yintercept = .33, linetype=2)

p3<-ggpubr::ggbarplot(FLO_tasks, x = "learning", y = "acc",
                      add = c("mean_se"),
                      facet.by = c("correctFrequency"),
                      title = "2AFC - together")+ geom_hline(yintercept = .33, linetype=2)



ggarrange(p1,p2,p3)->p4
p4


ggsave( "C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/fribbles/exp3_prolific/figures/floReplication.png", width =7, height = 6)


group_by(FLO_tasks, learning, correctFrequency) %>%
   summarise(
      count = n(),
      mean = mean(acc, na.rm = TRUE),
      sd = sd(acc, na.rm = TRUE)
   )

group_by(labPic_prolific.clean, learning, correctFrequency) %>%
   summarise(
      count = n(),
      mean = mean(acc, na.rm = TRUE),
      sd = sd(acc, na.rm = TRUE)
   )

group_by(picLab_prolific.clean, learning, correctFrequency) %>%
   summarise(
      count = n(),
      mean = mean(acc, na.rm = TRUE),
      sd = sd(acc, na.rm = TRUE)
   )
rm(p1,p2,p3)

df <- aggregate(acc ~ subjID+correctFrequency+learning, 
                data = picLab_prolific.clean, mean)

fd <- aggregate(acc ~ subjID+correctFrequency+learning, 
                data = labPic_prolific.clean, mean)

plot1<-ggplot(df, aes(x=correctFrequency, y=acc, fill = learning)) + 
   geom_boxplot(alpha=0.6)+
   geom_point(position = position_jitterdodge())  +
   geom_hline(yintercept = .33, linetype=2)+
   ggtitle('2AFC - 4 labels')+
   coord_cartesian(ylim = c(0, 1))+
   theme_classic()

plot2<-ggplot(fd, aes(x=correctFrequency, y=acc, fill = learning)) + 
   geom_boxplot(alpha=0.6)+
   geom_point(position = position_jitterdodge())  +
   geom_hline(yintercept = .33, linetype=2)+
   ggtitle('2AFC - 4 pictures')+
   coord_cartesian(ylim = c(0, 1))+
   theme_classic()

zf <- aggregate(acc ~ subjID+correctFrequency+learning, 
                data = FLO_tasks, mean)
plot3<-ggplot(zf, aes(x=correctFrequency, y=acc, fill = learning)) + 
   geom_boxplot(alpha=0.6)+
   geom_point(position = position_jitterdodge())  +
   geom_hline(yintercept = .33, linetype=2)+
   ggtitle('2AFC - together')+
   coord_cartesian(ylim = c(0, 1))+
   theme_classic()

ggarrange(plot2, plot1, plot3)-> plot4
plot4

ggsave( "C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/fribbles/exp3_prolific/figures/expe3_prolific_violins.png", width =8, height = 5)




#### contingency_prolific judgment ####
#from our pilot data:

main_effect_type <- 53.78
main_effect_freq_by_type <- 83.618
simple_effect_learn_by_freq_high_match <- 16
simple_effect_learn_by_freq_low_match <- -15
simple_effect_learn_by_freq_high_mismatch <- 15.477
simple_effect_learn_by_freq_low_mismatch <- 17.27

contingency_prolific$trialType<- as.factor(contingency_prolific$trialType)
contingency_prolific$frequency<- as.factor(contingency_prolific$frequency)
contingency_prolific$subjID<- as.factor(contingency_prolific$subjID)
contingency_prolific$fribbleID<- as.factor(contingency_prolific$fribbleID)

relevel(contingency_prolific$trialType, ref = "mismatch-type1")->contingency_prolific$trialType
relevel(contingency_prolific$learning, ref = "LF")->contingency_prolific$learning
relevel(contingency_prolific$frequency, ref = "l")->contingency_prolific$frequency

contingency_prolific<-lizCenter(contingency_prolific, listfname = list("learning", "frequency", "trialType"))

lm1<- lmerTest::lmer(resp ~  frequency:trialType:learning + frequency.ct * trialType.ct  + (frequency.ct|subjID), data = contingency_prolific)
car::Anova(lm1)
output<-round(summary(lm1)$coefficients,4) 
output

#note that if I simplify the model with (1|subjID) it doesn't tell me that is singular, and results do not change
lm1<- lmerTest::lmer(resp ~  frequency:trialType:learning + frequency.ct * trialType.ct  + (1|subjID)  , data = contingency_prolific)

#### D) type ####
main_effect <- output["trialType.ct", "Estimate"] 
main_se <- output["trialType.ct", "Std. Error"] 

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(main_effect_type,2), 
   tail = 1)

#### E) frequency by type ####
main_effect <- output["frequency.ct:trialType.ct", "Estimate"] 
main_se <- output["frequency.ct:trialType.ct", "Std. Error"] 

Bf(main_se, (main_effect), 
   likelihood = "normal",  
   modeloftheory = "normal",
   modeoftheory = 0, 
   scaleoftheory = round(main_effect_freq_by_type,2), 
   tail = 1)


#### C1) Simple effect of learning for frequency high - match ####
main_effect <- output["frequencyh:trialTypematch:learningLF", "Estimate"] 
main_se <- output["frequencyh:trialTypematch:learningLF", "Std. Error"] 

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect_learn_by_freq_high_match,2), 
   tail = 1)

#### C2) Simple effect of learning for frequency low - match ####
main_effect <- output["frequencyl:trialTypematch:learningLF", "Estimate"] 
main_se <- output["frequencyl:trialTypematch:learningLF", "Std. Error"] 

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect_learn_by_freq_low_match,2), 
   tail = 1)

#### C3) Simple effect of learning for frequency high - mismatch ####
main_effect <- output["frequencyh:trialTypemismatch-type1:learningLF", "Estimate"] 
main_se <- output["frequencyh:trialTypemismatch-type1:learningLF", "Std. Error"] 

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect_learn_by_freq_high_mismatch,2), 
   tail = 1)

#### C4) Simple effect of learning for frequency low - mismatch ####
main_effect <- output["frequencyl:trialTypemismatch-type1:learningLF", "Estimate"] 
main_se <- output["frequencyl:trialTypemismatch-type1:learningLF", "Std. Error"] 

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect_learn_by_freq_low_mismatch,2), 
   tail = 1)



#### plot contingency_prolific task####

humanWeight <- aggregate(resp ~ learning + frequency + trialType, data = contingency_prolific,FUN = mean)

barplot_humanWeights<- ggbarplot(humanWeight, x = "learning",
                                 color = "black",
                                 #fill = "learning",
                                 y = "resp",
                                 facet.by = c("frequency","trialType"),
                                 ylab = "association strength",
                                 position = position_dodge(.8),
                                 palette = c("#bdbdbd","#636363"),
                                 title = "human performance") +
  theme(legend.position = "none")+ 
  geom_hline(yintercept = 0, col='black', lwd=.6, linetype="dashed")
barplot_humanWeights

ggsave("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/fribbles/exp3_prolific/figures/expe3_prolific_contingencyTask.png", width =8, height = 5)
#### exploratory analysis: ####
#### 3) Is the control condition able to predict performance in low/high freq? ####

listSubj <- aggregate(acc ~ subjID, picLab_prolific[picLab_prolific$correctFrequency=="control",] ,mean)
colnames(listSubj)[2] <- c("acc.control")
listSubj2 <- aggregate(acc ~ subjID, picLab_prolific[picLab_prolific$correctFrequency=="low",] ,mean)
colnames(listSubj2)[2] <- c("acc.low")
listSubj3 <- aggregate(acc ~ subjID, picLab_prolific[picLab_prolific$correctFrequency=="high",] ,mean)
colnames(listSubj3)[2] <- c("acc.high")

merge(listSubj, listSubj2, by = c("subjID"))-> picLab_corrLOW
merge(listSubj, listSubj3, by = c("subjID"))-> picLab_corrHIGH
cor.test(picLab_corrLOW$acc.control, picLab_corrLOW$acc.low)
cor.test(picLab_corrHIGH$acc.control, picLab_corrHIGH$acc.high)


par(mfrow = c(1, 2))
plot(picLab_corrLOW$acc.control, 
     picLab_corrLOW$acc.low, xlab = "accuracy control category", 
     ylab = "accuracy in low frequency", 
     main = "2AFC - 4labels task \n Pearson r: .10"); 
plot(picLab_corrHIGH$acc.control, 
     picLab_corrHIGH$acc.high, 
     xlab = "accuracy control category", 
     ylab = "accuracy in high frequency", 
     main = "2AFC - 4labels task \n Pearson r: .46"); 
par(mfrow = c(1, 1))


listSubj <- aggregate(acc ~ subjID+learning, labPic_prolific[labPic_prolific$label=="bim",] ,mean)
colnames(listSubj)[3] <- c("acc.control")
listSubj2 <- aggregate(acc ~ subjID+learning, labPic_prolific[labPic_prolific$correctFrequency=="low",] ,mean)
colnames(listSubj2)[3] <- c("acc.low")
listSubj3 <- aggregate(acc ~ subjID+learning, labPic_prolific[labPic_prolific$correctFrequency=="high",] ,mean)
colnames(listSubj3)[3] <- c("acc.high")

merge(listSubj, listSubj2, by = c("subjID","learning"))-> labPic_corrLOW
merge(listSubj, listSubj3, by = c("subjID","learning"))-> labPic_corrHIGH
cor.test(labPic_corrLOW$acc.control, labPic_corrLOW$acc.low)
cor.test(labPic_corrHIGH$acc.control, labPic_corrHIGH$acc.high)


par(mfrow = c(1, 2))
plot(labPic_corrLOW$acc.control, 
     labPic_corrLOW$acc.low, xlab = "accuracy control category", 
     ylab = "accuracy in low frequency", 
     main = "2AFC - 4pictures task \n Pearson r: .34"); 
plot(labPic_corrHIGH$acc.control, 
     labPic_corrHIGH$acc.high, 
     xlab = "accuracy control category", 
     ylab = "accuracy in high frequency", 
     main = "2AFC - 4pictures task \n Pearson r: .65"); 
par(mfrow = c(1, 1))

cor.test(labPic_corrLOW[labPic_corrLOW$learning=="FL",]$acc.control, 
         labPic_corrLOW[labPic_corrLOW$learning=="FL",]$acc.low)
cor.test(labPic_corrLOW[labPic_corrLOW$learning=="LF",]$acc.control, 
         labPic_corrLOW[labPic_corrLOW$learning=="LF",]$acc.low)

cor.test(labPic_corrHIGH[labPic_corrHIGH$learning=="FL",]$acc.control, 
         labPic_corrHIGH[labPic_corrHIGH$learning=="FL",]$acc.high)
cor.test(labPic_corrHIGH[labPic_corrHIGH$learning=="LF",]$acc.control, 
         labPic_corrHIGH[labPic_corrHIGH$learning=="LF",]$acc.high)

par(mfrow = c(2, 2))
plot(labPic_corrLOW[labPic_corrLOW$learning=="FL",]$acc.control, 
     labPic_corrLOW[labPic_corrLOW$learning=="FL",]$acc.low, xlab = "accuracy control category", 
     ylab = "accuracy in low frequency", 
     main = "4pictures - FL \n Pearson r: .06"); 

plot(labPic_corrLOW[labPic_corrLOW$learning=="LF",]$acc.control, 
     labPic_corrLOW[labPic_corrLOW$learning=="LF",]$acc.low, xlab = "accuracy control category", 
     ylab = "accuracy in low frequency", 
     main = "4pictures - LF \n Pearson r: .46"); 

plot(labPic_corrHIGH[labPic_corrHIGH$learning=="FL",]$acc.control, 
     labPic_corrHIGH[labPic_corrHIGH$learning=="FL",]$acc.high, 
     xlab = "accuracy control category", 
     ylab = "accuracy in high frequency", 
     main = "4pictures - FL \n Pearson r: .66"); 

plot(labPic_corrHIGH[labPic_corrHIGH$learning=="LF",]$acc.control, 
     labPic_corrHIGH[labPic_corrHIGH$learning=="LF",]$acc.high, 
     xlab = "accuracy control category", 
     ylab = "accuracy in high frequency", 
     main = "4pictures - LF \n Pearson r: .68"); 
par(mfrow = c(1, 1))

#### Performance across test trials ####
rbind(labPic_prolific.clean[,c("correctFrequency","trialType","trial_index","learning","acc","subjID")], picLab_prolific.clean[,c("correctFrequency","trialType","trial_index","learning","acc","subjID")])->FLO_tasks
df <- aggregate(acc ~ subjID + trial_index, FLO_tasks, mean)
df$bins <- cut(df$trial_index,breaks = 4)

ggplot(df, aes(x = bins, y = acc)) +
   stat_summary_bin(fun = "mean", geom="point", bins=4 - 1)+
   ylim(0,1) +
   stat_summary_bin(aes(x = bins, y = acc, ),
                    fun.data = mean_se,
                    bins= 4-1, size= 0.4) 

first_quarter<-FLO_tasks %>%
   filter(trial_index %in% c(490:518)) %>%
   group_by(learning, correctFrequency, trialType) %>%
   summarise(mean=mean(acc, na.rm = T), 
             sd=sd(acc, na.rm = T),
             count = n()) %>%
   mutate(se = sd / sqrt(count),
          lower_ci = lower_ci(mean, se, count),
          upper_ci = upper_ci(mean, se, count))


second_quarter<-FLO_tasks %>%
   filter(trial_index %in% c(518:545)) %>%
   group_by(learning, correctFrequency, trialType) %>%
   summarise(mean=mean(acc, na.rm = T), 
             sd=sd(acc, na.rm = T),
             count = n()) %>%
   mutate(se = sd / sqrt(count),
          lower_ci = lower_ci(mean, se, count),
          upper_ci = upper_ci(mean, se, count))

third_quarter<-FLO_tasks %>%
   filter(trial_index %in% c(545:572)) %>%
   group_by(learning, correctFrequency, trialType) %>%
   summarise(mean=mean(acc, na.rm = T), 
             sd=sd(acc, na.rm = T),
             count = n()) %>%
   mutate(se = sd / sqrt(count),
          lower_ci = lower_ci(mean, se, count),
          upper_ci = upper_ci(mean, se, count))

fourth_quarter<-FLO_tasks %>%
   filter(trial_index %in% c(572:600)) %>%
   group_by(learning, correctFrequency, trialType) %>%
   summarise(mean=mean(acc, na.rm = T), 
             sd=sd(acc, na.rm = T),
             count = n()) %>%
   mutate(se = sd / sqrt(count),
          lower_ci = lower_ci(mean, se, count),
          upper_ci = upper_ci(mean, se, count))

first_quarter$test_n <-1
second_quarter$test_n <-2
third_quarter$test_n <-3
fourth_quarter$test_n <-4

rbind(first_quarter, second_quarter, third_quarter, fourth_quarter)-> quarters


ggplot(quarters, aes(x = correctFrequency, y = mean, colour = learning)) +
 geom_pointrange(aes(ymin = lower_ci, ymax = upper_ci), position = position_jitterdodge())+
   facet_wrap(~test_n, nrow = 1, ncol = 4) +
   ylim(0,1) 








lower_ci <- function(mean, se, n, conf_level = 0.95){
   lower_ci <- mean - qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
upper_ci <- function(mean, se, n, conf_level = 0.95){
   upper_ci <- mean + qt(1 - ((1 - conf_level) / 2), n - 1) * se
}
