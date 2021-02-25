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


#### 1) load data prolific ####
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

#### 1.2) load data tubingen ####
df <- list.files(paste(localGitDir, "/exp3_tubingen/preProcessed_data/", sep = "")); 
df<- c(df[df=="contingency.csv"], df[df=="labPic.csv"], df[df=="picLab.csv"])

for (i in 1:length(df)){
  gsub(".csv$", "", df[i]) -> id #remove .csv
  assign(id, data.frame()) #load into the environment
  read.csv(paste(localGitDir, "/exp3_tubingen/preProcessed_data/", df[i], sep = ""),
           na.strings=c("","NA"),
           stringsAsFactors = T,
           colClasses=c("label"="factor",
                        "correctLabel"="factor",
                        "learning"="factor"
           ))-> temp
  assign(paste0(id), temp)
}; rm(temp, df, i, id);

labPic$language <- NULL
labPic$codeword <- NULL
labPic$rt <- NULL
labPic$group <- as.factor("tubingen")
labPic_prolific$rt <- NULL
labPic_prolific$prolificID <- NULL
labPic_prolific$group <- as.factor("prolific")

labPic<-rbind(labPic, labPic_prolific)

picLab$language <- NULL
picLab$codeword <- NULL
picLab$rt <- NULL
picLab$group <- as.factor("tubingen")
picLab_prolific$rt <- NULL
picLab_prolific$prolificID <- NULL
picLab_prolific$group <- as.factor("prolific")

picLab<-rbind(picLab, picLab_prolific)

contingency$language <- NULL
contingency$codeword <- NULL
contingency$rt <- NULL
contingency$group <- as.factor("tubingen")
contingency_prolific$prolificID <- NULL

contingency_prolific$rt <- NULL
contingency_prolific$group <- as.factor("prolific")
contingency<-rbind(contingency, contingency_prolific)

rm(labPic_prolific, picLab_prolific,contingency_prolific)

#### 2) exclude participants ####
#Exclude participants that score less than 80% in the control category (blue bims)
listSubj.labPic <-aggregate(acc ~ subjID+group, labPic[labPic$label=='bim',] ,mean)
badSubj <-unique(listSubj.labPic[listSubj.labPic$acc<.8,]$subjID); 

listSubj.picLab <-aggregate(acc ~ subjID+group, picLab[picLab$trialType=='control',] ,mean)
badSubj2 <-unique(listSubj.picLab[listSubj.picLab$acc<.8,]$subjID); 


group_by(listSubj.labPic, group) %>%
   filter(acc <.8) %>%
   summarise(
      count = n(),
      mean = mean(acc, na.rm = TRUE),
      sd = sd(acc, na.rm = TRUE)
   ) 
#tubingen
15 / length(listSubj.labPic[listSubj.labPic$group=="tubingen",]$subjID)*100

#PROLIFIC
19 / length(listSubj.labPic[listSubj.labPic$group=="prolific",]$subjID)*100

group_by(listSubj.picLab, group) %>%
   filter(acc <.8) %>%
   summarise(
      count = n(),
      mean = mean(acc, na.rm = TRUE),
      sd = sd(acc, na.rm = TRUE)
) 

#TUBINGEN
6 / length(listSubj.picLab[listSubj.picLab$group=="tubingen",]$subjID)*100
#PROLIFIC
13 / length(listSubj.picLab[listSubj.picLab$group=="prolific",]$subjID)*100

round((19+13) / (nrow(listSubj.labPic[listSubj.labPic$group=="prolific",])+nrow(listSubj.picLab[listSubj.picLab$group=="prolific",])) *100, 1)
round((6+15) / (nrow(listSubj.labPic[listSubj.labPic$group=="tubingen",])+nrow(listSubj.picLab[listSubj.picLab$group=="tubingen",])) *100, 1)

#How many participants have been excluded in total?
round((length(badSubj)+length(badSubj2)) / (nrow(listSubj.labPic)+nrow(listSubj.picLab)) *100, 1)

labPic.clean <- labPic[!(labPic$subjID %in% badSubj),]; 
picLab.clean <- picLab[!(picLab$subjID %in% badSubj2),]; 
contingency <-contingency[!(contingency$subjID %in% badSubj),]
contingency <-contingency[!(contingency$subjID %in% badSubj2),]

#accuracy on controls after badsubjs removal 
round(mean(na.omit(labPic.clean[labPic.clean$label=="bim",]$acc))*100,1)
round(mean(na.omit(picLab.clean[picLab.clean$trialType=="control",]$acc))*100,1)

#remove control trials
labPic.clean <- labPic.clean[labPic.clean$label!='bim',]
picLab.clean <- picLab.clean[picLab.clean$trialType!='control',]

picLab.clean<-droplevels(picLab.clean)
labPic.clean<-droplevels(labPic.clean)

#### 3) raw means ####
#total number of participants after exclusion criterion
length(unique(labPic.clean$subjID)) + length(unique(picLab.clean$subjID))

length(unique(picLab$subjID)) #initial number
length(unique(picLab.clean$subjID)) #actual number
length(unique(labPic$subjID)) #initial number
length(unique(labPic.clean$subjID)) #actual number


#how many participants per learning?
#group1
length(unique(labPic.clean[labPic.clean$learning=="FL",]$subjID))
#group2
length(unique(labPic.clean[labPic.clean$learning=="LF",]$subjID))
#group3
length(unique(picLab.clean[picLab.clean$learning=="FL",]$subjID))
#group4
length(unique(picLab.clean[picLab.clean$learning=="LF",]$subjID))

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
picLab.clean$learning<- relevel(picLab.clean$learning, ref = "LF")
picLab.clean<-lizCenter(picLab.clean, listfname = list("learning", "correctFrequency"))

repFLO<-glmer(acc ~  correctFrequency.ct*learning.ct + (correctFrequency.ct|subjID), 
              data = picLab.clean, 
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
                 data = picLab.clean, 
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
labPic.clean$learning<- relevel(labPic.clean$learning, ref = "LF")
labPic.clean<-lizCenter(labPic.clean, listfname = list("learning", "correctFrequency"))

repFLO<-glmer(acc ~  correctFrequency.ct*learning.ct + (correctFrequency.ct|subjID), 
              data = labPic.clean, 
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
                 data = labPic.clean, 
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
rbind(labPic.clean[,c("learning","correctFrequency","acc","subjID","group")], picLab.clean[,c("learning","correctFrequency","acc","subjID","group")])->FLO_tasks

FLO_tasks$learning<- relevel(FLO_tasks$learning, ref = "LF")
FLO_tasks<-lizCenter(FLO_tasks, listfname = list("learning", "correctFrequency","group"))

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

Bf(main_se, (main_effect), 
   likelihood = "normal",  
   modeloftheory = "normal", 
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
p1<-ggpubr::ggbarplot(labPic.clean, x = "learning", y = "acc",
                      add = c("mean_se"),
                      facet.by = c("correctFrequency"),
                      title = "2AFC - 4pictures")+ geom_hline(yintercept = .33, linetype=2)

p2<-ggpubr::ggbarplot(picLab.clean, x = "learning", y = "acc",
                      add = c("mean_se"),
                      facet.by = c("correctFrequency"),
                      title = "2AFC - 4labels")+ geom_hline(yintercept = .33, linetype=2)

p3<-ggpubr::ggbarplot(FLO_tasks, x = "learning", y = "acc",
                      add = c("mean_se"),
                      facet.by = c("correctFrequency"),
                      title = "2AFC - together")+ geom_hline(yintercept = .33, linetype=2)



ggarrange(p1,p2,p3)->p4
p4


ggsave( "C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/fribbles/exp3_tubingen/figures/floReplication.png", width =7, height = 6)


group_by(FLO_tasks, group, learning, correctFrequency) %>%
  summarise(
    count = n(),
    mean = mean(acc, na.rm = TRUE),
    sd = sd(acc, na.rm = TRUE)
  )

group_by(labPic.clean, group, learning, correctFrequency) %>%
  summarise(
    count = n(),
    mean = mean(acc, na.rm = TRUE),
    sd = sd(acc, na.rm = TRUE)
  )

group_by(picLab.clean, group, learning, correctFrequency) %>%
  summarise(
    count = n(),
    mean = mean(acc, na.rm = TRUE),
    sd = sd(acc, na.rm = TRUE)
  )
rm(p1,p2,p3)

df <- aggregate(acc ~ subjID+correctFrequency+learning+group, 
                data = picLab.clean, mean)

fd <- aggregate(acc ~ subjID+correctFrequency+learning+group, 
                data = labPic.clean, mean)

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

ggarrange(plot2,plot1,plot3)-> plot4
plot4
ggsave( "C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/fribbles/exp3_prolific/figures/floReplication_violins.png", width =8, height = 5)




#### contingency judgment ####
#from our pilot data:

main_effect_type <- 53.78
main_effect_freq_by_type <- 83.618
simple_effect_learn_by_freq_high_match <- 16
simple_effect_learn_by_freq_low_match <- -16
simple_effect_learn_by_freq_high_mismatch <- 16
simple_effect_learn_by_freq_low_mismatch <- 16

contingency$trialType<- as.factor(contingency$trialType)
contingency$frequency<- as.factor(contingency$frequency)
contingency$subjID<- as.factor(contingency$subjID)
contingency$fribbleID<- as.factor(contingency$fribbleID)

relevel(contingency$trialType, ref = "mismatch-type1")->contingency$trialType
relevel(contingency$learning, ref = "LF")->contingency$learning
relevel(contingency$frequency, ref = "l")->contingency$frequency

contingency<-lizCenter(contingency, listfname = list("learning", "frequency", "trialType"))

lm1<- lmerTest::lmer(resp ~  frequency:trialType:learning + frequency.ct * trialType.ct  + (frequency.ct|subjID), 
                     data = contingency)
car::Anova(lm1)
output<-round(summary(lm1)$coefficients,4) 
output

#note that if I simplify the model with (1|subjID) it doesn't tell me that is singular, and results do not change
lm1<- lmerTest::lmer(resp ~  frequency:trialType:learning + frequency.ct * trialType.ct  + (1|subjID)  , data = contingency)

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

Bf(main_se, (main_effect), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(main_effect_freq_by_type,2), 
   tail = 1)



#### C1) Simple effect of learning for frequency high - match ####
main_effect <- output["frequencyh:trialTypematch:learningLF", "Estimate"] 
main_se <- output["frequencyh:trialTypematch:learningLF", "Std. Error"] 

Bf(main_se, (main_effect), 
   likelihood = "normal",  
   modeloftheory = "normal", 
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect_learn_by_freq_high_match,2), 
   tail = 1)

#### C2) Simple effect of learning for frequency low - match ####
main_effect <- output["frequencyl:trialTypematch:learningLF", "Estimate"] 
main_se <- output["frequencyl:trialTypematch:learningLF", "Std. Error"] 

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect_learn_by_freq_low_match,2)*-1, 
   tail = 1)

#### C3) Simple effect of learning for frequency high - mismatch ####
main_effect <- output["frequencyh:trialTypemismatch-type1:learningLF", "Estimate"] 
main_se <- output["frequencyh:trialTypemismatch-type1:learningLF", "Std. Error"] 

Bf(main_se, (main_effect), 
   likelihood = "normal",  
   modeloftheory = "normal", 
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect_learn_by_freq_high_mismatch,2), 
   tail = 1)

#### C4) Simple effect of learning for frequency low - mismatch ####
main_effect <- output["frequencyl:trialTypemismatch-type1:learningLF", "Estimate"] 
main_se <- output["frequencyl:trialTypemismatch-type1:learningLF", "Std. Error"] 

Bf(main_se, (main_effect), 
   likelihood = "normal",  
   modeloftheory = "normal", 
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect_learn_by_freq_low_mismatch,2), 
   tail = 1)



#### plot contingency task####

humanWeight <- aggregate(resp ~ learning + frequency + trialType, data = contingency,FUN = mean)

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

ggsave("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/fribbles/exp3_prolific/figures/floReplication_contingencyTask.png", width =8, height = 5)

#### exploratory analysis: ####


# distribution of responses based on the high salience feature #
df<-table(picLab.clean[picLab.clean$correctFrequency=="high",]$resp, 
          picLab.clean[picLab.clean$correctFrequency=="high",]$correctLabel)
df2<-table(picLab.clean[picLab.clean$correctFrequency=="low",]$resp, 
           picLab.clean[picLab.clean$correctFrequency=="low",]$correctLabel)


stack_picLab<-data.frame(
  labelPresented = as.factor(c("dep", "dep", "dep", "dep", 
                               "tob", "tob", "tob", "tob", 
                               "wug", "wug", "wug", "wug",
                               "dep", "dep", "dep", "dep", 
                               "tob", "tob", "tob", "tob", 
                               "wug", "wug", "wug", "wug")),
  resp = as.factor(c("bim", "dep", "tob", "wug",
                     "bim", "dep", "tob", "wug",
                     "bim", "dep", "tob", "wug",
                     "bim", "dep", "tob", "wug",
                     "bim", "dep", "tob", "wug",
                     "bim", "dep", "tob", "wug")),
  bodyColor = as.factor(c("control", "lightblue", "red", "purple", #coded based on resp
                          "control", "lightblue", "red", "purple",
                          "control", "lightblue", "red", "purple",
                          "control", "red", "purple", "lightblue",
                          "control", "red", "purple", "lightblue",
                          "control", "red", "purple", "lightblue")),
  count = c(df[1], df[2], df[3], df[4],
            df[1,2], df[2,2], df[3,2], df[4,2],
            df[1,3], df[2,3], df[3,3], df[4,3],
            df2[1], df2[2], df2[3], df2[4],
            df2[1,2], df2[2,2], df2[3,2], df2[4,2],
            df2[1,3], df2[2,3], df2[3,3], df2[4,3]),
  freq = as.factor(c(rep("high", 12),
                     rep("low", 12)))
)

percentage <- NULL
for (i in 1:length(unique(stack_picLab$labelPresented))){
  scoreh <- c(round(((stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="high",]$count)[1] / sum(stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="high",]$count))*100,1),
              round(((stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="high",]$count)[2] / sum(stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="high",]$count))*100,1),
              round(((stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="high",]$count)[3] / sum(stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="high",]$count))*100,1),
              round(((stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="high",]$count)[4] / sum(stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="high",]$count))*100,1))
  
  scorel <- c(round(((stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="low",]$count)[1] / sum(stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="low",]$count))*100,1),
              round(((stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="low",]$count)[2] / sum(stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="low",]$count))*100,1),
              round(((stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="low",]$count)[3] / sum(stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="low",]$count))*100,1),
              round(((stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="low",]$count)[4] / sum(stack_picLab[stack_picLab$labelPresented==unique(stack_picLab$labelPresented)[i] & stack_picLab$freq=="low",]$count))*100,1))
  
  percentage_high <- data.frame(
    score = scoreh,
    resp = c("bim", "dep", "tob", "wug"),
    labelPresented = as.factor(unique(stack_picLab$labelPresented)[i]),
    freq = "high")
  percentage_low <- data.frame(
    score = scorel,
    resp = c("bim", "dep", "tob", "wug"),
    labelPresented = as.factor(unique(stack_picLab$labelPresented)[i]),
    freq = "low")
  
  
  percentage<- rbind(percentage, percentage_high)
  percentage <- rbind(percentage, percentage_low)
}

stack_picLab<-merge(stack_picLab, percentage, by = c("labelPresented", "resp", "freq"))

p5<-ggbarplot(stack_picLab, x = "labelPresented", y = "score",
              fill = "resp", 
              palette = c("#0099ff", "#52adc6",  "#850000", "#6733f9"), #color of the high frequency
              facet.by = "freq",
              ylab = "proportion of responses %",
              title = "2AFC - 4labels",
              xlab = "") +  
  theme_pubr() 
p5
ggsave( "C:/Users/eva_v/Documents/GitHub/leverhulmeNDL/fribbles/exp3_tubingen/figures/exploratory12.png")

