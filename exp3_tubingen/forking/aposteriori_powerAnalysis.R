library(tidyverse)
library(ggpubr)
library(lme4)

localGitDir <- 'C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/fribbles/exp2'
outputDir <- 'C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/fribbles/exp3/forking/'
#localGitDir <- ''

source(paste(localGitDir, "/tools/loadFunctionsGithub.R", sep = "")) 
urlFolder <- 'https://api.github.com/repos/n400peanuts/languagelearninglab/git/trees/master?recursive=1'
urlRaw <- 'https://raw.githubusercontent.com/n400peanuts/languagelearninglab/master/tools/'

listFunctions <- c( "inverse_log_odd.R", "myCenter.R", "lizCenter.R", "getmode.R", "lizCenter2.R", "deleteRandomRows.R", "Bf.R", "adjust_intercept_model.R")

loadFunctionsGithub(urlFolder, urlRaw, listFunctions)

# 1. load data

df <- list.files(paste(localGitDir, "/preProcessed_data/", sep = "")); 
df<- c(df[df=="contingency.csv"], df[df=="labPic.csv"], df[df=="picLab.csv"])

for (i in 1:length(df)){
  gsub(".csv$", "", df[i]) -> id #remove .csv
  assign(id, data.frame()) #load into the environment
  read.csv(paste(localGitDir, "/preProcessed_data/", df[i], sep = ""),
           na.strings=c("","NA"),
           stringsAsFactors = F,
           colClasses=c("label"="factor",
                        "correctLabel"="factor",
                        "learning"="factor"
           ))-> temp
  assign(paste0(id), temp)
}; rm(temp, df, i, id);


# 2. exclude participants

listSubj.labPic <-aggregate(acc ~ subjID, labPic[labPic$trialFrequency=='control',] ,mean)
badSubj <-unique(listSubj.labPic[listSubj.labPic$acc<.8,]$subjID); 

listSubj.picLab <-aggregate(acc ~ subjID, picLab[picLab$trialType=='control',] ,mean)
badSubj2 <-unique(listSubj.picLab[listSubj.picLab$acc<.8,]$subjID); 
c(badSubj2, badSubj)-> badSubj; rm(badSubj2)

labPic.clean <- labPic[!(labPic$subjID %in% badSubj),]; 
picLab.clean <- picLab[!(picLab$subjID %in% badSubj),]; 
contingency <-contingency[!(contingency$subjID %in% badSubj),]


# 3. remove control trials (blue bims) --- as outlined in the preregistration, analysis are conducted on experimental trials only.

labPic.clean <- labPic.clean[labPic.clean$trialFrequency!='control',]
picLab.clean <- picLab.clean[picLab.clean$trialType!='control',]

picLab.clean$subjID <- as.factor(picLab.clean$subjID)
picLab.clean$correctFrequency<-recode(picLab.clean$correctFrequency, "25"="low", "75" = "high")
picLab.clean$trialType<-as.factor(picLab.clean$trialType)
picLab.clean$correctFrequency<-as.factor(picLab.clean$correctFrequency)
picLab.clean<-droplevels(picLab.clean)
picLab.clean$task<-as.factor(picLab.clean$task)
picLab.clean$body<-as.factor(picLab.clean$body)

labPic.clean$subjID <- as.factor(labPic.clean$subjID)
labPic.clean$correctFrequency<-recode(labPic.clean$correctFrequency, "25"="low", "75" = "high")
labPic.clean$trialType<-as.factor(labPic.clean$trialType)
labPic.clean$correctFrequency<-as.factor(labPic.clean$correctFrequency)
labPic.clean<-droplevels(labPic.clean)
labPic.clean$task<-as.factor(labPic.clean$task)
labPic.clean$body<-as.factor(labPic.clean$body)
labPic.clean$trialFrequency <- as.factor(labPic.clean$trialFrequency)
labPic.clean$type_of_resp <- as.factor(labPic.clean$type_of_resp)

# load Ramscar et al data
read.table(paste(localGitDir, "/fmri.txt", sep = ""), header = T, stringsAsFactors = T)-> fmri
fmri<-fmri[!(fmri$subjID==1  & fmri$learning=="sx") &
             !(fmri$subjID==14 & fmri$learning=="sx") &
             !(fmri$subjID==22 & fmri$learning=="sx") &
             !(fmri$subjID==3 & fmri$learning=="sx") &
             !(fmri$subjID==4 & fmri$learning=="xs") &
             !(fmri$subjID==7 & fmri$learning=="xs") &
             !(fmri$subjID==10 & fmri$learning=="xs")&
             !(fmri$subjID==12 & fmri$learning=="xs"),]
fmri <- fmri[(fmri$trialType!="control"),]
fmri<-lizCenter(fmri, listfname = list("learning", "frequency"))


fmriglmer1_V1<-glmer(acc ~  frequency.ct*learning.ct + (frequency.ct|subjID), 
                     data = fmri, 
                     family="binomial",
                     control=glmerControl(optimizer = "bobyqa"))
fmriglmer1_V2<-glmer(acc ~  frequency.ct+ frequency: learning.ct + (frequency.ct|subjID), 
                     data = fmri, 
                     family="binomial",
                     control=glmerControl(optimizer = "bobyqa"))
beta1 = summary(fmriglmer1_V1)$coeff["frequency.ct", "Estimate"]
beta2 = summary(fmriglmer1_V1)$coeff["learning.ct", "Estimate"]
beta3 = summary(fmriglmer1_V1)$coeff["frequency.ct:learning.ct", "Estimate"]
simple_effect = summary(fmriglmer1_V2)$coeff["frequencyl:learning.ct", "Estimate"]
simple_effecth = summary(fmriglmer1_V2)$coeff["frequencyh:learning.ct", "Estimate"]

# start with a small sample size and move to larger sample sizes
labPic.clean-> labPic.clean.comparison
picLab.clean->picLab.clean.comparison


labPic.clean.comparison$type_of_resp <- NULL; labPic.clean.comparison$trialFrequency <- NULL;
labPic.clean.comparison$trialFrequency.ct <- NULL;
labPic.clean.comparison$learning.ct <-NULL;
picLab.clean.comparison$correctFrequency.ct <- NULL;
picLab.clean.comparison$learning.ct <-NULL;

rbind(labPic.clean.comparison, picLab.clean.comparison)->FLO_tasks
FLO_tasks$learning<- relevel(FLO_tasks$learning, ref = "LF")
FLO_tasks<-lizCenter(FLO_tasks, listfname = list("learning", "correctFrequency"))

totalSubj <-length(unique(FLO_tasks$subjID))
subjIDs <-unique(FLO_tasks$subjID)

repFLO<-glmer(acc ~  correctFrequency.ct*learning.ct + (correctFrequency.ct|subjID), 
              data = FLO_tasks, 
              family="binomial",
              control=glmerControl(optimizer = "bobyqa"))

apostPower <- as.data.frame(round(summary(repFLO)$coefficients,4))
main_effect <- apostPower["correctFrequency.ct", "Estimate"] #that is negative
main_se <- apostPower["correctFrequency.ct", "Std. Error"]

bayesFac_apostPower_freq<-Bf(main_se, (main_effect*-1), 
                            likelihood = "normal",  
                            modeloftheory = "normal", 
                            modeoftheory = 0, 
                            scaleoftheory = round(beta1,2)*-1, 
                            tail = 1)
main_effect <- apostPower["learning.ct", "Estimate"] #that is negative
main_se <- apostPower["learning.ct", "Std. Error"]

bayesFac_apostPower_learn<-Bf(main_se, (main_effect), 
                             likelihood = "normal",  
                             modeloftheory = "normal", 
                             modeoftheory = 0, 
                             scaleoftheory = round(beta2,2), 
                             tail = 1)
main_effect <- apostPower["correctFrequency.ct:learning.ct", "Estimate"] #that is negative
main_se <- apostPower["correctFrequency.ct:learning.ct", "Std. Error"]

bayesFac_apostPower_inter<-Bf(main_se, (main_effect),
                             likelihood = "normal",  
                             modeloftheory = "normal", 
                             modeoftheory = 0, 
                             scaleoftheory = round(beta3,2), 
                             tail = 1)
apostPower$n <- totalSubj
apostPower$BF <- c(NA,bayesFac_apostPower_freq,bayesFac_apostPower_learn,bayesFac_apostPower_inter)

repFLO_V2<-glmer(acc ~  correctFrequency.ct+ correctFrequency: learning.ct  + (correctFrequency.ct|subjID), 
                 data = FLO_tasks, 
                 family="binomial",
                 control=glmerControl(optimizer = "bobyqa"))

apostpower_simpleEffect <- as.data.frame(round(summary(repFLO_V2)$coefficients,4))

main_effect <- apostpower_simpleEffect["correctFrequencylow:learning.ct", "Estimate"] #that is negative
main_se <- apostpower_simpleEffect["correctFrequencylow:learning.ct", "Std. Error"]

apostpower_simple_low_learn<-Bf(main_se, (main_effect),
                                        likelihood = "normal",  
                                        modeloftheory = "normal", 
                                        modeoftheory = 0, 
                                        scaleoftheory = round(simple_effect,2), 
                                        tail = 1)

main_effect <- apostpower_simpleEffect["correctFrequencyhigh:learning.ct", "Estimate"] #that is negative
main_se <- apostpower_simpleEffect["correctFrequencyhigh:learning.ct", "Std. Error"]

apostpower_simple_high_learn<-Bf(main_se, (main_effect),
                                         likelihood = "normal",  
                                         modeloftheory = "normal", 
                                         modeoftheory = 0, 
                                         scaleoftheory = round(simple_effect,2), 
                                         tail = 1)

apostpower_simpleEffect$n <- totalSubj
apostpower_simpleEffect$BF <- c(NA,NA,apostpower_simple_high_learn, apostpower_simple_low_learn)
apostPower <- rbind(apostPower, apostpower_simpleEffect[3:4,])
apostPower$is_sing <- "N"
apostPower$conv_error <- "N"
superPower <- NA


for (x in 1:50){
  FLO_tasks_temp <- FLO_tasks
  subjIDs <-unique(FLO_tasks$subjID)
  
  for(i in 1:9){
    totalSubj <-length(unique(FLO_tasks_temp$subjID))
    print(paste0("subjects left: ", totalSubj, " iteration number: ", x))
    mySubjs <- sample(subjIDs,20)
    subjIDs <- subjIDs[!(subjIDs %in% mySubjs)] 
    model1<-glmer(acc ~  correctFrequency.ct*learning.ct + (correctFrequency.ct|subjID), 
                  data = FLO_tasks[FLO_tasks$subjID %in% mySubjs,], 
                  family="binomial",
                  control=glmerControl(optimizer = "bobyqa"))
    FLO_tasks_temp <- FLO_tasks_temp[!(FLO_tasks_temp$subjID %in% mySubjs),]
    output <- as.data.frame(round(summary(model1)$coefficients,4))
    output$n <- length(unique(FLO_tasks_temp$subjID))
    output2 <- as.data.frame(round(summary(model2)$coefficients,4))
    output2$n <- length(unique(FLO_tasks_temp$subjID))
    
    if (isSingular(model1)) {
      is_sing = "Y"
    } else {
      is_sing = "N"
    }
    conv <- output$optinfo$conv$lme4$messages
    # 2. Collect if not duplicate of singularity warning
    if (!is.null(conv) && conv != "boundary (singular) fit: see ?isSingular") {
      conv_error = "Y"
    } else {
      conv_error = "N"
    }
    output$conv_error <- conv_error
    output$is_sing <- is_sing
    
    model2<-glmer(acc ~  correctFrequency.ct+ correctFrequency: learning.ct  + (correctFrequency.ct|subjID), 
                  data = FLO_tasks[FLO_tasks$subjID %in% mySubjs,], 
                  family="binomial",
                  control=glmerControl(optimizer = "bobyqa"))
    
    if (isSingular(model2)) {
      is_sing = "Y"
    } else {
      is_sing = "N"
    }
    conv <- output2$optinfo$conv$lme4$messages
    # 2. Collect if not duplicate of singularity warning
    if (!is.null(conv) && conv != "boundary (singular) fit: see ?isSingular") {
      conv_error = "Y"
    }else {
      conv_error = "N"
    }
    output2$conv_error <- conv_error
    output2$is_sing <- is_sing
    
    
    
    main_effect <- output["correctFrequency.ct", "Estimate"] #that is negative
    main_se <- output["correctFrequency.ct", "Std. Error"]
    
    bayesFac_apostPower_freq<-Bf(main_se, (main_effect*-1), 
                                 likelihood = "normal",  
                                 modeloftheory = "normal", 
                                 modeoftheory = 0, 
                                 scaleoftheory = round(beta1,2)*-1, 
                                 tail = 1)
    main_effect <- output["learning.ct", "Estimate"] #that is negative
    main_se <- output["learning.ct", "Std. Error"]
    
    bayesFac_apostPower_learn<-Bf(main_se, (main_effect), 
                                  likelihood = "normal",  
                                  modeloftheory = "normal", 
                                  modeoftheory = 0, 
                                  scaleoftheory = round(beta2,2), 
                                  tail = 1)
    main_effect <- output["correctFrequency.ct:learning.ct", "Estimate"] #that is negative
    main_se <- output["correctFrequency.ct:learning.ct", "Std. Error"]
    
    bayesFac_apostPower_inter<-Bf(main_se, (main_effect),
                                  likelihood = "normal",  
                                  modeloftheory = "normal", 
                                  modeoftheory = 0, 
                                  scaleoftheory = round(beta3,2), 
                                  tail = 1)
    output$BF <- c(NA,bayesFac_apostPower_freq,bayesFac_apostPower_learn,bayesFac_apostPower_inter) 
    
    
    main_effect <- output2["correctFrequencylow:learning.ct", "Estimate"] #that is negative
    main_se <- output2["correctFrequencylow:learning.ct", "Std. Error"]
    
    apostpower_simple_low_learn<-Bf(main_se, (main_effect),
                                    likelihood = "normal",  
                                    modeloftheory = "normal", 
                                    modeoftheory = 0, 
                                    scaleoftheory = round(simple_effect,2), 
                                    tail = 1)
    
    main_effect <- output2["correctFrequencyhigh:learning.ct", "Estimate"] #that is negative
    main_se <- output2["correctFrequencyhigh:learning.ct", "Std. Error"]
    
    apostpower_simple_high_learn<-Bf(main_se, (main_effect),
                                     likelihood = "normal",  
                                     modeloftheory = "normal", 
                                     modeoftheory = 0, 
                                     scaleoftheory = round(simple_effect,2), 
                                     tail = 1)
    output2$BF <- c(NA,NA,apostpower_simple_high_learn,apostpower_simple_low_learn) 
    
    apostPower <- rbind(apostPower, output, output2[3:4,])
  }
  apostPower$loopNumber <- x
  superPower <- rbind(superPower, apostPower)
  apostPower <- NA
}

summary(superPower)

superPower[sapply(superPower, is.character)] <- 
  lapply(superPower[sapply(superPower, is.character)], as.factor)


write.csv(superPower, paste0(outputDir, "aposteriori_power.csv"))

#### plot ####
power <- read.csv(paste0(outputDir, "aposteriori_power.csv"), stringsAsFactors=T)
power <- power[!is.na(power$is_sing),]
power <- power[!is.na(power$BF),]
power <- power[!(power$BF=="Inf"),]

#interaction
power.int <- power[grepl("correctFrequency.ct:learning.ct", power$X, fixed = T),]
p1<-ggplot(power.int, aes(x=n, y=BF)) + 
  geom_jitter(color = "black", fill = "lightgrey", shape = 21, size = 3)+ 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", width=2) +
  stat_summary(fun=mean, geom="point", color="red", size = 2)+
  ggtitle("interaction learning by frequency")+
  xlab("n")+
  ylab("Bayes Factor")+
  theme_pubr()+
  ylim(0,0.5)+
  geom_hline(yintercept=.33, linetype="dashed", 
             color = "red")


#simple effect low frequency
power.simple.low.freq <- power[grepl("correctFrequencylow:learning.ct", power$X, fixed = T),]
p2<-ggplot(power.simple.low.freq, aes(x=n, y=BF)) + 
  geom_jitter(color = "black", fill = "lightgrey", shape = 21, size = 3)+ 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", width=2) +
  stat_summary(fun=mean, geom="point", color="red", size = 2)+
  ggtitle("simple effect of learning for low frequency")+
  xlab("n")+
  ylab("Bayes Factor")+
  theme_pubr()+
  ylim(0,0.5)+
  geom_hline(yintercept=.33, linetype="dashed", 
             color = "red")


#simple effect high frequency
power.simple.high.freq <- power[grepl("correctFrequencyhigh:learning.ct", power$X, fixed = T),]
p3<-ggplot(power.simple.high.freq, aes(x=n, y=BF)) + 
  geom_jitter(color = "black", fill = "lightgrey", shape = 21, size = 3)+ 
  stat_summary(fun.data=mean_sdl, fun.args = list(mult=1), geom="errorbar", width=2) +
  stat_summary(fun=mean, geom="point", color="red", size = 2)+
  ggtitle("simple effect of learning for high frequency")+
  xlab("n")+
  ylab("Bayes Factor")+
  theme_pubr()+
  ylim(0,0.5)+
  geom_hline(yintercept=.33, linetype="dashed", 
             color = "red")


ggarrange(p1,p2,p3)
ggsave("posthocPower.png", width =9, height = 4)
plot(power.int$n,power.int$Pr...z..)
