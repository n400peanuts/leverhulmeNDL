library(tidyverse);
library(ggplot2)
library(ggpubr)
library(lme4)
library(lmerTest)
rm(list = ls())

#### set local directory ####
#Set your local working directory. This should be (and is assumed to be in the rest of the code) the highest point in your local folder:

localGitDir <- 'C:/Users/eva_v/Documents/GitHub/leverhulmeNDL'
#localGitDir <- ''

#### load functions from the lab repo ####
source(paste(localGitDir, "/tools/loadFunctionsGithub.R", sep = "")) 
urlFolder <- 'https://api.github.com/repos/n400peanuts/languagelearninglab/git/trees/master?recursive=1'
urlRaw <- 'https://raw.githubusercontent.com/n400peanuts/languagelearninglab/master/tools/'

listFunctions <- c( "inverse_log_odd.R", "myCenter.R", 
                    "lizCenter.R", "getmode.R", "lizCenter2.R", "deleteRandomRows.R")

loadFunctionsGithub(urlFolder, urlRaw, listFunctions)

adjust_intercept_model<- function(model, chance, intercept_list = c("(Intercept)"))
{
  summary = summary(model)$coefficients
  
  for (i in 1:length(intercept_list)){
    summary[intercept_list[i], "Estimate"] = summary[intercept_list[i], "Estimate"]- chance
    summary[intercept_list[i], "z value"] =  summary[intercept_list[i], "Estimate"]/summary[intercept_list[i], "Std. Error"]
    summary[intercept_list[i], "Pr(>|z|)"] =  p =2*pnorm(-abs(summary[intercept_list[i], "z value"])) }
  
  
  summary[intercept_list[i], "Estimate"]
  
  
  return(summary)
  
}

Bf<-function(sd, obtained, dfdata = 1, likelihood = c("normal", "t"), modeloftheory= c("normal","t","cauchy", "uniform") ,lower =0, upper=1, modeoftheory = 0, scaleoftheory = 1, dftheory = 1, tail = 2)
{
  # if using normal likelihood, set dfdata to high value
  # this means t distributions used for likelihood below will approximate to normal
  if(likelihood=="normal"){
    dfdata=10^10
  }
  # likewise for if using normal theory
  if(modeloftheory=="normal"){
    dftheory = 10^10
    # if using cauchy, set df to 1
    # this means t distributions used for theory below will turn into Cauchys
    # Cauchy is t distrib with df = 1
  } else if(modeloftheory=="cauchy"){
    dftheory = 1
  }
  # set area to 0
  area <- 0
  normarea <- 0
  # if a uniform distribution is specified by theory
  if(modeloftheory=="uniform"){
    # set theta = lower bound
    theta <- lower
    # set range = difference between upper and lower bound
    range <- upper - lower
    # increment = 1/2000th of the range
    incr <- range / 2000
    # from -1000 to +1000 - A isn't used, so this is equivalent to just counting from 0 to 2000
    # this  goes through the uniform dist by increments
    for (A in -1000:1000){
      # add the increment to theta
      theta <- theta + incr
      # for a uniform distribution, the probability density is constant: 1/range
      dist_theta <- 1 / range
      # we multiply this by the density at the obtained mean in the distrib centred around current theta with observed SE
      # this distrib is either normal, t or cauchy as determined by the degrees of freedom
      # in short: if this was the true theta, how likely are the data under this?
      height <- dist_theta * dt((obtained-theta)/sd, df=dfdata)
      # we then want the area under the line
      area <- area + height * incr
      # so what this is calculating is the combo of 'how probable is this theta according to the prior'
      # and 'how likely is this mean (assuming an accurate SE) under the current theta?'
      # i.e., the marginal likelihood
    }
    # assign the overall marginal likelihood to LikelihoodTheory
    LikelihoodTheory <- area
  }else{
    # if normal theta, go through the theta distribution = your expected H1 mean distribution
    # set theta initially to mode minus 10 * the scale parameter
    theta <- modeoftheory - 10 * scaleoftheory
    # increment is 1/200th of the scale parameter
    incr <- scaleoftheory/200
    # from -2000 to +2000 - again, A isn't used, so equiv of counting from 0 to 4000
    # the resulting calculations therefore end at mean plus 10 * scale parameter
    for (A in -2000:2000){
      # add increment
      theta <- theta + incr
      # here, the theoretical height is t density at (theta - mode)/scale
      # i.e. probability of the t-statistic corresponding to this theta relative to mode
      dist_theta <- dt((theta-modeoftheory)/scaleoftheory, df=dftheory)
      # are we specifying 1 tail?
      if(identical(tail, 1)){
        # if current theta is less than the mode, set density to 0
        if (theta <= modeoftheory){
          dist_theta <- 0
        } else {
          # otherwise we multiply density by 2
          dist_theta <- dist_theta * 2
        }
      }
      # if this was the true value of theta,
      # what's the likelihood of the t-statistic corresponding to the observed mean & SE
      # in a t distribution centred on this theta?
      # multiply this by dist_theta to combine prior probability and likelihood
      height <- dist_theta * dt((obtained-theta)/sd, df = dfdata)
      # add to accumulated area
      area <- area + height * incr
      # also add to normarea
      # pretty sure normarea will always just equate to the scale of theory, in normal case
      normarea <- normarea + dist_theta*incr
    }
    LikelihoodTheory <- area/normarea
    # print(LikelihoodTheory)
  }
  # likelihood of null is simply likelihood of the observed t in t distrib centred on 0
  Likelihoodnull <- dt(obtained/sd, df = dfdata)
  # print(Likelihoodnull)
  BayesFactor <- LikelihoodTheory/Likelihoodnull
  BayesFactor
}


#### 1) load data ####
#all tasks have been cleaned from Gorilla's https info and are ready to be loaded 
df <- list.files(paste(localGitDir, "/exp2/preProcessed_data/", sep = "")); 

for (i in 1:length(df)){
  gsub(".csv$", "", df[i]) -> id #remove .csv
  assign(id, data.frame()) #load into the environment
  read.csv(paste(localGitDir, "/exp2/preProcessed_data/", df[i], sep = ""),
           na.strings=c("","NA"),
           stringsAsFactors = F,
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
listSubj.labPic <-aggregate(acc ~ subjID, labPic[labPic$trialType=='control',] ,mean)
badSubj <-unique(listSubj.labPic[listSubj.labPic$acc<.8,]$subjID); 

listSubj.picLab <-aggregate(acc ~ subjID, picLab[picLab$trialType=='control',] ,mean)
badSubj2 <-unique(listSubj.picLab[listSubj.picLab$acc<.8,]$subjID); 
c(badSubj2, badSubj)-> badSubj; rm(badSubj2)

#How many participants have been excluded in total?
round(length(badSubj) / (nrow(listSubj.labPic)+nrow(listSubj.picLab)) *100, 1)
#around 30%

labPic.clean <- labPic[!(labPic$subjID %in% badSubj),]; 
picLab.clean <- picLab[!(picLab$subjID %in% badSubj),]; 

#accuracy on controls after badsubjs removal 
round(mean(na.omit(labPic.clean[labPic.clean$trialType=="control",]$acc))*100,1)
round(mean(na.omit(picLab.clean[picLab.clean$trialType=="control",]$acc))*100,1)

#remove control trials
labPic.clean <- labPic.clean[labPic.clean$trialType!='control',]
picLab.clean <- picLab.clean[picLab.clean$trialType!='control',]

picLab.clean$subjID <- as.factor(picLab.clean$subjID)
picLab.clean$correctFrequency<-recode(picLab.clean$correctFrequency, "25"="low", "75" = "high")
picLab.clean$trialType<-as.factor(picLab.clean$trialType)
picLab.clean$correctFrequency<-as.factor(picLab.clean$correctFrequency)
picLab.clean<-droplevels(picLab.clean)
picLab.clean$task<-as.factor(picLab.clean$task)

labPic.clean$subjID <- as.factor(labPic.clean$subjID)
labPic.clean$correctFrequency<-recode(labPic.clean$correctFrequency, "25"="low", "75" = "high")
labPic.clean$trialType<-as.factor(labPic.clean$trialType)
labPic.clean$correctFrequency<-as.factor(labPic.clean$correctFrequency)
labPic.clean<-droplevels(labPic.clean)
labPic.clean$task<-as.factor(labPic.clean$task)

#### 3) raw means ####
#total number of participants
length(unique(labPic.clean$subjID)) + length(unique(picLab.clean$subjID))

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
read.table(paste(localGitDir, "/preReg/fmri.txt", sep = ""), header = T, stringsAsFactors = T)-> fmri
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
simple_eFfecth #HIGH FREQUENCY


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

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta2,2), 
   tail = 1)

#### A) interaction ####
main_effect <- output["correctFrequency.ct:learning.ct", "Estimate"] #that is negative
main_se <- output["correctFrequency.ct:learning.ct", "Std. Error"]

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
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


Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect,2), 
   tail = 1)

#### A.2) simple effect (high frequency X learning) ####

main_effect <- output_simpleEffect["correctFrequencyhigh:learning.ct", "Estimate"] #that is negative
main_se <- output_simpleEffect["correctFrequencyhigh:learning.ct", "Std. Error"]

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
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

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
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


Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effecth,2), 
   tail = 1)

#### Both 2AFC tasks together ####
rbind(labPic.clean, picLab.clean)->FLO_tasks


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

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(beta2,2), 
   tail = 1)

#### A) interaction ####
main_effect <- output["correctFrequency.ct:learning.ct", "Estimate"] #that is negative
main_se <- output["correctFrequency.ct:learning.ct", "Std. Error"]

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
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

main_effect <- output_simpleEffect["correctFrequencylow:learning.ct", "Estimate"] #that is negative
main_se <- output_simpleEffect["correctFrequencylow:learning.ct", "Std. Error"]

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect,2), 
   tail = 1)
#### A.2) simple effect (high frequency X learning) ####

main_effect <- output_simpleEffect["correctFrequencyhigh:learning.ct", "Estimate"] #that is negative
main_se <- output_simpleEffect["correctFrequencyhigh:learning.ct", "Std. Error"]


Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effecth,2), 
   tail = 1)

#### A.2) simple effect (high frequency X learning) ####


#### Plot FLO tasks ####
p1<-ggpubr::ggbarplot(na.omit(labPic.clean), x = "learning", y = "acc",
                  add = c("mean_se"),
                  facet.by = c("correctFrequency"),
                  title = "2AFC - 4pictures")+ geom_hline(yintercept = .33, linetype=2)

p2<-ggpubr::ggbarplot(na.omit(picLab.clean), x = "learning", y = "acc",
                  add = c("mean_se"),
                  facet.by = c("correctFrequency"),
                  title = "2AFC - 4labels")+ geom_hline(yintercept = .33, linetype=2)

p3<-ggpubr::ggbarplot(na.omit(FLO_tasks), x = "learning", y = "acc",
                      add = c("mean_se"),
                      facet.by = c("correctFrequency"),
                      title = "2AFC - together")+ geom_hline(yintercept = .33, linetype=2)



ggarrange(p1,p2,p3)->p4
p4

aggregate(acc ~ correctFrequency+learning, FLO_tasks ,mean)
aggregate(acc ~ correctFrequency+learning, labPic.clean ,mean)
aggregate(acc ~ correctFrequency+learning, picLab.clean ,mean)
rm(p1,p2,p3)

#### contingency judgment ####
#from our pilot data:

main_effect_type <- 53.78
main_effect_freq_by_type <- 83.618
simple_effect_learn_by_freq_high_match <- 16
simple_effect_learn_by_freq_low_match <- -15
simple_effect_learn_by_freq_high_mismatch <- 15.477
simple_effect_learn_by_freq_low_mismatch <- 17.27

contingency$trialType<- as.factor(contingency$trialType)
contingency$frequency<- as.factor(contingency$frequency)
contingency$subjID<- as.factor(contingency$subjID)
contingency$fribbleID<- as.factor(contingency$fribbleID)

relevel(contingency$trialType, ref = "mismatch-type1")->contingency$trialType
relevel(contingency$learning, ref = "LF")->contingency$learning

contingency<-lizCenter(contingency, listfname = list("learning", "frequency", "trialType"))

lm1<- lmerTest::lmer(resp ~  frequency:trialType:learning + frequency.ct * trialType.ct  + (frequency.ct|subjID)  , data = contingency)
car::Anova(lm1)
output<-round(summary(lm1)$coefficients,4) 
output

#note that if I simplify the model with (1|subjID) it doesn't tell me that is singular, and results do not change
lm1<- lmerTest::lmer(resp ~  trialType.ct * frequency.ct * learning.ct  + (1|subjID)  , data = contingency)

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

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(main_effect_freq_by_type,2), 
   tail = 1)


#why Inf?

#### C1) Simple effect of learning for frequency high - match ####
main_effect <- output["frequencyh:trialTypematch:learningLF", "Estimate"] 
main_se <- output["frequencyh:trialTypematch:learningLF", "Std. Error"] 

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
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

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
   modeoftheory = 0, 
   scaleoftheory = round(simple_effect_learn_by_freq_high_mismatch,2), 
   tail = 1)

#### C4) Simple effect of learning for frequency low - mismatch ####
main_effect <- output["frequencyl:trialTypemismatch-type1:learningLF", "Estimate"] 
main_se <- output["frequencyl:trialTypemismatch-type1:learningLF", "Std. Error"] 

Bf(main_se, (main_effect*-1), #main effect - note that if you want to compute BF for frequency that is negative
   likelihood = "normal",  #you need to have (maineffect*-1) to revert it to positive otherwise the function
   modeloftheory = "normal", #throws an error
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

#### exploratory analysis: ####
#### 1) look at type of error in 2AFC ####
# 2AFC- 4labels
mismatchType<-rbind(
  picLab.clean[picLab.clean$correctLabel=="tob" & (picLab.clean$resp=="dep" | picLab.clean$resp=="wug"),],
  picLab.clean[picLab.clean$correctLabel=="wug" & (picLab.clean$resp=="tob" | picLab.clean$resp=="dep"),],
  picLab.clean[picLab.clean$correctLabel=="dep" & (picLab.clean$resp=="wug" | picLab.clean$resp=="tob"),],
  picLab.clean[picLab.clean$acc==1,])

#number of trials where the fribble was not of the type mismatch 1 and match, but clearly of another category (type2) 
nrow(picLab.clean)-nrow(mismatchType)

#very few trials were of type 2
#mean check to see if anything changes
aggregate(acc ~ correctFrequency+learning, picLab.clean ,mean)
aggregate(acc ~ correctFrequency+learning, mismatchType ,mean)
#LF always better than FL

#2AFC - 4pictures
mismatchType<-rbind(
  labPic.clean[labPic.clean$label=="tob" & (labPic.clean$correctLabel=="dep" | labPic.clean$correctLabel=="wug"),],
  labPic.clean[labPic.clean$label=="wug" & (labPic.clean$correctLabel=="tob" | labPic.clean$correctLabel=="dep"),],
  labPic.clean[labPic.clean$label=="dep" & (labPic.clean$correctLabel=="wug" | labPic.clean$correctLabel=="tob"),],
  labPic.clean[labPic.clean$acc==1,])

#number of trials where the mismatch was type 2      
nrow(mismatchType) - nrow(picLab.clean)     

aggregate(acc ~ correctFrequency+learning, labPic.clean ,mean)
aggregate(acc ~ correctFrequency+learning, mismatchType ,mean)

#### 2) correlation between the 2AFC tasks and contingency task ####
floTasks <-aggregate(acc ~ correctFrequency+subjID, FLO_tasks, mean)

#match trials
conTask0 <-aggregate(resp ~ correctFrequency+subjID, contingency[contingency$trialType=="match",], mean)
merge(floTasks, conTask0, by = c("subjID", "correctFrequency"))->tempMatch

#does accuracy in the generalization tasks correlate with response in the conjudg?
plot(tempMatch$acc, tempMatch$resp, xlab = "accuracy in the 2AFC tasks", ylab = "contingency judgments", main = "Pearson r: 0.89")
cor(tempMatch$acc, tempMatch$resp)

plot(tempMatch[tempMatch$correctFrequency=="low",]$acc, tempMatch[tempMatch$correctFrequency=="low",]$resp)
cor(tempMatch[tempMatch$correctFrequency=="low",]$acc, tempMatch[tempMatch$correctFrequency=="low",]$resp)

plot(tempMatch[tempMatch$correctFrequency=="high",]$acc, tempMatch[tempMatch$correctFrequency=="high",]$resp)
cor(tempMatch[tempMatch$correctFrequency=="high",]$acc, tempMatch[tempMatch$correctFrequency=="high",]$resp)

#does it make a difference which task is it?
pic4lab <-aggregate(acc ~ correctFrequency+subjID, FLO_tasks[FLO_tasks$task=="pictureLabels",], mean)
merge(conTask0, pic4lab, by = c("subjID", "correctFrequency"))->tempMatchpic4lab
plot(tempMatchpic4lab$acc, tempMatchpic4lab$resp)
cor(tempMatchpic4lab$acc, tempMatchpic4lab$resp)

lab4pic <-aggregate(acc ~ correctFrequency+subjID, FLO_tasks[FLO_tasks$task=="labelPictures",], mean)
merge(conTask0, lab4pic, by = c("subjID", "correctFrequency"))->tempMatchlab4pic
plot(tempMatchlab4pic$acc, tempMatchlab4pic$resp)
cor(tempMatchlab4pic$acc, tempMatchlab4pic$resp)

#correlations are almost identical

#mismatch-trials
conTask <-aggregate(resp ~ correctFrequency+subjID, contingency[contingency$trialType=="mismatch-type1",], mean)
merge(floTasks, conTask, by = c("subjID", "correctFrequency"))->tempMismatch
plot(tempMismatch$acc, tempMismatch$resp, xlab = "accuracy in the 2AFC tasks", ylab = "contingency judgments", main = "Pearson r: -0.88")
cor(tempMismatch$acc, tempMismatch$resp)

plot(tempMismatch[tempMismatch$correctFrequency=="low",]$acc, tempMismatch[tempMismatch$correctFrequency=="low",]$resp)
cor(tempMismatch[tempMismatch$correctFrequency=="low",]$acc, tempMismatch[tempMismatch$correctFrequency=="low",]$resp)

plot(tempMismatch[tempMismatch$correctFrequency=="high",]$acc, tempMismatch[tempMismatch$correctFrequency=="high",]$resp)
cor(tempMismatch[tempMismatch$correctFrequency=="high",]$acc, tempMismatch[tempMismatch$correctFrequency=="high",]$resp)

#does it make a difference which task is it?
merge(pic4lab, conTask, by = c("subjID", "correctFrequency"))->tempMismatchpic4lab
plot(tempMismatchpic4lab$acc, tempMismatchpic4lab$resp)
plot(tempMismatchpic4lab[tempMismatchpic4lab$correctFrequency=="low",]$acc, tempMismatchpic4lab[tempMismatchpic4lab$correctFrequency=="low",]$resp)
plot(tempMismatchpic4lab[tempMismatchpic4lab$correctFrequency=="high",]$acc, tempMismatchpic4lab[tempMismatchpic4lab$correctFrequency=="high",]$resp)

merge(lab4pic, conTask, by = c("subjID", "correctFrequency"))->tempMismatchlab4pic
plot(tempMismatchpic4lab$acc, tempMismatchpic4lab$resp)
plot(tempMismatchpic4lab[tempMismatchpic4lab$correctFrequency=="low",]$acc, tempMismatchpic4lab[tempMismatchpic4lab$correctFrequency=="low",]$resp)
plot(tempMismatchpic4lab[tempMismatchpic4lab$correctFrequency=="high",]$acc, tempMismatchpic4lab[tempMismatchpic4lab$correctFrequency=="high",]$resp)


#is accuracy in low freq correlated with high freq?
plot(tempMismatch[tempMismatch$correctFrequency=="low",]$acc, tempMismatch[tempMismatch$correctFrequency=="high",]$acc)
#no, performing good in one category doesn't predict the same performance also in the other


#are people consistent in the judgements? i.e., if match low freq is rated positive, then mismatch low freq should be rated negative 
plot(tempMatch[tempMatch$correctFrequency=="low",]$resp, 
     tempMismatch[tempMismatch$correctFrequency=="low",]$resp,
     xlab = "contingency judgments - match trials", 
     ylab = "contingency judgments - mismatch trials",
     main = "low frequency trials")
plot(tempMatch[tempMatch$correctFrequency=="high",]$resp, tempMismatch[tempMismatch$correctFrequency=="high",]$resp)
#yes, people are consistent in their judgments
#so this test indeed reflects their learning
#it looks like that the more they are accurate, the more they follow the RW model

#let's see this pattern more closely:
#I select only the people that were higher than chance, i.e., they learned correctly!
listSubj <-aggregate(acc ~ subjID, FLO_tasks[FLO_tasks$correctFrequency=="low",] ,mean)
goodSubjs <-unique(listSubj[listSubj$acc>=.3,]$subjID); length(goodSubjs)

conTask2 <-aggregate(resp ~ learning+trialType+correctFrequency, contingency[contingency$subjID %in% goodSubjs,], mean)
floTasks <-aggregate(acc ~ learning+correctFrequency, FLO_tasks[FLO_tasks$subjID %in% goodSubjs,], mean)


merge(floTasks, conTask2, by = c("learning", "correctFrequency"))->conWeight

ggbarplot(conWeight, x = "learning",
          color = "black",
          #fill = "learning",
          y = "resp",
          facet.by = c("correctFrequency","trialType"),
          ylab = "association strength",
          position = position_dodge(.8),
          palette = c("#bdbdbd","#636363"),
          title = "human performance") +
  theme(legend.position = "none")+ 
  geom_hline(yintercept = 0, col='black', lwd=.6, linetype="dashed")

#yes! indeed in those who scored higher than chance the weights follow the RW prediction
relevel(contingency$trialType, ref = "mismatch-type1")->contingency$trialType
relevel(contingency$learning, ref = "LF")->contingency$learning

contingency<-lizCenter(contingency, listfname = list("learning", "frequency", "trialType"))

con_lm1<-lmerTest::lmer(resp ~  frequency:trialType:learning + frequency.ct * trialType.ct  + (frequency.ct|subjID)  , data = contingency[contingency$subjID %in% goodSubjs,])

car::Anova(con_lm1)
round(summary(con_lm1)$coefficients,3)

#pattern of results do not change anyway

#### 3) Is the control condition able to predict performance in low/high freq? ####
#### PICLAB TASK
ggpubr::ggbarplot(na.omit(picLab), x = "learning", y = "acc",
                  add = c("mean_se"),
                  facet.by = c("correctFrequency"),
                  title = "1pic_4labels")+ geom_hline(yintercept = .33, linetype=2)


listSubj <- aggregate(acc ~ subjID, picLab[picLab$correctFrequency=="control",] ,mean)
colnames(listSubj)[2] <- c("acc.control")
listSubj2 <- aggregate(acc ~ subjID, picLab[picLab$correctFrequency=="low",] ,mean)
colnames(listSubj2)[2] <- c("acc.low")
listSubj3 <- aggregate(acc ~ subjID, picLab[picLab$correctFrequency=="high",] ,mean)
colnames(listSubj3)[2] <- c("acc.high")

merge(listSubj, listSubj2, by = c("subjID"))-> corrLOW
merge(corrLOW, listSubj3, by = c("subjID"))-> corrhigh_low

plot(corrhigh_low$acc.control, corrhigh_low$acc.low, xlab = "accuracy control category", ylab = "accuracy in low frequency", main = "2AFC - 4labels task \n Pearson r: .23"); cor.test(corrhigh_low$acc.control, corrhigh_low$acc.low)
plot(corrhigh_low$acc.control, corrhigh_low$acc.high, xlab = "accuracy control category", ylab = "accuracy in high frequency", main = "2AFC - 4labels task \n Pearson r: .7"); cor.test(corrhigh_low$acc.control, corrhigh_low$acc.high)

aggregate(acc ~ correctFrequency+learning, picLab.clean ,mean)
aggregate(acc ~ correctFrequency+learning, picLab[picLab$correctFrequency!="control",] ,mean)

#So there is a positive relation between how good you are in the control category and accuracy in the high and low freq
#this is particularly true for the high frequency
#and doesn't change the effects between learnings

#### LABPIC TASK
ggpubr::ggbarplot(na.omit(labPic), x = "learning", y = "acc",
                  add = c("mean_se"),
                  facet.by = c("correctFrequency"),
                  title = "1label_4pics")+ geom_hline(yintercept = .33, linetype=2)


listSubj <- aggregate(acc ~ subjID, labPic[labPic$correctFrequency=="control",] ,mean)
colnames(listSubj)[2] <- c("acc.control")
listSubj2 <- aggregate(acc ~ subjID, labPic[labPic$correctFrequency=="low",] ,mean)
colnames(listSubj2)[2] <- c("acc.low")
listSubj3 <- aggregate(acc ~ subjID, labPic[labPic$correctFrequency=="high",] ,mean)
colnames(listSubj3)[2] <- c("acc.high")

merge(listSubj, listSubj2, by = c("subjID"))-> corrLOW
merge(corrLOW, listSubj3, by = c("subjID"))-> corrhigh_low

plot(corrhigh_low$acc.control, corrhigh_low$acc.low, xlab = "accuracy control category", ylab = "accuracy in low frequency", main = "2AFC - 4pictures task \n Pearson r: .05"); cor.test(corrhigh_low$acc.control, corrhigh_low$acc.low)
plot(corrhigh_low$acc.control, corrhigh_low$acc.high, xlab = "accuracy control category", ylab = "accuracy in high frequency", main = "2AFC - 4pictures task \n Pearson r: .61"); cor.test(corrhigh_low$acc.control, corrhigh_low$acc.high)

aggregate(acc ~ correctFrequency+learning, labPic.clean ,mean)
aggregate(acc ~ correctFrequency+learning, labPic[labPic$correctFrequency!="control",] ,mean)

rbind(picLab, labPic)-> bothTasks_control

ggpubr::ggbarplot(na.omit(bothTasks_control), x = "learning", y = "acc",
                  add = c("mean_se"),
                  facet.by = c("correctFrequency"),
                  title = "both tasks with control category")+ geom_hline(yintercept = .33, linetype=2)

#like the previous task, removing participants that perform badly in the control cat. improves scores in the high freq but not so much in the low freq
#and more importantly, this doesn't affect the relation between FL and LF learning

#### CONTINGENCY
rbind(picLab, labPic)-> flo_control
listSubj <- aggregate(acc ~ subjID, flo_control[flo_control$correctFrequency=="control",] ,mean)
colnames(listSubj)[2] <- c("acc.control")
listSubj2 <- aggregate(resp ~ subjID, contingency[contingency$correctFrequency=="low" & contingency$trialType=="match",] ,mean)
colnames(listSubj2)[2] <- c("resp.low.match")
listSubj3 <- aggregate(resp ~ subjID, contingency[contingency$correctFrequency=="high" & contingency$trialType=="match",] ,mean)
colnames(listSubj3)[2] <- c("resp.high.match")
listSubj4 <- aggregate(resp ~ subjID, contingency[contingency$correctFrequency=="low" & contingency$trialType=="mismatch-type1",] ,mean)
colnames(listSubj4)[2] <- c("resp.low.mismatch")
listSubj5 <- aggregate(resp ~ subjID, contingency[contingency$correctFrequency=="high" & contingency$trialType=="mismatch-type1",] ,mean)
colnames(listSubj5)[2] <- c("resp.high.mismatch")

merge(listSubj, listSubj2, by = c("subjID"))-> corrLOW
merge(corrLOW, listSubj3, by = c("subjID"))-> corrhigh_low
merge(corrhigh_low, listSubj4, by = c("subjID"))-> corrhigh_low
merge(corrhigh_low, listSubj5, by = c("subjID"))-> corrhigh_low

par(mfrow=c(2,2))
plot(corrhigh_low$acc.control, corrhigh_low$resp.low.match, xlab = "accuracy control category", ylab = "contingency judgments low frequency", main = "Match trials \n Pearson r: -.06")
plot(corrhigh_low$acc.control, corrhigh_low$resp.high.match, xlab = "accuracy control category", ylab = "contingency judgments high frequency", main = "Match trials \n Pearson r: .51") 
plot(corrhigh_low$acc.control, corrhigh_low$resp.low.mismatch, xlab = "accuracy control category", ylab = "contingency judgments low frequency", main = "Mismatch trials \n Pearson r: .1") 
plot(corrhigh_low$acc.control, corrhigh_low$resp.high.mismatch, xlab = "accuracy control category", ylab = "contingency judgments high frequency", main = "Mismatch trials \n Pearson r: -.47")
par(mfrow=c(1,1))

cor.test(corrhigh_low$acc.control, corrhigh_low$resp.high.match)
cor.test(corrhigh_low$acc.control, corrhigh_low$resp.low.mismatch)
cor.test(corrhigh_low$acc.control, corrhigh_low$resp.high.mismatch)
cor.test(corrhigh_low$acc.control, corrhigh_low$resp.low.match)

#### 4) are exp1 and exp2 different?####
localGitDir <- 'C:/Users/eva_v/Documents/GitHub/leverhulmeNDL'

generalizationPL <-read.table(paste(localGitDir, "/exp1/data/pictureLabel.txt", sep = ""), header = T, stringsAsFactors = T)

generalizationLP <-read.table(paste(localGitDir, "/exp1/data/labelPicture.txt", sep = ""), header = T, stringsAsFactors = T)

contingencyJudgement <-read.table(paste(localGitDir, "/exp1/data/humansTypeWeigths.txt", sep = ""), header = T, stringsAsFactors = T)


#merge datasets
#---------------------PICLAB-----------------#
#I need to select only the columns that are useful though
picLab_expe1<- generalizationPL[,c("fribbleID", "subjID", "learning", "task", "rt", "acc", "frequency")]
picLab_expe2 <- picLab.clean[,c("fribbleID", "subjID", "learning", "task", "rt", "acc", "correctFrequency")]
colnames(picLab_expe2)[7] <- "frequency"

rbind(picLab_expe1, picLab_expe2)-> picLab_bothexp
picLab_bothexp$fribbleID <- as.factor(picLab_bothexp$fribbleID)
picLab_bothexp$subjID <- as.factor(picLab_bothexp$subjID)
picLab_bothexp$frequency <- as.factor(picLab_bothexp$frequency)
picLab_bothexp$frequency<-recode(picLab_bothexp$frequency, "25"="low", "75" = "high")
picLab_bothexp$task<-recode(picLab_bothexp$task, "generalizationPL"="expe1", "pictureLabels" = "expe2")

#are those datasets different?
picLab_bothexp$learning<- relevel(picLab_bothexp$learning, ref = "LF")

bothexp_glmer<-glmer(acc ~  frequency:learning:task + frequency*learning + (frequency|subjID), 
                     data = picLab_bothexp, 
                     family="binomial",
                     control=glmerControl(optimizer = "bobyqa"))

output <- round(summary(bothexp_glmer)$coefficients,4)
output

#so overall the experiments are different, for the high frequency condition both learnings show higher accuracy in exp2 (visual)
#but interestingly in exp1 FL people scored better in the low frequency condition as well
#while LF instead scored the same in this condition
aggregate(acc ~ task+frequency+learning, picLab_bothexp ,mean)

#exp2 has higher accuracy in the high frequency cond compared to exp1
#the simpler explanation give the high corr between control condition and acc in high frequency is that exp1 didn't have this cleaning

#---------------------LABPIC-----------------#

labPic_expe1<- generalizationLP[,c("fribbleID", "subjID", "learning", "task", "rt", "acc", "frequency")]
labPic_expe2 <- labPic.clean[,c("fribbleID", "subjID", "learning", "task", "rt", "acc", "correctFrequency")]
colnames(labPic_expe2)[7] <- "frequency"

rbind(labPic_expe1, labPic_expe2)-> labPic_bothexp
labPic_bothexp$fribbleID <- as.factor(labPic_bothexp$fribbleID)
labPic_bothexp$subjID <- as.factor(labPic_bothexp$subjID)
labPic_bothexp$frequency <- as.factor(labPic_bothexp$frequency)
labPic_bothexp$frequency<-recode(labPic_bothexp$frequency, "25"="low", "75" = "high")
labPic_bothexp$task<-recode(labPic_bothexp$task, "generalizationLP"="expe1", "labelPictures" = "expe2")

#are those datasets different?
labPic_bothexp$learning<- relevel(labPic_bothexp$learning, ref = "LF")

bothexp_glmer2<-glmer(acc ~  frequency:learning:task + frequency*learning + (frequency|subjID), 
                      data = labPic_bothexp, 
                      family="binomial",
                      control=glmerControl(optimizer = "bobyqa"))

output <- round(summary(bothexp_glmer2)$coefficients,4)
output

#in the label-picture task apparently accuracy is higher in the low frequency condition for both learnings
# accuracy in the high frequency is higher in the LF, but not in the FL
aggregate(acc ~ task+frequency+learning, labPic_bothexp ,mean)





