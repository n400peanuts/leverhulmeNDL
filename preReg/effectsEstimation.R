# load data
library(ggplot2)
library(ggpubr)
library(lme4)

rm(list = ls())

#### set your working directory ####
localGitDir <- 'C:/Users/eva_v/Documents/GitHub/leverhulmeNDL/prereg'

#### load lab functions ####
urlFolder <- 'https://api.github.com/repos/n400peanuts/languagelearninglab/git/trees/master?recursive=1'
urlRaw <- 'https://raw.githubusercontent.com/n400peanuts/languagelearninglab/master/tools/'

loadFunctionsGithub <-function(urlFolder, urlRaw){
  if (!require(httr)) {
    stop("httr not installed")
  } 
  else if (!require(RCurl)){
    stop("RCurl not installed") 
  }
  else {
    print('----loading. Please wait----')
  };
  httr::GET(urlFolder)-> req
  stop_for_status(req)
  filelist <- unlist(lapply(content(req)$tree, "[", "path"), use.names = F)
  urlFunctions <- grep("docs/tools/", filelist, value = TRUE, fixed = TRUE)
  gsub("docs/tools/", "", urlFunctions) -> functions
  for (i in 1:length(functions)){
    RCurl::getURL(paste0(urlRaw, functions[i]), ssl.verifypeer = FALSE)-> temp
    eval(parse(text = temp), envir = .GlobalEnv)
  };
}

loadFunctionsGithub(urlFolder = urlFolder, urlRaw = urlRaw);
rm(urlFolder, urlRaw, loadFunctionsGithub)

#### load fmri data ####
# XS = feature label
# SX = label feature

fmri<-read.table(paste(localGitDir, "/fmri.txt", sep = ""), header = T)

#Michael trimmed participants that scored <=.8 in the control condition
temp <- aggregate(acc ~  subjID+learning+testing, data = fmri[fmri$trialType=="control",], mean)

#these are the participants that do satisfy this criteria:
temp[(temp$acc<=.8),]

#we know also from Mike's notes that number 7 of the XS-XS took too long in the scanner
#okay let's remove completely these participants:
fmriClean<-fmri[!(fmri$subjID==1  & fmri$learning=="sx") &
                  !(fmri$subjID==14 & fmri$learning=="sx") &
                  !(fmri$subjID==22 & fmri$learning=="sx") &
                  !(fmri$subjID==3 & fmri$learning=="sx") &
                  !(fmri$subjID==4 & fmri$learning=="xs") &
                  !(fmri$subjID==7 & fmri$learning=="xs") &
                  !(fmri$subjID==10 & fmri$learning=="xs")&
                  !(fmri$subjID==12 & fmri$learning=="xs"),]

#check manually means with the excel file, "total" sheet:
aggregate(acc ~  subjID + learning+testing+frequency, fmriClean[fmriClean$learning=="xs"  & fmriClean$testing=="xs"  & !(fmriClean$trialType=="control"),],mean)
#ok
aggregate(acc ~  subjID + learning+testing+frequency, fmriClean[fmriClean$learning=="xs"  & fmriClean$testing=="sx"  & !(fmriClean$trialType=="control"),],mean)
#ok
aggregate(acc ~  subjID + learning+testing+frequency, fmriClean[fmriClean$learning=="sx"  & fmriClean$testing=="xs"  & !(fmriClean$trialType=="control"),],mean)
#ok
aggregate(acc ~  subjID + learning+testing+frequency, fmriClean[fmriClean$learning=="sx"  & fmriClean$testing=="sx"  & !(fmriClean$trialType=="control"),],mean)
#ok

#okay, means are the same as reported in the excel file

#### barplot ####
ggbarplot(fmriClean[!(fmriClean$trialType=="control"),], x = "learning",
          color = "black",
          fill = "frequency",
          #facet.by = "testing", #to see differences between tasks
          y = "acc",
          ylab = "average accuracy",
          add = c("mean_se"),
          position = position_dodge(.8),
          palette = c("#63AAFE","#DD2D32"),
          title = "results fmri data"
) + scale_y_continuous(breaks = seq(0, 1, .1)) +
  geom_hline(yintercept = .33, col='black', lwd=.6, linetype="dashed")+
  theme(legend.position="right", legend.title = element_blank())

#okay, the figure reported in chart1 of the excel file is not 100% correct
#UPDATE: I've spotted an error in the excel file, "tmp" sheet. So our figure here is the right one.

#what is curious is the pattern across the two tasks if we split the data by testing condition.
#However authors consider both tasks together, so we're going to consider it together in the glmm

#### GLMM ####

#let's center the variables. 
fmriClean$frequency <- as.factor(fmriClean$frequency)
fmriClean$learning<-as.factor(fmriClean$learning)
fmriClean$testing<-as.factor(fmriClean$testing)
fmriClean$trialType<-as.factor(fmriClean$trialType)

fmriClean<-lizCenter(fmriClean, listfname = list("learning" , "frequency"))

#relevel
fmriClean$learning <- relevel(fmriClean$learning, ref = "sx")
fmriClean$frequency <- relevel(fmriClean$frequency, ref = "l")


fmriglmer1<-glmer(acc ~  frequency.ct*learning.ct + (frequency.ct|subjID), 
      data = fmriClean[!(fmriClean$trialType=="control"),], # "control" are blue fribbles, so we need to remove it.
      family="binomial",
      control=glmerControl(optimizer = "bobyqa"))
car::Anova(fmriglmer1)
summary(fmriglmer1)

fmriglmer1.emm <-emmeans::emmeans(fmriglmer1 , ~ frequency.ct* learning.ct )
emmeans::contrast(fmriglmer1.emm, "consec",  simple = "each", combine = F, adjust = "bonferroni")



#interaction between frequency and learning:
round(summary(fmriglmer1)$coefficients[4,],4)

#main effect of frequency:
round(summary(fmriglmer1)$coefficients[2,],4)

#main effect of learning:
round(summary(fmriglmer1)$coefficients[3,],4)


#These are the final means
meanfmri<-aggregate(acc ~  frequency+learning, fmriClean[!(fmriClean$trialType=="control"),],mean)
meanfmri

#contingency judgment
# the estimates from the model
# match vs mismatch type 1 - run separate models
# learning by frequency
# low frequency mismt type 1 - between learning

#in terms of stopping rules
# see the max Masa's participants
# see Nixon
# keep going until they show an answer

#go for merged data
