rm(list = ls())
library(tidyverse)

inPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/stimuli/")
spreadsheetPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/stimuli/spreadsheet Gorilla/finalSpreadsheets/")
outPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/stimuli/spreadsheet Gorilla/finalSpreadsheets/")

read.csv(paste0(inPath, "stimuli.csv"), header = T, stringsAsFactors = T) -> fribbles 
read.csv(paste0(spreadsheetPath, "learning_list1.csv"), header = T, stringsAsFactors = T) -> fribbles_used_in_learning 

summary(fribbles_used_in_learning)
summary(fribbles)

fribbles_left <- fribbles[!(fribbles$fribbleID %in% fribbles_used_in_learning$ID),]
fribbles_left <- fribbles_left[!(fribbles_left$label=="bim"),]

ntrials <- 60

contingency <-data.frame(
  randomise_blocks = "",
  randomise_trials = rep(1, ntrials),
  display = as.factor("task"),
  fribbleID = NA,
  labelPresented = as.factor(c(rep("tob", 5), rep("wug",5), rep("dep",5),
                      rep("tob", 5), rep("wug",5), rep("dep",5),
                      rep("tob", 5), rep("wug",5), rep("dep",5),
                      rep("tob", 5), rep("wug",5), rep("dep",5))),
  frequency = as.factor(c(rep("h",30),rep("l",30))),
  trialType = as.factor(c(rep("match",15), rep("mismatch-type1",15),
                          rep("match",15), rep("mismatch-type1", 15))),
  ShowProgressBar = 1
)





dep_high <- paste0(sample(subset(fribbles_left, label == "dep" & frequency == "high")$fribbleID,10),".jpg") 
tob_high <- paste0(sample(subset(fribbles_left, label == "tob" & frequency == "high")$fribbleID,10), ".jpg") 
wug_high <- paste0(sample(subset(fribbles_left, label == "wug" & frequency == "high")$fribbleID,10), ".jpg")  

dep_low <- paste0(sample(subset(fribbles_left, label == "dep" & frequency == "low")$fribbleID,10),".jpg") 
tob_low <- paste0(sample(subset(fribbles_left, label == "tob" & frequency == "low")$fribbleID,10), ".jpg") 
wug_low <- paste0(sample(subset(fribbles_left, label == "wug" & frequency == "low")$fribbleID,10), ".jpg")  

#------------- match ----------------#
contingency[contingency$labelPresented=="tob" & contingency$frequency == "h" & contingency$trialType=="match",]$fribbleID <- tob_high[1:5]
contingency[contingency$labelPresented=="dep" & contingency$frequency == "h" & contingency$trialType=="match",]$fribbleID <- dep_high[1:5]
contingency[contingency$labelPresented=="wug" & contingency$frequency == "h" & contingency$trialType=="match",]$fribbleID <- wug_high[1:5]

contingency[contingency$labelPresented=="tob" & contingency$frequency == "l" & contingency$trialType=="match",]$fribbleID <- tob_low[1:5]
contingency[contingency$labelPresented=="dep" & contingency$frequency == "l" & contingency$trialType=="match" ,]$fribbleID <- dep_low[1:5]
contingency[contingency$labelPresented=="wug" & contingency$frequency == "l" & contingency$trialType=="match",]$fribbleID <- wug_low[1:5]

#------------- mismatch - type1 ----------------#
contingency[contingency$labelPresented=="tob" & contingency$frequency == "h" & contingency$trialType=="mismatch-type1",]$fribbleID <- wug_high[6:10]
contingency[contingency$labelPresented=="dep" & contingency$frequency == "h" & contingency$trialType=="mismatch-type1",]$fribbleID <- tob_high[6:10]
contingency[contingency$labelPresented=="wug" & contingency$frequency == "h" & contingency$trialType=="mismatch-type1",]$fribbleID <- dep_high[6:10]

contingency[contingency$labelPresented=="tob" & contingency$frequency == "l" & contingency$trialType=="mismatch-type1",]$fribbleID <- dep_low[6:10]
contingency[contingency$labelPresented=="dep" & contingency$frequency == "l" & contingency$trialType=="mismatch-type1" ,]$fribbleID <- wug_low[6:10]
contingency[contingency$labelPresented=="wug" & contingency$frequency == "l" & contingency$trialType=="mismatch-type1",]$fribbleID <- tob_low[6:10]

contingency$labelPresented <- paste0('<p style="font-size: 500%;"> ',contingency$labelPresented," </p>")


contingency<- contingency[sample(nrow(contingency)),] #shuffle

contingency<-contingency %>% 
  add_row(display = "instructions", 
          .before = 1)

contingency<-contingency %>% 
  add_row(display = "finish", 
          .before = ntrials+2)

contingency$list <- 1
write.csv(contingency, paste0(outPath, "contingency_list1.csv"), row.names = F, na = "")
