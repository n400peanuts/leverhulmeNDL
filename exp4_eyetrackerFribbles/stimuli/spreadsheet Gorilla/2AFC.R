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


ntrials <- 56

twoAFC <-data.frame(
  randomise_blocks = NA,
  randomise_trials = rep(1, ntrials),
  display = "task",
  ANSWER = NA,
  label = as.factor(c(rep(" bim",8), rep(" tob", 16), rep(" wug",16), rep(" dep",16))),
  button1 = NA,
  button2 = NA,
  button3 = NA,
  button4 = NA,
  frequency = as.factor(c(rep("control_high",4),
                          rep("control_low",4),
               rep("high",8),rep("low",8),
               rep("high",8),rep("low",8),
               rep("high",8),rep("low",8))),
  ShowProgressBar = 1
)

twoAFC$label <- paste0('<p style="font-size: 700%;">',twoAFC$label,"</p>")

for (i in 1:ntrials){
   idc<- sample(6:9,4)

   if (twoAFC[i,]$frequency == "high"){
     dep_high <- paste0(sample(subset(fribbles_left, label == "dep" & frequency == "high")$fribbleID,1),".jpg") 
     tob_high <- paste0(sample(subset(fribbles_left, label == "tob" & frequency == "high")$fribbleID,1), ".jpg") 
     wug_high <- paste0(sample(subset(fribbles_left, label == "wug" & frequency == "high")$fribbleID,1), ".jpg")  
     bim <- paste0(sample(subset(fribbles_left, label == "bim")$fribbleID,1), ".jpg") 
     fribbles_left<-fribbles_left[!(fribbles_left$fribbleID %in% c(dep_high, bim, tob_high, wug_high)),]
     
     twoAFC[i,idc[1]] <-  dep_high
     twoAFC[i,idc[2]] <-  tob_high
     twoAFC[i,idc[3]] <-  wug_high
     twoAFC[i,idc[4]] <-  bim
     
   } else if (twoAFC[i,]$frequency == "low"){
     dep_low <- paste0(sample(subset(fribbles_left, label == "dep" & frequency == "low")$fribbleID,1), ".jpg") 
     tob_low <- paste0(sample(subset(fribbles_left, label == "tob" & frequency == "low")$fribbleID,1), ".jpg")  
     wug_low <- paste0(sample(subset(fribbles_left, label == "wug" & frequency == "low")$fribbleID,1), ".jpg")  
     bim <- paste0(sample(subset(fribbles_left, label == "bim")$fribbleID,1), ".jpg") 
     fribbles_left<-fribbles_left[!(fribbles_left$fribbleID %in% c(dep_low, bim, tob_low, wug_low)),]
     
     twoAFC[i,idc[1]] <-  dep_low
     twoAFC[i,idc[2]] <-  tob_low
     twoAFC[i,idc[3]] <-  wug_low
     twoAFC[i,idc[4]] <-  bim
     
   } else if (twoAFC[i,]$frequency == "control_low"){
     dep_low <- paste0(sample(subset(fribbles_left, label == "dep" & frequency == "low")$fribbleID,1), ".jpg") 
     tob_low <- paste0(sample(subset(fribbles_left, label == "tob" & frequency == "low")$fribbleID,1), ".jpg")  
     wug_low <- paste0(sample(subset(fribbles_left, label == "wug" & frequency == "low")$fribbleID,1), ".jpg")  
     bim <- paste0(sample(subset(fribbles_left, label == "bim")$fribbleID,1), ".jpg") 
     fribbles_left<-fribbles_left[!(fribbles_left$fribbleID %in% c(dep_low, bim, tob_low, wug_low)),]
     
     twoAFC[i,idc[1]] <-  dep_low
     twoAFC[i,idc[2]] <-  tob_low
     twoAFC[i,idc[3]] <-  wug_low
     twoAFC[i,idc[4]] <-  bim
   } else if (twoAFC[i,]$frequency == "control_high"){
     dep_high <- paste0(sample(subset(fribbles_left, label == "dep" & frequency == "high")$fribbleID,1), ".jpg") 
     tob_high <- paste0(sample(subset(fribbles_left, label == "tob" & frequency == "high")$fribbleID,1), ".jpg")  
     wug_high <- paste0(sample(subset(fribbles_left, label == "wug" & frequency == "high")$fribbleID,1), ".jpg")  
     bim <- paste0(sample(subset(fribbles_left, label == "bim")$fribbleID,1), ".jpg") 
     fribbles_left<-fribbles_left[!(fribbles_left$fribbleID %in% c(dep_high, bim, tob_high, wug_high)),]
     
     twoAFC[i,idc[1]] <-  dep_high
     twoAFC[i,idc[2]] <-  tob_high
     twoAFC[i,idc[3]] <-  wug_high
     twoAFC[i,idc[4]] <-  bim
   }
}

twoAFC<- twoAFC[sample(nrow(twoAFC)),] #shuffle

twoAFC<-twoAFC %>% 
  add_row(display = "instructions", 
          .before = 1)

twoAFC<-twoAFC %>% 
  add_row(display = "finish", 
          .before = ntrials+2)

twoAFC$list <- 1
write.csv(twoAFC, paste0(outPath, "2afc_test_list1.csv"), row.names = F, quote= F, na = "")


twoAFC