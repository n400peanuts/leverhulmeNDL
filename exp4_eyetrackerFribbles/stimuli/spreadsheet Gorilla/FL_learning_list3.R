rm(list = ls())
library(tidyverse)

inPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/stimuli/")
outPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/stimuli/spreadsheet Gorilla/finalSpreadsheets/")

read.csv(paste0(inPath, "stimuli.csv"), header = T, stringsAsFactors = T) -> fribbles 
summary(fribbles)

# we got 15 high frequency fribbles per category and 5 low frequency fribbles per category

hf <- data.frame(
  display = "experiment",
  ID = paste0(unlist(list(
    sample(fribbles[fribbles$label=="dep" & fribbles$frequency=="high",]$fribbleID,15),
    sample(fribbles[fribbles$label=="tob" & fribbles$frequency=="high",]$fribbleID,15),
    sample(fribbles[fribbles$label=="wug" & fribbles$frequency=="high",]$fribbleID,15))),".jpg"),
  label = as.factor(c(rep("dep",15) ,rep("tob",15), rep("wug",15))),
  frequency = "h"
)

lf <- data.frame(
  display = "experiment",
  ID = paste0(unlist(list(
    sample(fribbles[fribbles$label=="dep" & fribbles$frequency=="low",]$fribbleID,5),
    sample(fribbles[fribbles$label=="tob" & fribbles$frequency=="low",]$fribbleID,5),
    sample(fribbles[fribbles$label=="wug" & fribbles$frequency=="low",]$fribbleID,5))),".jpg"),
  label = as.factor(c(rep("dep",5) ,rep("tob",5), rep("wug",5))),
  frequency = "l"
)


control <- data.frame(
  display = "experiment",
  ID = paste0(unlist(list(sample(fribbles[fribbles$label=="bim",]$fribbleID,20, replace=T))),".jpg"),
  label = as.factor(c(rep("bim",20))),
  frequency = "control"
)

hf <- hf %>% 
  slice(rep(1:n(), each=2))
lf <- lf %>% 
  slice(rep(1:n(), each=2))
control <- control %>% 
  slice(rep(1:n(), each=2))

control$buttonLeft <- c(rep('<p style="font-size: 500%;">Click for next trial</p>',13), rep('',nrow(control)-13))
control$buttonCenter <- c(rep('',13), rep('<p style="font-size: 500%;">Click for next trial</p>',nrow(control)-26), rep('',13))
control$buttonRight <- c(rep('',13), rep('',nrow(control)-26), rep('<p style="font-size: 500%;">Click for next trial</p>',13))

control$leftLabel <- c(rep('<p style="font-size: 700%;">bim</p>',13), rep('',nrow(control)-13))
control$centerLabel <- c(rep('',13), rep('<p style="font-size: 700%;">bim</p>',nrow(control)-26), rep('',13))
control$rightLabel <- c(rep('',13), rep('',nrow(control)-26), rep('<p style="font-size: 700%;">bim</p>',13))
control$ANSWER <- c(rep('l',13), rep('c',nrow(control)-26), rep('r',13))


hf <- hf %>%
  mutate(
    ANSWER = case_when(
      label == 'tob' ~ 'l',
      label == 'wug' ~ 'c',
      label == 'dep' ~ 'r'
    ),
    leftLabel = case_when(
      label == 'tob' ~ '<p style="font-size: 700%;">tob</p>',
      label == 'wug' ~ '',
      label == 'dep' ~ ''),
    
    rightLabel = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '',
      label == 'dep' ~ '<p style="font-size: 700%;">dep</p>' ),
    
    centerLabel = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '<p style="font-size: 700%;">wug</p>',
      label == 'dep' ~ '' 
    ),
    
    buttonLeft = case_when(
      label == 'tob' ~ '<p style="font-size: 500%;">Click for next trial</p>',
      label == 'wug' ~ '',
      label == 'dep' ~ ''),
    
    buttonRight = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '',
      label == 'dep' ~ '<p style="font-size: 500%;">Click for next trial</p>' ),
    
    buttonCenter = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '<p style="font-size: 500%;">Click for next trial</p>',
      label == 'dep' ~ '' 
    )
  ) 


lf <- lf %>%
  mutate(
    ANSWER = case_when(
      label == 'tob' ~ 'l',
      label == 'wug' ~ 'c',
      label == 'dep' ~ 'r'),
    leftLabel = case_when(
      label == 'tob' ~ '<p style="font-size: 700%;">tob</p>',
      label == 'wug' ~ '',
      label == 'dep' ~ ''),
    
    rightLabel = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '',
      label == 'dep' ~ '<p style="font-size: 700%;">dep</p>' ),
    
    centerLabel = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '<p style="font-size: 700%;">wug</p>',
      label == 'dep' ~ '' 
    ),
    
    buttonLeft = case_when(
      label == 'tob' ~ '<p style="font-size: 500%;">Click for next trial</p>',
      label == 'wug' ~ '',
      label == 'dep' ~ ''),
    
    buttonRight = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '',
      label == 'dep' ~ '<p style="font-size: 500%;">Click for next trial</p>' ),
    
    buttonCenter = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '<p style="font-size: 500%;">Click for next trial</p>',
      label == 'dep' ~ '' 
    )
  ) 


learningList <- rbind(hf,lf, control) ; rownames(learningList) <- NULL

learningList$randomise_blocks <- ""
learningList$randomise_trials <- ""
learningList$TimedSection <- ""
learningList$pause <- ""
learningList$list <- 3

learningList<-learningList[sample(nrow(learningList)),] #shuffle


learningList<-learningList %>% 
  add_row(display = "screen calibration", .before = 1)

learningList<-learningList %>% 
  add_row(display = "instructions1", .before = 2)

learningList <- learningList %>% 
  add_row(tibble_row( display = "practice",  
                      ID = sample(hf[hf$label=='tob' & hf$frequency=='h',]$ID,1), 
                      label = 'tob',
                      buttonRight = '',
                      rightLabel = '',
                      buttonLeft = '<p style="font-size: 500%;">Click for next trial</p>',
                      buttonCenter = '',
                      leftLabel = '<p style="font-size: 700%;">tob</p>',
                      centerLabel = ''), .before = 3)

learningList <- learningList %>% 
  add_row(display = "practice", .before = 4, 
          ID = sample(hf[hf$label=='wug' & hf$frequency=='h',]$ID,1), 
          label = 'wug',
          buttonRight = '',
          rightLabel = '',
          buttonLeft = '',
          buttonCenter = '<p style="font-size: 500%;">Click for next trial</p>',
          leftLabel = '',
          centerLabel = '<p style="font-size: 700%;">wug</p>')

learningList <- learningList %>% 
  add_row(display = "practice", .before = 5, 
          ID = sample(hf[hf$label=='dep' & hf$frequency=='h',]$ID,1), 
          label = 'dep',
          buttonRight = '<p style="font-size: 500%;">Click for next trial</p>',
          rightLabel = '<p style="font-size: 700%;">dep</p>',
          buttonLeft = '',
          buttonCenter = '',
          leftLabel = '',
          centerLabel = '')

learningList <- learningList %>% 
  add_row(display = "practice", .before = 6, 
          ID = sample(lf[lf$label=='dep' & lf$frequency=='l',]$ID,1), 
          label = 'dep',
          buttonRight = '<p style="font-size: 500%;">Click for next trial</p>',
          rightLabel = '<p style="font-size: 700%;">dep</p>',
          buttonLeft = '',
          buttonCenter = '',
          leftLabel = '',
          centerLabel = '')

learningList <- learningList %>% 
  add_row(display = "practice", .before = 7, 
          ID = sample(lf[lf$label=='tob' & lf$frequency=='l',]$ID,1), 
          label = 'tob',
          buttonRight = '',
          rightLabel = '',
          buttonLeft = '<p style="font-size: 500%;">Click for next trial</p>',
          buttonCenter = '',
          leftLabel = '<p style="font-size: 700%;">tob</p>',
          centerLabel = '')

learningList <- learningList %>% 
  add_row(display = "practice", .before = 8, 
          ID = sample(lf[lf$label=='wug' & lf$frequency=='l',]$ID,1), 
          label = 'wug',
          buttonRight = '',
          rightLabel = '',
          buttonLeft = '',
          buttonCenter = '<p style="font-size: 500%;">Click for next trial</p>',
          leftLabel = '',
          centerLabel = '<p style="font-size: 700%;">wug</p>')

learningList<-learningList %>% 
  add_row(display = "instructions2", .before = 9)

learningList<-learningList %>% 
  add_row(display = "break",
          pause = "# Break n.1 of 5 - Take your time to rest. When you're ready click the button below.", 
          .before = 40)

learningList<-learningList %>% 
  add_row(display = "break", 
          pause = "# Break n.2 of 5 - Take your time to rest. When you're ready click the button below.", 
          .before = 66)

learningList<-learningList %>% 
  add_row(display = "break", 
          pause = "# Break n.3 of 5 - Take your time to rest. When you're ready click the button below.", 
          .before = 93)

learningList<-learningList %>% 
  add_row(display = "break", 
          pause = "# Break n.4 of 5 - Take your time to rest. When you're ready click the button below.", 
          .before = 123)

learningList<-learningList %>% 
  add_row(display = "break", 
          pause = "# Break n.5 of 5 - Take your time to rest. Only few trials left!", 
          .before = 150)

# convert to factor everything that is labeled as "character"
learningList[sapply(learningList, is.character)] <- 
  lapply(learningList[sapply(learningList, is.character)], as.factor)


write.csv(learningList, file = paste0(outPath,"learning_list3.csv"), row.names = F, na = "")


