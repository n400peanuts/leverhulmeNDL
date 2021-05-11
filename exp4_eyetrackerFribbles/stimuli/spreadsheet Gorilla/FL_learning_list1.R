rm(list = ls())
library(tidyverse)

inPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/stimuli/")
outPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/stimuli/")

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

control$buttonLeft <- c(rep('Click for next trial',13), rep('',nrow(control)-13))
control$buttonCenter <- c(rep('',13), rep('Click for next trial',nrow(control)-26), rep('',13))
control$buttonRight <- c(rep('',13), rep('',nrow(control)-26), rep('Click for next trial',13))

control$leftLabel <- c(rep('bim',13), rep('',nrow(control)-13))
control$centerLabel <- c(rep('',13), rep('bim',nrow(control)-26), rep('',13))
control$rightLabel <- c(rep('',13), rep('',nrow(control)-26), rep('bim',13))


learningList <- rbind(hf,lf,control)

hf <- hf %>%
  mutate(
    leftLabel = case_when(
    label == 'tob' ~ '',
    label == 'wug' ~ 'wug',
    label == 'dep' ~ ''),
    
    rightLabel = case_when(
      label == 'tob' ~ 'tob',
      label == 'wug' ~ '',
      label == 'dep' ~ '' ),
    
    centerLabel = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '',
      label == 'dep' ~ 'dep' 
    ),
    
    buttonLeft = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ 'Click for next trial',
      label == 'dep' ~ ''),
    
    buttonRight = case_when(
      label == 'tob' ~ 'Click for next trial',
      label == 'wug' ~ '',
      label == 'dep' ~ '' ),
    
    buttonCenter = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '',
      label == 'dep' ~ 'Click for next trial' 
    )
  ) 


lf <- lf %>%
  mutate(
    leftLabel = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ 'wug',
      label == 'dep' ~ ''),
    
    rightLabel = case_when(
      label == 'tob' ~ 'tob',
      label == 'wug' ~ '',
      label == 'dep' ~ '' ),
    
    centerLabel = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '',
      label == 'dep' ~ 'dep' 
    ),
    
    buttonLeft = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ 'Click for next trial',
      label == 'dep' ~ ''),
    
    buttonRight = case_when(
      label == 'tob' ~ 'Click for next trial',
      label == 'wug' ~ '',
      label == 'dep' ~ '' ),
    
    buttonCenter = case_when(
      label == 'tob' ~ '',
      label == 'wug' ~ '',
      label == 'dep' ~ 'Click for next trial' 
    )
  ) 

learningList <- rbind(hf,lf, control) ; rownames(learningList) <- NULL

learningList$randomise_blocks <- ""
learningList$randomise_trials <- ""
learningList$TimedSection <- ""
learningList$ANSWER <- ""
learningList$pause <- ""

learningList<-learningList[sample(nrow(learningList)),] #shuffle


learningList<-learningList %>% 
  add_row(display = "screen calibration", .before = 1)

learningList<-learningList %>% 
  add_row(display = "instructions1", .before = 2)

learningList <- learningList %>% 
  add_row(tibble_row( display = "practice",  
          ID = sample(hf[hf$label=='tob' & hf$frequency=='h',]$ID,1), 
          label = 'tob',
          buttonRight = 'Click for next trial',
          rightLabel = 'tob',
          buttonLeft = '',
          buttonCenter = '',
          leftLabel = '',
          centerLabel = ''), .before = 3)

learningList <- learningList %>% 
  add_row(display = "practice", .before = 4, 
          ID = sample(hf[hf$label=='wug' & hf$frequency=='h',]$ID,1), 
          label = 'wug',
          buttonRight = '',
          rightLabel = '',
          buttonLeft = 'Click for next trial',
          buttonCenter = '',
          leftLabel = 'wug',
          centerLabel = '')

learningList <- learningList %>% 
  add_row(display = "practice", .before = 5, 
          ID = sample(hf[hf$label=='dep' & hf$frequency=='h',]$ID,1), 
          label = 'dep',
          buttonRight = '',
          rightLabel = '',
          buttonLeft = '',
          buttonCenter = 'Click for next trial',
          leftLabel = '',
          centerLabel = 'dep')

learningList <- learningList %>% 
  add_row(display = "practice", .before = 6, 
          ID = sample(lf[lf$label=='dep' & lf$frequency=='l',]$ID,1), 
          label = 'dep',
          buttonRight = '',
          rightLabel = '',
          buttonLeft = '',
          buttonCenter = 'Click for next trial',
          leftLabel = '',
          centerLabel = 'dep')

learningList <- learningList %>% 
  add_row(display = "practice", .before = 7, 
          ID = sample(lf[lf$label=='tob' & lf$frequency=='l',]$ID,1), 
          label = 'tob',
          buttonRight = 'Click for next trial',
          rightLabel = 'tob',
          buttonLeft = '',
          buttonCenter = '',
          leftLabel = '',
          centerLabel = '')

learningList <- learningList %>% 
  add_row(display = "practice", .before = 8, 
          ID = sample(lf[lf$label=='wug' & lf$frequency=='l',]$ID,1), 
          label = 'wug',
          buttonRight = '',
          rightLabel = '',
          buttonLeft = 'Click for next trial',
          buttonCenter = '',
          leftLabel = 'wug',
          centerLabel = '')

learningList<-learningList %>% 
  add_row(display = "instructions2", .before = 9)

learningList<-learningList %>% 
  add_row(display = "break",
          pause = "# Break n.1 of 5 - Take your time to rest. When you're ready click the button below.", 
          .before = 31)

learningList<-learningList %>% 
  add_row(display = "break", 
          pause = "# Break n.2 of 5 - Take your time to rest. When you're ready click the button below.", 
          .before = 51)

learningList<-learningList %>% 
  add_row(display = "break", 
          pause = "# Break n.3 of 5 - Take your time to rest. When you're ready click the button below.", 
          .before = 71)

learningList<-learningList %>% 
  add_row(display = "break", 
          pause = "# Break n.4 of 5 - Take your time to rest. When you're ready click the button below.", 
          .before = 91)

learningList<-learningList %>% 
  add_row(display = "break", 
          pause = "# Break n.5 of 5 - Take your time to rest. Only few trials left!", 
          .before = 111)

# convert to factor everything that is labeled as "character"
learningList[sapply(learningList, is.character)] <- 
  lapply(learningList[sapply(learningList, is.character)], as.factor)


write.csv(learningList, file = paste0(outPath,"spreadsheet_Gorilla.csv"), row.names = F)


