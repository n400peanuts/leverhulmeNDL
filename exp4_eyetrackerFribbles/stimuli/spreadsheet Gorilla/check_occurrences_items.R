rm(list = ls())
library(tidyverse)

path <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - fribbles/stimuli/spreadsheet Gorilla/finalSpreadsheets/")


read.csv(paste0(path,"learning_list1.csv"), stringsAsFactors = T) -> p2_list1
read.csv(paste0(path,"learning_list2.csv"), stringsAsFactors = T) -> p2_list2
read.csv(paste0(path,"learning_list3.csv"), stringsAsFactors = T) -> p2_list3
read.csv(paste0(path,"pilot1/spreadsheet.csv"), stringsAsFactors = T) -> p1_list1

head(p2_list1$randomise_trials)
summary(p2_list1$frequency)


list1_p2<-p2_list1 %>%
  filter(display=="experiment") %>%
  group_by(randomise_trials)%>%
  count(frequency) %>%
  rename(block = randomise_trials)%>%
  add_column(
    list = 1,
    pilot = 2)

list2_p2<-p2_list2 %>%
  filter(display=="experiment") %>%
  group_by(randomise_trials)%>%
  count(frequency) %>%
  rename(block = randomise_trials)%>%
  add_column(
    list = 2,
    pilot = 2)

list3_p2<-p2_list3 %>%
  filter(display=="experiment") %>%
  group_by(randomise_trials)%>%
  count(frequency) %>%
  rename(block = randomise_trials)%>%
  add_column(
    list = 3,
    pilot = 2)

list1_p1<-p1_list1 %>%
  filter(display=="experiment") %>%
  group_by(randomise_trials)%>%
  count(frequency) %>%
  rename(block = randomise_trials)%>%
  add_column(
    list = 1,
    pilot = 1)


bind_rows(list1_p1, list1_p2, list2_p2, list3_p2) -> table_occurrences_items
write.csv(table_occurrences_items, paste0(path, "table_occurrences_items.csv"), row.names = F, quote = F)

p2_list1 %>%
  filter(display=="experiment") %>%
  group_by(randomise_trials)%>%
  count() 

p1_list1 %>%
  filter(display=="experiment") %>%
  group_by(randomise_trials)%>%
  count() 


p2_list2 %>%
  filter(display=="experiment") %>%
  group_by(randomise_trials)%>%
  count() 

p2_list3 %>%
  filter(display=="experiment") %>%
  group_by(randomise_trials)%>%
  count() 

expeType <- "pilot1"

read.csv(paste0(localDirectory,"rawdata/",expeType,"/","data_exp_45245-v3_task-wcph.csv")) -> learningSpreadsheeet

learningSpreadsheeet %>%
  filter(Zone.Type=="timelimit_screen" & display == "experiment" )%>%
  count() 



eyeTracker -> p1
eyeTracker -> p2
summary(p2)
p2$expeType <- as.factor("pilot2")
p1$expeType <- as.factor("pilot1")
bind_rows(p1,p2) -> eyeTracker
summary(eyeTracker)



eyeTracker %>%
  group_by(blocks, frequency, subjID,list,expeType) %>%
  summarise(n_distinct(trial))%>%
  pivot_wider(names_from = c(expeType,list), values_from = `n_distinct(trial)`)%>%
  write.csv(paste0(path,"table_check_bysubj.csv"), row.names = F, quote = F)

eyeTracker[eyeTracker$expeType=="pilot1",]$list <-1
eyeTracker %>%
  group_by(blocks, frequency, list,expeType) %>%
  summarise(n_distinct(trial))%>%
  pivot_wider(names_from = c(expeType,list), values_from = `n_distinct(trial)`)%>%
  write.csv(paste0(path,"table_check_agg.csv"), row.names = F, quote = F)


length(eyeTracker[eyeTracker$screen_index==2 & eyeTracker$time<=400,]$time)
length(bothpilot[bothpilot$screen_index==2 & bothpilot$time>400,]$time)