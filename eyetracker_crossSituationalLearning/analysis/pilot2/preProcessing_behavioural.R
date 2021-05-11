#-----------------------------------------#
#--------------- PILOT 2 -----------------#
#-----------------------------------------#
library(tidyverse)

rm(list = ls())
# 
input <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/rawdata/pilot2/")
output <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/preProcessed data/pilot2/")
destinationPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/")
setwd(destinationPath)

#### load 2AFC generalization data ####
df <- list.files(paste0(input)) #folder where I story my eyetracking datafiles
gorillaNames <-c("4zyk","dkv7","slz1","lvet","1zus","qmqf","24uf","e3fe","exnw","9tqt")
df <- df[grep(paste(gorillaNames,collapse="|"), df)]

data <- do.call(bind_rows,lapply(paste0(input,df),read.csv, stringsAsFactors = T))

summary(data)

columnsIwantTokeep<- data[c('Task.Name','Participant.Private.ID', 'display','Trial.Number','Zone.Type',
                              'Screen.Name', 'Response', 'label','frequency','list')]

rowsIwantTokeep <- c("Feature screen")


AFC <- columnsIwantTokeep %>% 
  filter(Screen.Name == rowsIwantTokeep & display == "2AFC layout") %>%
  rename(subjID = Participant.Private.ID, 
         task = Task.Name,
         resp = Response, 
         trial = Trial.Number,
         labelPresented = label)

AFC$display <- NULL; AFC$Screen.Name <- NULL; AFC$Zone.Type <- NULL #we don't need these columns anymore
rm(rowsIwantTokeep, columnsIwantTokeep)
droplevels(AFC)-> AFC
summary(AFC)

AFC$resp <- factor(AFC$resp, levels=c("tob", "wug","bim"))

levels(AFC$resp)
ifelse(AFC$resp == AFC$labelPresented,1,0)-> AFC$acc

summary(AFC)
write.csv(AFC, paste0(output, "2AFC.csv"), row.names = F, quote = F)

AFC%>%
  group_by(frequency, subjID,list)%>%
  summarise(mean(acc, na.rm = T))%>%
  pivot_wider(names_from = frequency,values_from = `mean(acc, na.rm = T)`)

AFC%>%
  group_by(frequency)%>%
  summarise(mean(acc, na.rm = T))%>%
  pivot_wider(names_from = frequency,values_from = `mean(acc, na.rm = T)`)

#### load 2AFC pairing data ####

df <- list.files(paste0(input)) #folder where I story my eyetracking datafiles
gorillaNames <-c("kp7j","5d9z","4zf9","kc64","5osi","pe3w","18vk","inux","hy16","d61c")
df <- df[grep(paste(gorillaNames,collapse="|"), df)]

data <- do.call(bind_rows,lapply(paste0(input,df),read.csv, stringsAsFactors = T))

summary(data)

columnsIwantTokeep<- data[c('Task.Name','Participant.Private.ID', 'display','Trial.Number','Zone.Type',
                            'Screen.Name', 'Response', 'ObjectType','frequency',"list")]

rowsIwantTokeep <- c("Feature screen")


pairing <- columnsIwantTokeep %>% 
  filter(Screen.Name == rowsIwantTokeep & display == "2AFC layout") %>%
  rename(subjID = Participant.Private.ID, 
         task = Task.Name,
         resp = Response, 
         trial = Trial.Number)

pairing$display <- NULL; pairing$Screen.Name <- NULL; pairing$Zone.Type <- NULL #we don't need these columns anymore
rm(rowsIwantTokeep, columnsIwantTokeep)
droplevels(pairing)-> pairing
summary(pairing)

pairing$correctLabel <- "altro"
pairing[substr(pairing$ObjectType,2,4)=="tob",]$correctLabel <- "tob" 
pairing[substr(pairing$ObjectType,2,4)=="wug",]$correctLabel <- "wug" 
pairing[substr(pairing$ObjectType,11,12)=="_1",]$correctLabel <- "tob" 
pairing[substr(pairing$ObjectType,11,12)=="_2",]$correctLabel <- "wug" 
pairing[substr(pairing$ObjectType,1,7)=="control",]$correctLabel <- "bim" 

pairing$correctLabel <- as.factor(pairing$correctLabel)

summary(pairing$correctLabel)
summary(pairing)

table(pairing$frequency, pairing$correctLabel)
pairing$resp <- factor(pairing$resp, levels=c("tob", "wug","bim"))

ifelse(pairing$resp == pairing$correctLabel,1,0)-> pairing$acc

write.csv(pairing, paste0(output, "2AFC_pairing.csv"), row.names = F, quote = F)

pairing%>%
  group_by(frequency,subjID,list)%>%
  summarise(mean(acc, na.rm = T))%>%
  pivot_wider(names_from = frequency,values_from = `mean(acc, na.rm = T)`)

pairing%>%
  group_by(frequency)%>%
  summarise(mean(acc, na.rm = T))%>%
  pivot_wider(names_from = frequency,values_from = `mean(acc, na.rm = T)`)

#### load contingency task data ####
df <- list.files(paste0(input)) #folder where I story my eyetracking datafiles
gorillaNames <-c("qs46","j8an","bfg1","b1jp","46fj","5rdq","45j8","ilwx","9fpf","rqqo")
df <- df[grep(paste(gorillaNames,collapse="|"), df)]

data <- do.call(bind_rows,lapply(paste0(input,df),read.csv, stringsAsFactors = T))

summary(data)

columnsIwantTokeep<- data[c('Task.Name','Participant.Private.ID', 'display','Trial.Number','Zone.Type',
                            'Screen.Name', 'Response', 'label','frequency','trialType','list')]

rowsIwantTokeep <- c("response_slider_endValue")


contingency <- columnsIwantTokeep %>% 
  filter(Zone.Type == rowsIwantTokeep & display == "task") %>%
  rename(subjID = Participant.Private.ID, 
         task = Task.Name,
         resp = Response, 
         trial = Trial.Number,
         labelPresented = label)

contingency$display <- NULL; contingency$Screen.Name <- NULL; contingency$Zone.Type <- NULL #we don't need these columns anymore
rm(rowsIwantTokeep, columnsIwantTokeep)
droplevels(contingency)-> contingency
summary(contingency)

write.csv(contingency, paste0(output, "contingency.csv"), row.names = F, quote = F)

contingency%>%
  group_by(frequency, trialType,list)%>%
  summarise(mean(resp))%>%
  pivot_wider(names_from = frequency,values_from = `mean(resp)`)

contingency%>%
  group_by(frequency, trialType)%>%
  summarise(mean(resp))%>%
  pivot_wider(names_from = frequency,values_from = `mean(resp)`)
