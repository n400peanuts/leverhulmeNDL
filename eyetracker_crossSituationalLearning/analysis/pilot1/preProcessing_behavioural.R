rm(list = ls())
# 
input <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/rawdata/")
output <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/preProcessed data/")
destinationPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/")

#### load 2AFC generalization data ####
df <- list.files(paste0(input)) #folder where I story my eyetracking datafiles
df <- df[grepl("dkv7",df)]

data <- NULL
for (i in 1:length(df)){
  read.csv(paste0(input, df[i]), stringsAsFactors = T)-> temp
  data <- bind_rows(temp,data)
};
rm(temp)

summary(data)

columnsIwantTokeep<- data[c('Task.Name','Participant.Private.ID', 'display','Trial.Number','Zone.Type',
                              'Screen.Name', 'Response', 'label','frequency')]

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

AFC$resp <- factor(AFC$resp, levels=c("tob", "wug"))

ifelse(AFC$resp == AFC$labelPresented,1,0)-> AFC$acc

write.csv(AFC, paste0(output, "2AFC.csv"), row.names = F, quote = F)


#### load 2AFC pairing data ####

df <- list.files(paste0(input)) #folder where I story my eyetracking datafiles
df <- df[grepl("kp7j",df)]

data <- NULL
for (i in 1:length(df)){
  read.csv(paste0(input, df[i]), stringsAsFactors = T)-> temp
  data <- bind_rows(temp,data)
};
rm(temp)

summary(data)

columnsIwantTokeep<- data[c('Task.Name','Participant.Private.ID', 'display','Trial.Number','Zone.Type',
                            'Screen.Name', 'Response', 'ObjectType')]

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
pairing$frequency <- "altro"
pairing[substr(pairing$ObjectType,1,1)=="H",]$frequency <- "high" 
pairing[substr(pairing$ObjectType,1,1)=="L",]$frequency <- "low" 
pairing[substr(pairing$ObjectType,1,1)=="C",]$frequency <- "centralObj" 

pairing$frequency <- as.factor(pairing$frequency)

pairing[substr(pairing$ObjectType,2,4)=="tob",]$correctLabel <- "tob" 
pairing[substr(pairing$ObjectType,2,4)=="wug",]$correctLabel <- "wug" 
pairing[substr(pairing$ObjectType,9,10)=="01",]$correctLabel <- "tob" 
pairing[substr(pairing$ObjectType,9,10)=="02",]$correctLabel <- "wug" 

pairing$correctLabel <- as.factor(pairing$correctLabel)


summary(pairing$correctLabel)
summary(pairing$frequency)

table(pairing$frequency, pairing$correctLabel)

pairing$resp <- factor(pairing$resp, levels=c("tob", "wug"))

ifelse(pairing$resp == pairing$correctLabel,1,0)-> pairing$acc

write.csv(pairing, paste0(output, "2AFC_pairing.csv"), row.names = F, quote = F)


#### load contingency task data ####
df <- list.files(paste0(input)) #folder where I story my eyetracking datafiles
df <- df[grepl("qs46",df)]

data <- NULL
for (i in 1:length(df)){
  read.csv(paste0(input, df[i]), stringsAsFactors = T)-> temp
  data <- bind_rows(temp,data)
};
rm(temp)

summary(data)

columnsIwantTokeep<- data[c('Task.Name','Participant.Private.ID', 'display','Trial.Number','Zone.Type',
                            'Screen.Name', 'Response', 'label','frequency','trialType')]

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

