
library(tidyverse)
library(ggpubr)
library(wesanderson)
library(ggstatsplot)

rm(list = ls())

# 
input <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/preProcessed data/pilot2/")
destinationPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/")


#### load eyetracker data - learning ####
prop_of_fix <- read.csv(paste0(input, "eyeTracker_proportions_byBlock_longFormat.csv"), stringsAsFactors = T)


prop_of_fix%>%
  group_by(list,subjID,screen_index,label)%>%
  count(n_distinct(block))%>%
  pivot_wider(names_from = label, values_from = n)

summary(prop_of_fix)

#feature screen: configuration of 5 objects positioned in A,B,C,D,E positions
#note that control condition has always position fixed
#the screen stays for 3000ms or until a button press
#
subset(prop_of_fix, screen_index == "feature" & !(ROI %in% c("A_avg","B_avg","C_avg","D_avg","E_avg"))) -> feature_screen

#let's check how long trials are
hist(feature_screen$time)
fivenum(feature_screen$time)

#I would remove only trials <= 3000 ms as people are encouraged to click on the pictures to proceed through trials
subset(prop_of_fix, screen_index == "feature" & !(ROI %in% c("left","right")) & time <=3000) -> feature_screen
fivenum(feature_screen$time) #OK


#ISI pre and post label last 500ms:

subset(prop_of_fix, screen_index == "ISI_preLabel" & ROI %in% c("left_avg","right_avg") & time <=500) -> ISI_prelabel
subset(prop_of_fix, screen_index == "ISI_postLabel"& ROI %in% c("left_avg","right_avg") & time <=500) -> ISI_postlabel

hist(ISI_prelabel$time)
fivenum(ISI_prelabel$time)

hist(ISI_postlabel$time)
fivenum(ISI_postlabel$time)

#label lasts 1000ms:

subset(prop_of_fix, screen_index == "label" & ROI %in% c("left_avg","right_avg") & time <=1000) -> label_screen

hist(label_screen$time)
fivenum(label_screen$time)


#the columns: "obj_position" and "label_position" gives us the info about where obj/labels 
#have been displayes


summary(feature_screen)

# ---------------- screen 1 (feature screen) -----------------#

feature_screen$where <- "foils"

feature_screen[feature_screen$frequency != "control" & substr(feature_screen$ROI,1,1) == substr(feature_screen$obj_position,1,1),]$where <- "target"
feature_screen[feature_screen$frequency == "control" & substr(feature_screen$ROI,1,5) %in% c("A_avg","B_avg","C_avg","D_avg","E_avg"),]$where <- "target"

feature_screen$where <- as.factor(feature_screen$where)
summary(feature_screen$where)


# ---------------- screen 3 (label screen) -----------------#

ISI_prelabel$where <- "foils"
ISI_prelabel[substr(ISI_prelabel$ROI,1,1) == substr(ISI_prelabel$label_position,1,1),]$where <- "target"

# ISI_prelabel%>%
#   group_by(list, subjID, frequency, where, label, label_position, ROI)%>%
#   summarise(n_distinct(label))%>%
#   pivot_wider(names_from = label, values_from = `n_distinct(label)`) %>%
#   write.csv("check_by_location_by_subj_full.csv", row.names = F, na = "")

ISI_prelabel$where <- as.factor(ISI_prelabel$where)
summary(ISI_prelabel$where)

ISI_postlabel$where <- "foils"
ISI_postlabel[substr(ISI_postlabel$ROI,1,1) == substr(ISI_postlabel$label_position,1,1),]$where <- "target"

ISI_postlabel$where <- as.factor(ISI_postlabel$where)
summary(ISI_postlabel$where)


# ---------------- screen 3 (label screen) -----------------#
label_screen$where <- "foils"
label_screen[substr(label_screen$ROI,1,1) == substr(label_screen$label_position,1,1),]$where <- "target"

label_screen$where <- as.factor(label_screen$where)
summary(label_screen$where)


# label_screen%>%
#   group_by(list, subjID, frequency, where, label, label_position, ROI)%>%
#   summarise(n_distinct(label))%>%
#   pivot_wider(names_from = label, values_from = `n_distinct(label)`) %>%
#   write.csv("check_by_location_by_subj_full_labelScreen.csv", row.names = F, na = "")



summary(feature_screen$time)
summary(ISI_prelabel$time)
summary(label_screen$time)
summary(ISI_postlabel$time)

ISI_prelabel$time + max(round(feature_screen$time)) -> ISI_prelabel$time
label_screen$time +max(round(ISI_prelabel$time)) -> label_screen$time
ISI_postlabel$time +max(round(label_screen$time)) -> ISI_postlabel$time

bind_rows(feature_screen,ISI_prelabel, label_screen, ISI_postlabel)-> targetLooks

targetLooks_minimal<- aggregate(prop ~ subjID + screen_index + block + frequency + where + time, targetLooks, mean)

head(targetLooks_minimal)
summary(targetLooks_minimal)

targetLooks%>%
  group_by(subjID,block)%>%
  summarise(mean(face_conf))%>%
  filter(`mean(face_conf)` <.5) #this value should always be above .5



#### time series plot ####

targetLooks_minimal%>%
  filter(screen_index == "feature")%>%
  ggplot(aes(x= time, y = prop, color = where)) + 
  facet_wrap( ~ frequency+block)+
  ggtitle("feature screen - n=10")+
  stat_summary_bin(geom = "point", fun = "mean", bins = 30) +  
  stat_summary_bin(geom = "line", fun = "mean", bins = 30) +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  #geom_vline(xintercept = c(3000))+
  scale_color_manual(values=c(wes_palette("Darjeeling1", 3, type = c("discrete"))))+
 # scale_x_continuous(limits = range(targetLooks_minimal$time))+
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0,.3)) ->p1
p1

ggsave(paste0(input, "feature_screen.jpg"), height = 6, width = 8)



rt_range <- max(targetLooks_minimal$time)
n_bins <- 30
break_seq <- seq(0, rt_range, rt_range/n_bins)

targetLooks_minimal_range <- targetLooks_minimal %>%
  filter(screen_index %in% c("ISI_preLabel","label","ISI_postLabel")) %>%
  mutate(time_bin = cut(time, breaks = break_seq)) %>%
  group_by(time_bin, frequency, where) %>%
  mutate(time_bin_avg = mean(time, na.rm = T))


count_range <- targetLooks_minimal_range %>%
  group_by(time_bin, frequency, where) %>%
  summarise(subjcount = n_distinct(subjID), #looks good
            totalcount = n())



myColors<-c(wes_palette("Darjeeling1", 2, type = c("discrete")),
            wes_palette("Darjeeling2", 5, type = c("discrete"))[c(2,5)],
            wes_palette("GrandBudapest1", 2, type = c("discrete"))[c(2)])

targetLooks_minimal_range%>%
  ggplot(aes(x= time_bin_avg, y = prop, color = frequency)) + 
  facet_wrap( ~ where)+
  ggtitle(" label screen - n=20")+
  geom_point()
  
  
  
  stat_summary_bin(geom = "point", fun = "mean", bins = 30) +  
  stat_summary_bin(geom = "line", fun = "mean", bins = 30) +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  geom_vline(xintercept = c(3500,4500))+
  scale_color_manual(values=myColors)+
  #scale_x_continuous(limits = range(targetLooks_minimal$time))+
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0,.3)) ->p2
p2



ggsave(paste0(input, "label_screen.jpg"), height = 6, width = 8)

#### heatmaps ####

read.csv(paste0(input, "eyeTracker_intersections.csv"), stringsAsFactors = T) -> eyetracker
read.csv(paste0(input, "ROIs.csv"), stringsAsFactors = T) -> ROIs

ggplot(targetLooks[targetLooks$screen_index=="feature" & targetLooks$list ==3,], aes(x, y))+
  xlim(0,1)+
  ylim(0,1)+
  facet_wrap( label ~ frequency)+
  geom_point()+
  ggtitle("object screen - list 3 - n=2")+
  stat_density2d(aes(fill = ..level..), alpha = .7, geom = "polygon", contour = T)+
  scale_fill_distiller(palette = "Spectral", direction = -1)+
  geom_density_2d(size = 0.25, colour = "black")+
  annotate("rect",xmin=ROIs[ROIs$box=="A",]$x1, xmax=ROIs[ROIs$box=="A",]$x2, ymin=ROIs[ROIs$box=="A",]$y1, ymax=ROIs[ROIs$box=="A",]$y2, fill = "gray", alpha=0.5, color="black") +
  annotate("rect",xmin=ROIs[ROIs$box=="B",]$x1, xmax=ROIs[ROIs$box=="B",]$x2, ymin=ROIs[ROIs$box=="B",]$y1, ymax=ROIs[ROIs$box=="B",]$y2, fill = "gray", alpha=0.5, color="black") +
  annotate("rect",xmin=ROIs[ROIs$box=="C",]$x1, xmax=ROIs[ROIs$box=="C",]$x2, ymin=ROIs[ROIs$box=="C",]$y1, ymax=ROIs[ROIs$box=="C",]$y2, fill = "gray", alpha=0.5, color="black") +
  annotate("rect",xmin=ROIs[ROIs$box=="D",]$x1, xmax=ROIs[ROIs$box=="D",]$x2, ymin=ROIs[ROIs$box=="D",]$y1, ymax=ROIs[ROIs$box=="D",]$y2, fill = "gray", alpha=0.5, color="black") +
  annotate("rect",xmin=ROIs[ROIs$box=="E",]$x1, xmax=ROIs[ROIs$box=="E",]$x2, ymin=ROIs[ROIs$box=="E",]$y1, ymax=ROIs[ROIs$box=="E",]$y2, fill = "gray", alpha=0.5, color="black") +
  theme_bw()+
  theme(legend.position = 'none')

ggsave(paste0(input, "feature_screen_heatMap_list3.jpg"), height = 6, width = 8)

ggplot(targetLooks[targetLooks$screen_index=="label" & targetLooks$list == 3,], aes(x, y))+
  xlim(0,1)+
  ylim(0,1)+
  facet_wrap( label ~ frequency)+
  geom_point()+
  ggtitle("label screen - list 3 - n=2")+
  stat_density2d(aes(fill = ..level..), alpha = .7, geom = "polygon", contour = T)+
  scale_fill_distiller(palette = "Spectral", direction = -1)+
  geom_density_2d(size = 0.25, colour = "black")+
  annotate("rect",xmin=ROIs[ROIs$box=="left",]$x1, xmax=ROIs[ROIs$box=="left",]$x2, ymin=ROIs[ROIs$box=="left",]$y1, ymax=ROIs[ROIs$box=="left",]$y2, fill = "gray", alpha=0.5, color="black") +
  annotate("rect",xmin=ROIs[ROIs$box=="right",]$x1, xmax=ROIs[ROIs$box=="right",]$x2, ymin=ROIs[ROIs$box=="right",]$y1, ymax=ROIs[ROIs$box=="right",]$y2, fill = "gray", alpha=0.5, color="black") +
  theme_bw()+
  theme(legend.position = 'none')


ggsave(paste0(input, "label_screen_heatMap_list3.jpg"), height = 6, width = 8)


eyetracker %>%
  filter(screen_index == "label")%>%
  group_by(list,subjID,label,frequency,label_position)%>%
  summarise(n_distinct(label_position))%>%
  pivot_wider(names_from = label_position, values_from = `n_distinct(label_position)`)%>%
 write.csv("check_list_by_subj.csv")


####  2AFC generalization data ####
read.csv(paste0(input, "2AFC.csv"), stringsAsFactors = T)-> AFC
head(AFC)
summary(AFC)
length(unique(AFC$subjID))

AFC_agg <- with(AFC, aggregate(acc ~ frequency + subjID, FUN=mean))

ggbetweenstats(
  data = AFC_agg,
  x = frequency,
  y = acc,
  title = "2AFC - n=10",
  ggstatsplot.layer = FALSE,
  ggtheme = ggthemes::theme_clean(),
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)+geom_hline(yintercept=.5, linetype="dashed", 
            color = "red", size=1)

ggsave(paste0(input, "2AFC.jpg"), height = 6, width = 8)

####  2AFC pairing data ####
read.csv(paste0(input, "2AFC_pairing.csv"), stringsAsFactors = T)-> pairing
head(pairing)
summary(pairing)

pairing_agg <- with(pairing, aggregate(acc ~ frequency + subjID, FUN=mean))

ggbetweenstats(
  data = pairing_agg,
  x = frequency,
  y = acc,
  title = "2AFC pairing - n=10",
  ggstatsplot.layer = FALSE,
  ggtheme = ggthemes::theme_clean(),
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)+geom_hline(yintercept=.5, linetype="dashed", 
             color = "red", size=1)

ggsave(paste0(input, "2AFC_pairing.jpg"), height = 6, width = 8)


#### contingency data ####
read.csv(paste0(input, "contingency.csv"), stringsAsFactors = T)-> contingency
head(contingency)
summary(contingency)

contingency_agg <- with(contingency, aggregate(resp ~ trialType + frequency + subjID, FUN=mean))

grouped_ggbetweenstats(
  data = contingency_agg,
  x = frequency,
  y = resp,
  grouping.var = trialType,
  ggstatsplot.layer = FALSE,
  ggtheme = ggthemes::theme_clean(),
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE,
  annotation.args = list(title = "contingency task - n=10"),
  ggplot.component =
    list(ggplot2::scale_y_continuous(
      breaks = seq(-10, 10, 2),
      limits = (c(-10, 10))
    ))
) 


ggsave(paste0(input, "contingency.jpg"), height = 5, width = 9)
