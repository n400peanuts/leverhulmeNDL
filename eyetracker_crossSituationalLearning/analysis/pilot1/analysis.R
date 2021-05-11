
library(tidyverse)
library(trajr)
library(ggpubr)
library(wesanderson)
library(ggstatsplot)

rm(list = ls())

# 
input <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/rawdata/eyetracking/")
output <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/preProcessed data/")
destinationPath <- c("C:/Users/eva_v/Nexus365/Elizabeth Wonnacott - Eva_Liz_Leverhulme/leverhulmeNDL/eyetracker - cross situational learning/")


#### load eyetracker data - learning ####
prop_of_fix <- read.csv(paste0(input, "eyeTracker_proportions_longFormat.csv"), stringsAsFactors = T)

subset(prop_of_fix, screen_index == "feature" & !(ROI %in% c("tob","wug")) & time <=3000) -> feature_screen
subset(prop_of_fix, screen_index == "label" & time <=1000) -> label_screen

spreadsheet <-readxl::read_xlsx(paste0(destinationPath,"stimuli/gorillaSpreadsheets/spreadsheet_12Apr.xlsx")) #spreadsheet that I loaded on Gorilla with the list of the trials

#wug - high frequency: B
#wug - low frequency: A
#tob - high frequency: E
#tob - low frequency: D

# ---------------- screen 1 (feature screen) -----------------#
feature_screen$where <- "foils"
feature_screen[feature_screen$ROI=="B" & feature_screen$label == "wug" & feature_screen$frequency=="high",]$where <- "target"
feature_screen[feature_screen$ROI=="A" & feature_screen$label == "wug" & feature_screen$frequency=="low",]$where <- "target"
feature_screen[feature_screen$ROI=="E" & feature_screen$label == "tob" & feature_screen$frequency=="high",]$where <- "target"
feature_screen[feature_screen$ROI=="D" & feature_screen$label == "tob" & feature_screen$frequency=="low",]$where <- "target"
feature_screen[feature_screen$ROI=="C",]$where <- "center"

feature_screen$where <- as.factor(feature_screen$where)
summary(feature_screen$where)


# ---------------- screen 3 (label screen) -----------------#
label_screen$where <- "foils"
label_screen[label_screen$ROI=="wug" & label_screen$label == "wug",]$where <- "target"
label_screen[label_screen$ROI=="tob" & label_screen$label == "tob",]$where <- "target"
label_screen[label_screen$ROI=="C",]$where <- "center"

label_screen$where <- as.factor(label_screen$where)
summary(label_screen$where)

label_screen$time +max(round(feature_screen$time)) -> label_screen$time

rbind(feature_screen,label_screen)-> targetLooks

targetLooks_minimal<- aggregate(prop ~ frequency + where + time,targetLooks, mean)

#### time series plot ####

ggplot(targetLooks_minimal, 
       aes(x= time, y = prop, color = where)) + 
  facet_wrap( ~ frequency)+
  ggtitle("fribble + label screen - n=7")+
  stat_summary_bin(geom = "point", fun = "mean", bins = 30) +  
  stat_summary_bin(geom = "line", fun = "mean", bins = 30) +  
  stat_summary_bin(fun.data = mean_se , geom = "errorbar", width = 0)+
  stat_smooth(aes(group =where, color = where),method = "gam", span = .2, se = T)+
  geom_vline(xintercept = c(3000))+
  scale_color_manual(values=c(wes_palette("Darjeeling1", 3, type = c("discrete"))))+
  scale_x_continuous(limits = range(targetLooks_minimal$time))+
  theme_minimal() +
  theme(legend.position="bottom") +
  coord_cartesian(ylim=c(0,1)) 

ggsave(paste0(output, "fribble+label_screen.jpg"), height = 6, width = 8)

#### heatmaps ####

read.csv(paste0(output, "eyeTracker.csv"), stringsAsFactors = T) -> eyetracker
read.csv(paste0(output, "ROIs.csv"), stringsAsFactors = T) -> ROIs

ggplot(eyetracker[eyetracker$screen_index=="feature",], aes(x, y))+
  xlim(0,1)+
  ylim(0,1)+
  facet_wrap( label ~ frequency)+
  geom_point()+
  ggtitle("fribble screen - n=7")+
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


ggplot(eyetracker[eyetracker$screen_index=="label",], aes(x, y))+
  xlim(0,1)+
  ylim(0,1)+
  facet_wrap( label ~ frequency)+
  geom_point()+
  ggtitle("label screen - n=7")+
  stat_density2d(aes(fill = ..level..), alpha = .7, geom = "polygon", contour = T)+
  scale_fill_distiller(palette = "Spectral", direction = -1)+
  geom_density_2d(size = 0.25, colour = "black")+
  annotate("rect",xmin=ROIs[ROIs$box=="tob",]$x1, xmax=ROIs[ROIs$box=="tob",]$x2, ymin=ROIs[ROIs$box=="tob",]$y1, ymax=ROIs[ROIs$box=="tob",]$y2, fill = "gray", alpha=0.5, color="black") +
  annotate("rect",xmin=ROIs[ROIs$box=="wug",]$x1, xmax=ROIs[ROIs$box=="wug",]$x2, ymin=ROIs[ROIs$box=="wug",]$y1, ymax=ROIs[ROIs$box=="wug",]$y2, fill = "gray", alpha=0.5, color="black") +
  theme_bw()+
  theme(legend.position = 'none')


ggsave(paste0(output, "label_screen.jpg"), height = 6, width = 8)


####  2AFC generalization data ####
read.csv(paste0(output, "2AFC.csv"), stringsAsFactors = T)-> AFC
head(AFC)
summary(AFC)

AFC_agg <- with(AFC, aggregate(acc ~ frequency + subjID, FUN=mean))

ggbetweenstats(
  data = AFC_agg,
  x = frequency,
  y = acc,
  title = "2AFC - n=7",
  ggstatsplot.layer = FALSE,
  ggtheme = ggthemes::theme_clean(),
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)+geom_hline(yintercept=.5, linetype="dashed", 
            color = "red", size=1)

ggsave(paste0(output, "2AFC.jpg"), height = 6, width = 8)

####  2AFC pairing data ####
read.csv(paste0(output, "2AFC_pairing.csv"), stringsAsFactors = T)-> pairing
head(pairing)
summary(pairing)

pairing_agg <- with(pairing, aggregate(acc ~ frequency + subjID, FUN=mean))

ggbetweenstats(
  data = pairing_agg,
  x = frequency,
  y = acc,
  title = "2AFC pairing - n=7",
  ggstatsplot.layer = FALSE,
  ggtheme = ggthemes::theme_clean(),
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
)+geom_hline(yintercept=.5, linetype="dashed", 
             color = "red", size=1)

ggsave(paste0(output, "2AFC_pairing.jpg"), height = 6, width = 8)


#### contingency data ####

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
  annotation.args = list(title = "contingency task - n=7"),
  ggplot.component =
    list(ggplot2::scale_y_continuous(
      breaks = seq(-10, 10, 2),
      limits = (c(-10, 10))
    ))
) 


p1<-ggbetweenstats(
  data = contingency_agg[contingency_agg$trialType=="match",],
  x = frequency,
  y = resp,
  ggstatsplot.layer = FALSE,
  ggtheme = ggthemes::theme_clean(),
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE,
  annotation.args = list(title = "contingency task - n=7")
) + ggtitle("match") + ylim(-10,10)

p2<-ggbetweenstats(
  data = contingency_agg[contingency_agg$trialType=="mismatch",],
  x = frequency,
  y = resp,
  ggstatsplot.layer = FALSE,
  ggtheme = ggthemes::theme_clean(),
  results.subtitle = FALSE,
  pairwise.comparisons = FALSE
) + ggtitle("mismatch") + ylim(-10,10)


ggarrange(p1,p2)
ggsave(paste0(output, "contingency.jpg"), height = 5, width = 9)
