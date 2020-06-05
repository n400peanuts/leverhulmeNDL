rm(list = ls());
library(ndl);
library(ggplot2);
library(knitr);
library(ggpubr);
library(ggsci);
library(grid);
library(gridExtra);
library(cowplot);
library(plyr);
library(wesanderson);
library(data.table);
library(tinytex);
library(RColorBrewer);

#local directory
localGitDir <- 'C:/Users/eva_v/Documents/GitHub/leverhulmeNDL/modelling/'
setwd(localGitDir);


#load RW functions
source("tools/rescorlawagner_FL.R")
source("tools/rescorlawagner_LF.R")
source("tools/prepare_data.R")
source("tools/plot.bar.R")
source("tools/plot_line.R")

myexp = read.csv(paste(localGitDir,"input/exp1.csv", sep = ''));

mystims <- prepare_data(myexp, condition = "suffix")

traceOutcomes=c("dep")
traceCues=c("a", "b", "c", "blue", "red")

saliency <- rep(0.1, length(traceCues));

plot_line(mystims, saliency, "suffix")


mystims <- prepare_data(myexp, condition = "prefix")
traceOutcomes=c("a", "b", "c", "blue", "red")
traceCues=c("dep")

saliency <- rep(0.1, length(traceOutcomes));

plot_line(mystims, saliency, "prefix")

