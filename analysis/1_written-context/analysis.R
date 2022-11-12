library(dplyr)
library(ggplot2)
library(gtable)
library(lme4)
library(tidyverse)
library(simr)
library(lmerTest)
library(brms)
library(bootstrap)
library(ggpubr)
# # install mixedpower
# if (!require("devtools")) {
#   install.packages("devtools", dependencies = TRUE)}
# devtools::install_github("DejanDraschkow/mixedpower") # mixedpower is hosted on GitHub
# # load library
# library(mixedpower)
# `%notin%` <- Negate(`%in%`)
#Functions for 95%CI
theta <- function(x,xdata,na.rm=T) {mean(xdata[x],na.rm=na.rm)}
ci.low <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.025,na.rm=na.rm)}
ci.high <- function(x,na.rm=T) {
  quantile(bootstrap(1:length(x),1000,theta,x,na.rm=na.rm)$thetastar,.975,na.rm=na.rm)}
#######################Load Data ######################################
mos_data<-read.csv("C:/Users/aldos/Downloads/1_written-context-trials.csv")
#######################Participant Exclusion###########################
excluded_subjects <- c()
practice_data=subset(mos_data,block_id == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(data, !is.element(workerid, practice_good_data$workerid))$workerid)
mos_data=subset(mos_data, is.element(workerid, practice_good_data$workerid))
length(unique(mos_data$workerid))
mos_data_acc <- subset(mos_data, acceptability_rating != "NA")
mos_data_acc$acceptability_rating <- as.numeric(mos_data_acc$acceptability_rating)
filler_data = subset(mos_data_acc, condition %in% c("filler_good_1", "filler_good_2" ))
ungram_data = subset(mos_data_acc, condition %in% c("filler_bad_1", "filler_bad_2" ))
filler_by_subject = aggregate(filler_data[,"acceptability_rating"],list(filler_data$workerid), mean)
ungram_by_subject = aggregate(ungram_data[,"acceptability_rating"],list(ungram_data$workerid), mean)
names(filler_by_subject)[names(filler_by_subject) == "Group.1"] <- "subject"
names(filler_by_subject)[names(filler_by_subject) == "x"] <- "fill_avg"
names(ungram_by_subject)[names(ungram_by_subject) == "Group.1"] <- "subject"
names(ungram_by_subject)[names(ungram_by_subject) == "x"] <- "ungram_avg"
all_filler <- merge(ungram_by_subject, filler_by_subject, by.x="subject")
eligible_subjects = c()
for (i in (1:length(all_filler$subject))){
  row = all_filler[i,]
  if (row$ungram_avg < row$fill_avg){
    eligible_subjects <- c(eligible_subjects, row$subject)
  }
}
data = subset(data, workerid %in% eligible_subjects)
##################################Plots#############################################
mos_data_nofill <-  subset(mos_data_acc, condition %in% c("embed_focus","verb_focus"))
mos_data_acc_noprac <- subset(mos_data_nofill, block_id != "practice")
mos_means = mos_data_acc %>%
  group_by(condition) %>%
  summarize(Mean = mean(acceptability_rating), CILow = ci.low(acceptability_rating),
            CIHigh = ci.high(acceptability_rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
ggplot(mos_means, aes(x=condition, y=Mean, fill=condition)) +
  geom_bar(stat="identity",aes(color=condition)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.5,  show.legend = FALSE) +
  scale_fill_manual(values=cbPalette, name = NULL) +
  theme_bw()+
  xlab("Context Focus Condition") +
  ylab("Target Mean Acceptability Rating")+
  scale_color_manual(values=cbPalette,name=NULL) +
  guides(color = FALSE)+
  guides(fill = FALSE)+
  theme(legend.position="bottom")
#+facet_wrap(~ID)