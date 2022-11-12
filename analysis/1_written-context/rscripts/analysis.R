library(dplyr)
library(ggplot2)
library(lme4)
library(tidyverse)
library(simr)
library(brms)
theme_set(theme_bw())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7") 
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
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
mos_data<-read.csv("../../data/1_written-context/pilot/1_written-context-pilot-trials.csv")
#######################Participant Exclusion###########################
excluded_subjects <- c()
practice_data=subset(mos_data,block_id == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(data, !is.element(workerid, practice_good_data$workerid))$workerid)
mos_data=subset(mos_data, is.element(workerid, practice_good_data$workerid))
length(unique(mos_data$workerid))
mos_data_acc <- subset(mos_data, acceptability_rating != "NA")
mos_data_bg <- subset(mos_data, bg_response != "NA")
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
  # exclude out practice trials
  filter(block_id != "practice") %>% 
  # combine all good/bad fillers together
  mutate(condition = case_when(condition == "filler_good_1" | condition == "filler_good_2" ~ "Filler Good",
                               condition == "filler_bad_1" | condition == "filler_bad_2" ~ "Filler Bad",
                               condition == "embed_focus" ~ "Embedded Focus",
                               condition == "verb_focus" ~ "Verb Focus")) %>%
  group_by(condition) %>%
  summarize(Mean = mean(acceptability_rating), CILow = ci.low(acceptability_rating),
            CIHigh = ci.high(acceptability_rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>% 
  # reorder the factors
  mutate(condition = fct_relevel(condition, "Filler Good", "Embedded Focus", "Verb Focus", "Filler Bad"))

mos_data_bg_nofill <-  subset(mos_data_bg, condition %in% c("embed_focus","verb_focus"))
mos_data_bg_noprac <- subset(mos_data_bg_nofill, block_id != "practice")
mos_bg_means = mos_data_bg %>% 
  filter(condition %in% c("embed_focus", "verb_focus")) %>%
  # exclude out practice trials
  # filter(block_id != "practice") %>%
  # combine all good/bad fillers together
  # mutate(condition = case_when(condition == "filler_good_1" | condition == "filler_good_2" ~ "Filler Good",
  #                              condition == "filler_bad_1" | condition == "filler_bad_2" ~ "Filler Bad",
  #                              condition == "embed_focus" ~ "Embedded Focus",
  #                              condition == "verb_focus" ~ "Verb Focus")) %>%
  mutate(condition = ifelse(condition=="verb_focus", "Verb Focus", "Embedded Focus")) %>%
  #  1 -> verb focus, 0 -> noun focus; the lower the value, the more backgrounded it is
  mutate(bg = case_when(condition == "Verb Focus" & bg_response == "correct" ~ 1,
                        condition == "Verb Focus" & bg_response == "incorrect" ~ 0,
                        condition == "Embedded Focus" & bg_response == "incorrect" ~ 1,
                        condition == "Embedded Focus" & bg_response == "correct" ~ 0
                        )) %>% 
  group_by(condition) %>% 
  # mutate(bg = ifelse(bg_response == "correct", 1, 0)) %>% 
  summarize(Mean = mean(bg),
            CILow = ci.low(bg),
            CIHigh = ci.high(bg)) %>% 
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)
  # reorder the factors
  # mutate(condition = fct_relevel(condition, "Filler Good", "Embedded Focus", "Verb Focus", "Filler Bad"))

mos_acc_graph <- ggplot(mos_means %>% 
                          filter(condition %in% c("Embedded Focus", "Verb Focus")), aes(x=condition, y=Mean, fill=condition)) +
  geom_bar(stat="identity", width=0.6, aes(color=condition)) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.5,  show.legend = FALSE) +
  scale_fill_manual(values=c("#56B4E9", "#009E73"), name = NULL) +
  theme_bw()+
  xlab("Condition") +
  ylab("Mean Acceptability Rating")+
  scale_color_manual(values=c("#56B4E9", "#009E73"), name=NULL) +
  guides(color = "none")+
  guides(fill = "none")+
  theme(legend.position="bottom",
          axis.text.x = element_text(size=8)) 
  # + scale_x_discrete(labels = c("Embedded focus", "filler bad 1", "filler bad 2", "filler good 1", "filler good 2", "Verb focus"))
# +facet_wrap(~ID) 
mos_acc_graph
ggsave(mos_acc_graph, file="../graphs/mos_acc.pdf", width=4, height=3) 


mos_bg_graph <- ggplot(mos_bg_means, aes(x=condition, y=Mean, fill=condition)) +
  geom_bar(stat="identity", width=0.6, aes(color=condition)) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.5,  show.legend = FALSE) +
  scale_fill_manual(values=c("#56B4E9", "#009E73"), name = NULL) +
  # scale_fill_manual(values=cbPalette, name = NULL) +
  theme_bw()+
  xlab("Condition") +
  ylab("Percentage of Backgrounded\nInterpretation of the Embedded Clause")+
  scale_color_manual(values=c("#56B4E9", "#009E73"),name=NULL) +
  # scale_color_manual(values=cbPalette, name=NULL) +
  guides(color = "none")+
  guides(fill = "none")+
  theme(legend.position="bottom",
        axis.text.x = element_text(size=8),
          axis.title=element_text(size=10)) 
mos_bg_graph
ggsave(mos_bg_graph, file="../graphs/mos_bg_graph.pdf", width=4, height=3) 
