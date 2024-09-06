library(dplyr)
library(ggplot2)
library(ggsignif)
library(lme4)
library(lmerTest)
library(emmeans)
library(tidyverse)
library(simr)
library(brms)
library(bootstrap)
library(ggrepel)

`%notin%` <- Negate(`%in%`)
theme_set(theme_bw())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#######################Load Data ######################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('locative_helpers.R')
mos_data<-read.csv("../../../data/1a_mos_say/main/1a_mos_say-trials.csv")

mos_verbs <- c("whisper", "stammer", "mumble", "mutter", "scream", "yell",
               "groan", "whine", "murmur", "shriek", "moan", "shout")

exp1a_mos_data <- read.csv("../../../data/1a_mos_say/main/1a_mos_say-trials.csv")


##Add log SCR score to verbs###
mos_data$scr[mos_data$verb == "moan"] = log(0.019933555)
mos_data$scr[mos_data$verb == "mumble"] = log(0.028169014)
mos_data$scr[mos_data$verb == "murmur"] = log(0.011627907)
mos_data$scr[mos_data$verb == "mutter"] =log(0.015695067)
mos_data$scr[mos_data$verb == "scream"] =log(0.018957346)
mos_data$scr[mos_data$verb == "shout"] =log(0.009748172)
mos_data$scr[mos_data$verb == "shriek"] =log(0.01119403)
mos_data$scr[mos_data$verb == "stammer"] =log(0.009090909)
mos_data$scr[mos_data$verb == "whine"] =log(0.035433071)
mos_data$scr[mos_data$verb == "whisper"] =log(0.012575889)
mos_data$scr[mos_data$verb == "yell"]=log(0.010537992)
mos_data$scr[mos_data$verb == "groan"]=log(0.00539924)

mos_data$vff[mos_data$verb == "stammer"] = -6.841637508
mos_data$vff[mos_data$verb == "shout"] = -5.603800653
mos_data$vff[mos_data$verb == "yell"] =	-5.987162775
mos_data$vff[mos_data$verb == "murmur"] =	-6.230622674
mos_data$vff[mos_data$verb == "whisper"] =	-5.53165267
mos_data$vff[mos_data$verb == "mutter"] =	-6.092051478
mos_data$vff[mos_data$verb == "scream"] =	-5.793174124
mos_data$vff[mos_data$verb == "mumble"] =	-6.395773947
mos_data$vff[mos_data$verb == "whine"] =	-6.549750892


#######################Participant Exclusion##########################
###Exclude Subjects###
length(unique(mos_data$workerid))
excluded_subjects <- c(86, 88) ## non-english native speaker, or no response for the language status

mos_data <- subset(mos_data, !is.element(mos_data$workerid, excluded_subjects))
length(unique(mos_data$workerid))

practice_data=subset(mos_data,block_id == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(mos_data, !is.element(workerid, practice_good_data$workerid))$workerid)
                mos_data=subset(mos_data, is.element(workerid, practice_good_data$workerid))
                length(unique(mos_data$workerid))
                
length(unique(practice_good_data$workerid))

mos_data_acc <- subset(mos_data, acceptability_rating != "NA")
mos_data_bg <- subset(mos_data, bg_response != "NA")
mos_data_acc$acceptability_rating <- as.numeric(mos_data_acc$acceptability_rating)
filler_data = subset(mos_data_acc, condition == "filler_good")
ungram_data = subset(mos_data_acc, condition == "filler_bad")
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
length(eligible_subjects)

# sanity check for data exclusion
is.element(excluded_subjects,eligible_subjects)
mos_data = subset(mos_data, workerid %in% eligible_subjects)
# write.csv(mos_data,"../../../data/1a_mos_say/main/exp1a_after_exclusion.csv", row.names = FALSE)
##################################Getting data ready for plotting and analysis#############################################
# Data cleaning    
## acceptability
mos_data_overall <- mos_data %>% 
     filter(block_id != "practice") %>%
     filter(condition %in% c("say","mos"))%>%
     mutate(bg = case_when(bg_response == "embed" ~ 0,
                           bg_response == "verb" ~ 1)) %>%
      group_by(item_id, condition, verb) %>%
      summarise(BG = mean(bg, na.rm=TRUE), 
                AC = mean(acceptability_rating, na.rm=TRUE))
                
mos_data_nofill <-  subset(mos_data_acc, condition %in% c("say","mos"))
mos_data_acc_noprac <- subset(mos_data_acc, block_id != "practice")

# mos_data_acc_noprac <- mos_data_acc_noprac %>% 
#   group_by(workerid) %>% 
#   mutate(mean = mean(acceptability_rating),
#          sd = sd(acceptability_rating),
#          z_rating = (mean-acceptability_rating)/sd) %>% 
#   ungroup()

# accept tasks for each verb
mos_verbs = c("groan", "moan", "mumble", "murmur", "mutter", "scream", "shout", "shriek", "stammer", "whine", "whisper", "yell")
filler_verbs = c("believe", "confirm", "expect", "guess", "hope", "imply", "reveal", "spectulate", "suggest", "suspect", "think") # spectulate->speculate, corrected in the actual exp
mos_acc_verbs <- mos_data_acc %>%
  filter(block_id != "practice") %>%
  filter(condition == "mos" | condition == "filler_good" | condition == "say") %>%
  group_by(verb, condition) %>%
  summarize(Mean = mean(acceptability_rating),
            CILow = ci.low(acceptability_rating),
            CIHigh = ci.high(acceptability_rating))  %>% 
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

mos_acc_means = mos_data_acc %>%
  filter(block_id != "practice") %>% # exclude practice trials
  mutate(condition = case_when(condition == "filler_good" ~ "Good Filler",
                               condition == "filler_bad" ~ "Bad Filler",
                               condition == "say" ~ "Say",
                               condition == "mos" ~ "MoS")) %>%
  group_by(condition) %>%
  summarize(Mean = mean(acceptability_rating),
            CILow = ci.low(acceptability_rating),
            CIHigh = ci.high(acceptability_rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
# reorder the factors
  mutate(condition = fct_relevel(condition, "Good Filler", "Say", "MoS", "Bad Filler"))

## backgroundedness
mos_data_bg_nofill <-  subset(mos_data_bg, condition %in% c("say","mos"))
mos_data_bg_noprac <- subset(mos_data_bg_nofill, block_id != "practice")

# count the number of ag tasks for each verb
mos_bg_verbs <- mos_data_bg_noprac %>% 
  filter(condition == "mos") %>%  
  group_by(verb) %>% 
  summarize(count = n())

mos_bg_means = mos_data_bg %>%
               filter(condition %in% c("say", "mos")) %>%
                  #  1 -> verb Focus, 0 -> embed focus;lower  value,  more backgrounded
              mutate(bg = case_when(bg_response == "embed" ~ 0,
                                    bg_response == "verb" ~ 1)) %>%
              group_by(condition) %>%
              summarize(Mean = mean(bg),
                        CILow = ci.low(bg),
                        CIHigh = ci.high(bg)) %>%
              ungroup() %>%
              mutate(YMin=Mean-CILow,
                     YMax=Mean+CIHigh)


##########Acceptability plot########################
mos_acc_graph <- ggplot(mos_acc_means, 
                        aes(x=condition, y=Mean, fill=condition)) +
  geom_bar(stat="identity", width=0.8, aes(color=condition)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,  show.legend = FALSE) +
  scale_fill_manual(values=cbPalette, name = NULL, guide="none") +
  theme_bw() +
  xlab("Condition") +
  scale_color_manual(values=cbPalette, name=NULL, guide="none") +
  scale_x_discrete(labels=c("Good Filler"="Good\nFiller", 
                            "Say"="Say",
                            "MoS"="MoS",
                            "Bad Filler"="Bad\nFiller")) +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1)) +
  # scale_y_continuous(name="Z-score Acceptability Rating", limits=c(-1.5, 1)) +
  geom_signif(comparisons=list(c("Bad Filler", "Say")), annotations="***",y_position = 0.95) +
  geom_signif(comparisons=list(c("Say", "MoS")), annotations="***",y_position = 0.85) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14))
                      # + scale_x_discrete(labels = c("Embedded focus", "filler bad 1", "filler bad 2", "filler good 1", "filler good 2", "Adverb Focus"))
                      # +facet_wrap(~ID) 
mos_acc_graph
ggsave(mos_acc_graph, file="../graphs/main/mos_acc.pdf", width=3, height=3)

### fillers by item
mos_acc_verb_graph <- ggplot(mos_acc_verbs, 
                        aes(x=verb, y=Mean, fill=condition)) +
  geom_bar(stat="identity", width=0.8, aes(color=condition)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,  show.legend = FALSE) +
  scale_fill_manual(values=cbPalette, name = NULL, guide="none") +
  theme_bw() +
  xlab("Condition") +
  scale_color_manual(values=cbPalette, name=NULL, guide="none") +
  scale_x_discrete(labels=c("Good Filler"="Good\nFiller", 
                            "Say"="Say",
                            "MoS"="MoS")) +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1)) +
  theme(axis.text=element_text(size=10),
        axis.text.x = element_text(angle=60,vjust = 0.5, hjust=0.5),
        axis.title=element_text(size=14))
mos_acc_verb_graph

###########BG question plot#######################
mos_bg_graph <- ggplot(mos_bg_means %>% 
                         mutate(condition = fct_relevel(condition, "say", "mos")), aes(x=condition, y=Mean, fill=condition)) +
  geom_bar(stat="identity", width=0.8, aes(color=condition)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,  show.legend = FALSE) +
  scale_fill_manual(values=c("#56B4E9", "#009E73"), name = NULL, guide="none") +
  theme_bw() +
  xlab("Condition") +
  scale_color_manual(values=c("#56B4E9", "#009E73"),name=NULL, guide="none") +
  scale_x_discrete(labels=c("say"="Say",
                            "mos"="MoS")) +
  scale_y_continuous(name="Proportion of Backgrounded\nInterpretation of the Embedded Object", limits=c(0, 1)) + 
  theme(legend.position="bottom",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12))  +
  geom_signif(comparisons = list(c("say", "mos")),
              annotations="***",y_position = 0.7)
mos_bg_graph
ggsave(mos_bg_graph, file="../graphs/main/mos_bg.pdf", width=2, height=3)
 
 
#######SCR plot############
mos_scr_means <- mos_data_acc %>% 
  filter(condition %in% c("mos")) %>%
  group_by(verb, condition) %>%
  summarise(ACC = mean(acceptability_rating), 
            SCR = mean(scr))

mos_scr_plot <- ggplot(mos_scr_means,
                        aes(x = SCR, y = ACC)) +
  geom_point() +
  geom_smooth(method = "lm", color="black") +
  # geom_text(aes(label=verb),size=3, color="black", alpha=0.6, hjust="inward", vjust="inward")+
  geom_label_repel(aes(label=verb),color="black",fill="white",
                   box.padding=0.1,
                   segment.size=0.2)+
  geom_line(aes(group=verb),
             color = "black",
             alpha = 0.6,
             linetype = "dashed") +
  # scale_x_continuous(expand=expansion(mult = 0.08)) +
  xlab("Log-transformed SCR score")+
  ylab("Mean Acceptability Rating") +
  theme(legend.position="top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18)) 
mos_scr_plot
ggsave(mos_scr_plot, file="../graphs/main/scr.pdf", width=6, height=4)


#######VFF plot############
mos_vff_means = mos_data_acc %>% 
  filter(condition %in% c("mos")) %>%
  group_by(verb, condition) %>%
  summarise(ACC = mean(acceptability_rating), 
             VFF = mean(vff)) %>% 
  filter(!is.na(VFF))

mos_vff_plot <- ggplot(mos_vff_means,
                       aes(x = VFF, y = ACC)) +
  geom_point() +
  geom_smooth(method = "lm",color="black") +
  # geom_text(aes(label=verb),size=3, color="black", alpha=0.6, hjust="inward", vjust="inward")+
  geom_label_repel(aes(label=verb),color="black",fill="white",
                   box.padding=0.1,
                   segment.size=0.2)+
  geom_line(aes(group=verb),
            color = "black",
            alpha = 0.6,
            linetype = "dashed") +
  # scale_x_continuous(expand=expansion(mult = 0.08)) +
  xlab("Log-transformed verb-frame frequency")+
  ylab("Mean Acceptability Rating") +
  theme(legend.position="top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18))
mos_vff_plot
ggsave(mos_vff_plot, file="../graphs/main/vff.pdf", width=6, height=4)

##############trial_order plot#############
mos_trial_means = mos_data_acc %>% 
  filter(condition %in% c("mos", "say") ) %>%
  group_by(trial_num, condition) %>%
  summarise(Mean = mean(acceptability_rating))
 
 
mos_trial_plot <- ggplot(mos_trial_means,
                         aes(x = trial_num, 
                             y = Mean,
                             color = condition,
                             fill=condition)) +
  geom_point(alpha=0.6) +
  scale_fill_manual(values=cbPalette[2:4], name="Condition", labels=c("Embedded Focus", "Adverb Focus")) +
  scale_color_manual(values=cbPalette[2:4], name="Condition", labels=c("Embedded Focus", "Adverb Focus")) +
  geom_smooth(method = "lm") +
  xlab("Presentation order") +
  ylab("Mean acceptability")
 
mos_trial_plot
ggsave(mos_trial_plot, file="../graphs/main/trial_plot.pdf", width=4, height=3)

#########################Stats##################################
######BG analysis###########
#  1 -> verb focus, 0 -> noun focus; the lower the value, the more backgrounded it is
mos_data_bg_nofill<- mos_data_bg_nofill %>%
  mutate(bg = case_when(bg_response == "embed" ~ 0,
                        bg_response == "verb" ~ 1
  ))
mos_data_bg_nofill$bg <- as.numeric(mos_data_bg_nofill$bg)
mos_data_bg_nofill$condition <- as.factor(mos_data_bg_nofill$condition)
mos_data_bg_nofill$condition <- relevel(mos_data_bg_nofill$condition, ref="mos")
contrasts(mos_data_bg_nofill$condition)=contr.sum(2)
levels(mos_data_bg_nofill$condition)

# failed to converge with the random effect structure: (1+condition|workerid) + (1|item_id)
bg_model <- glmer(bg~condition+
                    (1+condition|workerid)+
                    (1|item_id),
                  family = "binomial",
                  data=mos_data_bg_nofill)
summary(bg_model)

#####acceptability analysis######
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "filler_bad"] <- "filler_bad"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "filler_good"] <- "filler_good"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "say"] <- "say"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "mos"] <- "mos"
mos_data_acc_noprac$prim_cond<- as.factor(mos_data_acc_noprac$prim_cond)
mos_data_acc_noprac$prim_cond<-relevel(mos_data_acc_noprac$prim_cond, ref = "say")

# failed to converge with the random effect structure (0+prim_cond|workerid) and (1+prim_cond|workerid)
acc_model <- lmer(acceptability_rating ~ prim_cond + 
                    # (1|item_id)+
                    (1+prim_cond|workerid),
                  data = mos_data_acc_noprac)
summary(acc_model)

######SCR analysis#######
# mean-center scr scores: center=TRUE, scale=TRUE (divided by sd)
mos_data_acc$scr <- scale(mos_data_acc$scr, center=TRUE)
mos_scr_model_data <- mos_data_acc %>% 
  filter(condition == "mos")

model_scr <- lmer(acceptability_rating ~ scr + 
                    (1|item_id)+ # should not include scr in the by-item random effect 
                    (1+scr|workerid),
                  data = mos_scr_model_data)
summary(model_scr)

######VFF analysis#######
# mean-center vff: center=TRUE, scale=TRUE (divided by sd)
mos_data_acc$vff <- scale(mos_data_acc$vff, center=TRUE)
mos_vff_model_data <- mos_data_acc %>% 
  filter(condition=="mos")

mos_vff_model_data$condition <- as.factor(mos_vff_model_data$condition)
model_vff <- lmer(acceptability_rating ~ vff + 
                    (1|item_id)+ # should not include scr in the by-item random effect 
                    (1+vff|workerid),
                  data = mos_vff_model_data)
summary(model_vff)

####### Satiation analysis#########
mos_trial_data <- mos_data_acc %>%
                  filter(condition %in% c("embed_focus", "verb_focus") ) %>%
                  mutate(condition = as.factor(condition),
                         condition = relevel(condition, ref = "verb_focus"))

contrasts(mos_trial_data$condition) <- contr.sum(2)       

model_trial <- lmer(acceptability_rating ~ condition * trial_num + 
                      (1+ condition*trial_num|item_id)+
                      (1+ condition*trial_num|workerid),
                    data = mos_trial_data)
summary(model_trial)
