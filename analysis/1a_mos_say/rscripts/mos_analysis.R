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
freq_data <- read.csv("../../../data/exp2_freq.csv")

##Add log SCR score to verbs###
freq_data <- freq_data %>% 
  rename(vff = coca_v_sc, # using coca
         google_vff = google_v_sc) %>%
  select(-c(google_vff, verb, v)) %>% 
  # rename(vff = google_v_sc, # using google book
  #        coca_vff = coca_v_sc) %>%
  # select(-c(coca_vff, verb, v)) %>% 
  rename(verb=adverb)

mos_data <- left_join(mos_data, freq_data, by="verb") %>% 
  mutate(scr = log(scr),
         vff = log(vff))

#######################Participant Exclusion##########################
###Exclude Subjects###
length(unique(mos_data$workerid))
excluded_subjects <- c(86) ## non-english native speaker

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
filler_verbs = c("believe", "confirm", "expect", "guess", "hope", "imply", "reveal", "spectulate", "suggest", "suspect", "think") # spectulate->speculate, corrected in the actual stimuli
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
  # group_by(condition) %>%
  # summarize(Mean = mean(acceptability_rating),
  #           CILow = ci.low(acceptability_rating),
  #           CIHigh = ci.high(acceptability_rating)) %>%
  # ungroup() %>%
  group_by(workerid) %>% 
  mutate(mean = mean(acceptability_rating),
         sd = sd(acceptability_rating),
         z_rating = (acceptability_rating - mean)/sd) %>% 
  ungroup() %>% 
  group_by(condition) %>%
  summarize(Mean = mean(z_rating),
            CILow = ci.low(z_rating),
            CIHigh = ci.high(z_rating)) %>%
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
  # scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1)) +
  scale_y_continuous(name="Z-score Acceptability Rating", limits=c(-1.5, 1)) +
  # geom_signif(comparisons=list(c("Good Filler", "Adverb Focus")), annotations="***",y_position = 0.9) +
  # geom_signif(comparisons=list(c("Embedded Focus", "Adverb Focus")), annotations="***",y_position = 0.8) +
  # geom_signif(comparisons=list(c("Bad Filler", "Adverb Focus")), annotations="***",y_position = 0.7) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14))
                      # + scale_x_discrete(labels = c("Embedded focus", "filler bad 1", "filler bad 2", "filler good 1", "filler good 2", "Adverb Focus"))
                      # +facet_wrap(~ID) 
mos_acc_graph
ggsave(mos_acc_graph, file="../graphs/main/mos_acc_z-scored.pdf", width=4, height=4)

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
        axis.title=element_text(size=12)) # +
  # geom_signif(comparisons = list(c("Embedded Focus", "Adverb Focus")),
  #             annotations="***",y_position = 0.93)
mos_bg_graph
ggsave(mos_bg_graph, file="../graphs/main/mos_bg.pdf", width=3, height=4)
 
 
#######SCR plot############
mos_scr_means <- mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus")) %>%
  mutate(condition = ifelse(condition=="verb_focus", "Adverb Focus", "Embedded Focus")) %>% 
  group_by(verb, condition) %>%
  summarise( ACC = mean(acceptability_rating), 
              SCR = mean(scr))
 
mos_scr_plot <- ggplot(mos_scr_means %>% 
                         mutate(condition = fct_relevel(condition, "Embedded Focus", "Adverb Focus"),
                                label = case_when(verb %in% c("calmly", "dryly", "wearily") & condition == "Adverb Focus" ~ verb,
                                                  verb %in% c("softly", "gently", "quietly", "sternly", "ruefully", "cheerfully", "loudly", "wistfully", "bluntly") & condition =="Embedded Focus" ~ verb,
                                                  TRUE ~ "")),
                        aes(x = SCR, y = ACC, 
                            color = condition, 
                            fill=condition)) +
  geom_point() +
  geom_smooth(method = "lm") +
  # geom_text(size=3, color="black", alpha=0.6, hjust="inward", vjust="inward")+
  geom_label_repel(aes(label=label),color="black",fill="white",max.overlaps=Inf,seed=1235)+
                   # nudge_x = ifelse(mos_scr_means$condition == "Adverb Focus",0.3,0))+
  geom_line(aes(group=verb),
             color = "black",
             alpha = 0.6,
             linetype = "dashed") +
  # scale_x_continuous(expand=expansion(mult = 0.08)) +
  xlab("Log-transformed SCR score")+
  ylab("Mean Acceptability Rating") +
  scale_color_manual(values=c("#56B4E9", "#009E73"), 
                     labels=c("Embedded Focus", "Adverb Focus"),
                     name = "Condition") +
  scale_fill_manual(values=c("#56B4E9", "#009E73"), 
                    labels=c("Embedded Focus", "Adverb Focus"),
                    name = "Condition") +
  theme(legend.position="top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18)) 
mos_scr_plot
ggsave(mos_scr_plot, file="../graphs/main/scr_single_label.pdf", width=7, height=6)


#######VFF plot############
mos_vff_means = mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus")) %>%
  mutate(condition = ifelse(condition=="verb_focus", "Adverb Focus", "Embedded Focus")) %>% 
  group_by(verb, condition) %>%
  summarise( ACC = mean(acceptability_rating), 
             VFF = mean(vff))

mos_vff_plot <- ggplot(mos_vff_means %>% 
                         mutate(condition = fct_relevel(condition, "Embedded Focus", "Adverb Focus"),
                                label = case_when(verb %in% c("dryly", "wearily","wistfully") & condition == "Adverb Focus" ~ verb,
                                                  verb %in% c("calmly", "softly", "gently", "quietly", "sternly", "ruefully", "cheerfully", "loudly", "bluntly") & condition =="Embedded Focus" ~ verb,
                                                  TRUE ~ "")),
                       aes(x = VFF, y = ACC, 
                           color = condition, 
                           fill=condition, 
                           label=verb)) +
  geom_point() +
  geom_smooth(method = "lm") +
  # geom_text(size=3, color="black", alpha=0.6, hjust="inward", vjust="inward")+
  geom_label_repel(aes(label=verb),color="black",fill="white")+
  geom_line(aes(group=verb),
            color = "black",
            alpha = 0.6,
            linetype = "dashed") +
  # scale_x_continuous(expand=expansion(mult = 0.08)) +
  xlab("Log-transformed predicate-frame frequency")+
  ylab("Mean Acceptability Rating") +
  scale_color_manual(values=c("#56B4E9", "#009E73"), 
                     labels=c("Embedded Focus", "Adverb Focus"),
                     name = "Condition") +
  scale_fill_manual(values=c("#56B4E9", "#009E73"), 
                    labels=c("Embedded Focus", "Adverb Focus"),
                    name = "Condition") +
  theme(legend.position="top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18))
mos_vff_plot
ggsave(mos_vff_plot, file="../graphs/main/vff.pdf", width=7, height=6)

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
#####acceptability analysis######
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "filler_bad"] <- "filler_bad"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "filler_good"] <- "filler_good"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "say"] <- "say"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "mos"] <- "mos"
mos_data_acc_noprac$prim_cond<- as.factor(mos_data_acc_noprac$prim_cond)
mos_data_acc_noprac$prim_cond<-relevel(mos_data_acc_noprac$prim_cond, ref = "say")

acc_model <- lmer(acceptability_rating ~ prim_cond + 
                    (1+prim_cond|workerid),
                  data = mos_data_acc_noprac)
summary(acc_model)

######BG analysis###########
mos_data_bg_nofill<- mos_data_bg_nofill %>%
  # mutate(cond = ifelse(condition=="verb_focus", "Adverb Focus", "Embedded Focus")) %>%
  #  1 -> Adverb Focus, 0 -> noun focus; the lower the value, the more backgrounded it is
  mutate(bg = case_when(bg_response == "embed" ~ 0,
                        bg_response == "verb" ~ 1
  ))
mos_data_bg_nofill$bg <- as.numeric(mos_data_bg_nofill$bg)
mos_data_bg_nofill$condition <- as.factor(mos_data_bg_nofill$condition)
contrasts(mos_data_bg_nofill$condition)=contr.sum(2)
bg_model <- glmer(bg~condition+
                    (1+condition|workerid)+
                    (1+condition|item_id),
                  family = "binomial",
                  data=mos_data_bg_nofill)
summary(bg_model)

######SCR analysis#######
# mean-center scr scores: center=TRUE, scale=TRUE (divided by sd)
mos_data_acc$scr <- scale(mos_data_acc$scr, center=TRUE)
mos_scr_model_data <- mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus"))

mos_scr_model_data$condition <- as.factor(mos_scr_model_data$condition)
contrasts(mos_scr_model_data$condition) <- contr.sum(2)
model_scr <- lmer(acceptability_rating ~ condition * scr + 
                    (1+condition|item_id)+ # should not include scr in the by-item random effect 
                    (1+condition * scr|workerid),
                  data = mos_scr_model_data)
summary(model_scr)

######VFF analysis#######
# mean-center vff: center=TRUE, scale=TRUE (divided by sd)
mos_data_acc$vff <- scale(mos_data_acc$vff, center=TRUE)
mos_vff_model_data <- mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus"))

mos_vff_model_data$condition <- as.factor(mos_vff_model_data$condition)
contrasts(mos_vff_model_data$condition) <- contr.sum(2)
model_vff <- lmer(acceptability_rating ~ condition * vff + 
                    (1+condition|item_id)+ # should not include scr in the by-item random effect 
                    (1+condition * vff|workerid),
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
