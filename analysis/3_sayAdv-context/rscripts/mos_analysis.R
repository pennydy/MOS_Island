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
library(colorspace)

`%notin%` <- Negate(`%in%`)
theme_set(theme_bw())
# grayscale palette: "#1B1B1B" "#6D6D6D" "#BEBEBE" "#F9F9F9"
grayPalette <- sequential_hcl(4, palette="Grays")
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#######################Load Data ######################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('locative_helpers.R')
mos_data<-read.csv("../../../data/3_sayAdv-context/main/3_sayAdv-context-trials.csv")
freq_data <- read.csv("../../../data/exp2_freq.csv")
#######################Participant Exclusion###########################
##Add log SCR score to verbs##
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


###Exclude Subjects###
length(unique(mos_data$workerid))
excluded_subjects <- c(1391) ## excluded due to bilingual speaker status
excluded_subjects <- c(excluded_subjects, 1348) ## excluded due to second attempt

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
length(eligible_subjects)

# sanity check for data exclusion
is.element(excluded_subjects,eligible_subjects)
mos_data = subset(mos_data, workerid %in% eligible_subjects)
##################################Getting data ready for plotting and analysis#############################################
# Data cleaning      
mos_data_overall <- mos_data %>% 
     filter(block_id != "practice") %>%
     filter(condition %in% c("embed_focus","verb_focus"))%>%
     mutate(bg = case_when(bg_response == "embed" ~ 0,
                           bg_response == "verb" ~ 1)) %>%
      group_by(item_id, condition) %>%
      summarise(BG = mean(bg, na.rm=TRUE), 
                AC = mean(acceptability_rating, na.rm=TRUE))
                
mos_data_nofill <-  subset(mos_data_acc, condition %in% c("embed_focus","verb_focus"))
mos_data_acc_noprac <- subset(mos_data_acc, block_id != "practice")

mos_means = mos_data_acc %>%
# exclude practice trials
filter(block_id != "practice") %>%
# combine all good/bad fillers together
      mutate(condition = case_when(condition == "filler_good_1" | condition == "filler_good_2" ~ "Good Filler",
      condition == "filler_bad_1" | condition == "filler_bad_2" ~ "Bad Filler",
      condition == "embed_focus" ~ "Embedded Focus",
      condition == "verb_focus" ~ "Adverb Focus")) %>%
      group_by(condition) %>%
      summarize(Mean = mean(acceptability_rating), CILow = ci.low(acceptability_rating),
                            CIHigh = ci.high(acceptability_rating)) %>%
      ungroup() %>%
      mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
# reorder the factors
      mutate(condition = fct_relevel(condition, "Good Filler", "Embedded Focus", "Adverb Focus", "Bad Filler"))
mos_data_bg_nofill <-  subset(mos_data_bg, condition %in% c("embed_focus","verb_focus"))
mos_data_bg_noprac <- subset(mos_data_bg_nofill, block_id != "practice")
mos_bg_means = mos_data_bg %>%
               filter(condition %in% c("embed_focus", "verb_focus")) %>%
              mutate(condition = ifelse(condition=="verb_focus", "Adverb Focus", "Embedded Focus")) %>%
                  #  1 -> Adverb Focus, 0 -> noun focus;lower  value,  more backgrounded
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
mos_acc_graph <- ggplot(mos_means, 
                        aes(x=condition, 
                            y=Mean, 
                            fill=condition)) +
  geom_bar(stat="identity", width=0.8, alpha=0.85,color="black") +
  # geom_bar(stat="identity", width=0.8, aes(color=condition)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,  show.legend = FALSE) +
  scale_fill_manual(values=grayPalette, name = NULL, guide="none") +
  # scale_fill_manual(values=cbPalette, name = NULL, guide="none") +
  theme_bw() +
  xlab("Condition") +
  scale_color_manual(values=grayPalette, name=NULL, guide="none") +
  # scale_color_manual(values=cbPalette, name=NULL, guide="none") +
  scale_x_discrete(labels=c("Good Filler"="Good\nFiller", 
                            "Embedded Focus"="Embedded\nFocus",
                            "Adverb Focus"="Adverb\nFocus",
                            "Bad Filler"="Bad\nFiller")) +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1)) +
  geom_signif(comparisons=list(c("Good Filler", "Adverb Focus")), annotations="***",y_position = 0.9) +
  geom_signif(comparisons=list(c("Embedded Focus", "Adverb Focus")), annotations="***",y_position = 0.8) +
  geom_signif(comparisons=list(c("Bad Filler", "Adverb Focus")), annotations="***",y_position = 0.7) +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=12))
mos_acc_graph
ggsave(mos_acc_graph, file="../graphs/main/mos_acc_grayscale.pdf", width=3, height=3)

###########BG question plot#######################
mos_bg_graph <- ggplot(mos_bg_means %>% 
                         mutate(condition = fct_relevel(condition, "Embedded Focus", "Adverb Focus")), aes(x=condition, y=Mean, fill=condition)) +
  geom_bar(stat="identity", width=0.8, alpha=0.85,color="black") +
  # geom_bar(stat="identity", width=0.8, aes(color=condition)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,  show.legend = FALSE) +
  scale_fill_manual(values=c("#6D6D6D", "#BEBEBE"), name = NULL, guide="none") +
  # scale_fill_manual(values=c("#56B4E9", "#009E73"), name = NULL, guide="none") +
  theme_bw() +
  xlab("Condition") +
  scale_color_manual(values=c("#6D6D6D", "#BEBEBE"),name=NULL, guide="none") +
  # scale_color_manual(values=c("#56B4E9", "#009E73"),name=NULL, guide="none") +
  scale_x_discrete(labels=c("Embedded Focus"="Embedded\nFocus",
                            "Adverb Focus"="Adverb\nFocus")) +
  scale_y_continuous(name="Proportion of Backgrounded\nInterpretation of the Embedded Object", limits=c(0, 1)) + 
  theme(axis.text.x = element_text(size=8),
        axis.title=element_text(size=10)) +
  geom_signif(comparisons = list(c("Embedded Focus", "Adverb Focus")),
              annotations="***",y_position = 0.93)
mos_bg_graph
ggsave(mos_bg_graph, file="../graphs/main/mos_bg_grayscale.pdf", width=2, height=3)
 
 
#######SCR plot############
mos_scr_means <- mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus")) %>%
  mutate(condition = ifelse(condition=="verb_focus", "Adverb Focus", "Embedded Focus")) %>% 
  group_by(verb, condition) %>%
  summarise( ACC = mean(acceptability_rating), 
              SCR = mean(scr)) %>% 
  mutate(label = case_when(verb %in% c("cheerfully","dryly", "wearily", "gently") & 
                             condition == "Adverb Focus" ~ verb,
                           verb %in% c("softly", "quietly", "sternly", "ruefully", 
                                       "calmly", "loudly", "wistfully", "bluntly") & 
                             condition =="Embedded Focus" ~ verb,
                           TRUE ~ ""))
 
mos_scr_plot <- ggplot(mos_scr_means %>% 
                         mutate(condition = fct_relevel(condition, "Embedded Focus", "Adverb Focus")),
                        aes(x = SCR, y = ACC, 
                            color = condition, 
                            fill=condition)) +
  geom_smooth(method = "lm") +
  geom_point(aes(shape=condition),
             color="black",size=2.5) +
  geom_label_repel(data=subset(mos_scr_means, verb%in%c("bluntly","quietly","gently","dryly",
                                                        "cheerfully","calmly")),
                   aes(label=label),
                   color="black",fill="white",
                   box.padding=0.2,
                   segment.size=0.1, nudge_x=-0.13, direction="y",
                   seed=124)+
  geom_label_repel(data=subset(mos_scr_means, verb%in%c("softly","sternly","wistfully",
                                                        "ruefully", "wearily", "loudly")),
                   aes(label=label),
                   color="black",fill="white",
                   box.padding=0.2,
                   segment.size=0.1, nudge_x=0.13, direction="y",
                   seed=124)+
  geom_line(aes(group=verb),
             color = "black",
             alpha = 0.6,
             linetype = "dashed") +
  xlab("Log-transformed SCR score")+
  ylab("Mean Acceptability Rating") +
  labs(shape="Condition") +
  scale_color_manual(values=c("#6D6D6D", "#BEBEBE"),
                     labels=c("Embedded Focus", "Adverb Focus"),
                     name = "Condition") +
  # scale_color_manual(values=c("#56B4E9", "#009E73"),
  #                    labels=c("Embedded Focus", "Adverb Focus"),
  #                    name = "Condition") +
  scale_fill_manual(values=c("#6D6D6D", "#BEBEBE"), 
                    labels=c("Embedded Focus", "Adverb Focus"),
                    name = "Condition") +
  # scale_fill_manual(values=c("#56B4E9", "#009E73"), 
  #                   labels=c("Embedded Focus", "Adverb Focus"),
  #                   name = "Condition") +
  theme(legend.position="top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18)) 
mos_scr_plot
ggsave(mos_scr_plot, file="../graphs/main/scr_grayscale.pdf", width=7, height=6)


#######VFF plot############
mos_vff_means = mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus")) %>%
  mutate(condition = ifelse(condition=="verb_focus", "Adverb Focus", "Embedded Focus")) %>% 
  group_by(verb, condition) %>%
  summarise( ACC = mean(acceptability_rating), 
             VFF = mean(vff)) %>% 
  mutate(label = case_when(verb %in% c() & condition == "Adverb Focus" ~ verb,
                           verb %in% c("wearily","dryly","cheerfully", "gently", "calmly", "softly", "quietly", "sternly", "ruefully", "loudly", "bluntly","wistfully") & condition =="Embedded Focus" ~ verb,
                           TRUE ~ ""))

mos_vff_plot <- ggplot(mos_vff_means %>% 
                         mutate(condition = fct_relevel(condition, "Embedded Focus", "Adverb Focus")),
                       aes(x = VFF, y = ACC, 
                           color = condition, 
                           fill=condition, 
                           label=verb)) +
  geom_smooth(method = "lm") +
  geom_point(aes(shape=condition),
             color="black",size=2.5) +
  geom_label_repel(data=subset(mos_vff_means, verb%in%c("bluntly","wearily","gently","dryly", "cheerfully")),
                   aes(label=label),
                   color="black",fill="white",
                   box.padding=0.2,
                   segment.size=0.1, nudge_x=-0.13, direction="y",
                   seed=124)+
  geom_label_repel(data=subset(mos_vff_means, verb%in%c("softly","sternly","wistfully","ruefully", "quietly", "loudly","calmly")),
                   aes(label=label),
                   color="black",fill="white",
                   box.padding=0.2,
                   segment.size=0.1, nudge_x=0.13, direction="y",
                   seed=124)+
  geom_line(aes(group=verb),
            color = "black",
            alpha = 0.6,
            linetype = "dashed") +
  xlab("Log-transformed predicate-frame frequency")+
  ylab("Mean Acceptability Rating") +
  labs(shape="Condition")+
  scale_color_manual(values=c("#6D6D6D", "#BEBEBE"), 
                     labels=c("Embedded Focus", "Adverb Focus"),
                     name = "Condition") +
  # scale_color_manual(values=c("#56B4E9", "#009E73"), 
  #                    labels=c("Embedded Focus", "Adverb Focus"),
  #                    name = "Condition") +
  scale_fill_manual(values=c("#6D6D6D", "#BEBEBE"), 
                    labels=c("Embedded Focus", "Adverb Focus"),
                    name = "Condition") +
  # scale_fill_manual(values=c("#56B4E9", "#009E73"), 
  #                   labels=c("Embedded Focus", "Adverb Focus"),
  #                   name = "Condition") +
  theme(legend.position="top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18))
mos_vff_plot
ggsave(mos_vff_plot, file="../graphs/main/vff_grayscale.pdf", width=7, height=6)

##############trial_order plot#############
mos_trial_means = mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus") ) %>%
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
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition %in% c("filler_bad_1","filler_bad_2")] <- "filler_bad"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition %in% c("filler_good_1","filler_good_2")] <- "filler_good"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "embed_focus"] <- "embed_focus"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "verb_focus"] <- "verb_focus"
mos_data_acc_noprac$prim_cond<- as.factor(mos_data_acc_noprac$prim_cond)
mos_data_acc_noprac$prim_cond<-relevel(mos_data_acc_noprac$prim_cond, ref = "verb_focus")
levels(mos_data_acc_noprac$prim_cond)

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
mos_data_bg_nofill$condition <- relevel(mos_data_bg_nofill$condition, ref="embed_focus")
contrasts(mos_data_bg_nofill$condition)=contr.sum(2)
levels(mos_data_bg_nofill$condition)
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
mos_scr_model_data$condition <- relevel(mos_scr_model_data$condition, ref="embed_focus")
contrasts(mos_scr_model_data$condition) <- contr.sum(2)
levels(mos_scr_model_data$condition)
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
mos_vff_model_data$condition <- relevel(mos_vff_model_data$condition, ref="embed_focus")
contrasts(mos_vff_model_data$condition) <- contr.sum(2)
levels(mos_vff_model_data$condition)
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
