library(dplyr)
library(ggplot2)
library(ggpattern)
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
mos_data<-read.csv("../../../data/1b_mos_say-context/main/1b_mos_say-context-trials.csv")

mos_verbs <- c("whisper", "stammer", "mumble", "mutter", "scream", "yell",
               "groan", "whine", "murmur", "shriek", "moan", "shout")

exp1a_mos_data <- read.csv("../../../data/1a_mos_say/main/1a_mos_say-trials.csv")

filler_verbs <- c("think", "suggest", "suspect", "believe", "hope", "guess", "imply",
                  "reveal")
mos_data <- mos_data %>% 
  mutate(verb_type = case_when(verb %in% mos_verbs ~ "mos",
                               verb %in% filler_verbs ~ condition,
                               verb == "say" ~ "say"),
         condition = ifelse(condition == "filler_good" | condition == "filler_bad", "filler", condition))

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

#######################Participant Exclusion###########################
###Exclude Subjects###
length(unique(mos_data$workerid))

practice_data=subset(mos_data,block_id == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- subset(mos_data, !is.element(workerid, practice_good_data$workerid))$workerid
                mos_data=subset(mos_data, is.element(workerid, practice_good_data$workerid))
                length(unique(mos_data$workerid))
                
length(unique(practice_good_data$workerid))

mos_data_acc <- subset(mos_data, acceptability_rating != "NA")
mos_data_bg <- subset(mos_data, bg_response != "NA")
mos_data_acc$acceptability_rating <- as.numeric(mos_data_acc$acceptability_rating)
filler_data = subset(mos_data_acc, verb_type == "filler_good")
ungram_data = subset(mos_data_acc, verb_type == "filler_bad")
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
# write.csv(mos_data,"../../../data/1b_mos_say-context/main/exp1b_after_exclusion.csv", row.names = FALSE)

##################################Getting data ready for plotting and analysis#############################################
# Data cleaning      
## no fillers
mos_data_overall <- mos_data %>% 
     filter(block_id != "practice") %>%
     filter(condition %in% c("embed_focus","verb_focus"))%>%
     mutate(bg = case_when(bg_response == "embed" ~ 0,
                           bg_response == "verb" ~ 1)) %>%
      group_by(item_id, condition, verb, verb_type) %>%
      summarise(BG = mean(bg, na.rm=TRUE), 
                AC = mean(acceptability_rating, na.rm=TRUE))
                
mos_data_nofill <-  subset(mos_data_acc, condition %in% c("embed_focus","verb_focus"))
mos_data_acc_noprac <- subset(mos_data_acc, block_id != "practice")

mos_acc_means = mos_data_acc %>% 
  filter(block_id != "practice") %>% # exclude practice trials
  mutate(verb_type = case_when(verb_type == "filler_good" ~ "Good Filler",
                               verb_type == "filler_bad" ~ "Bad Filler",
                               verb_type == "say" ~ "Say",
                               verb_type == "mos" ~ "MoS"),
         condition = case_when(condition == "filler" ~ "Filler",
                               condition == "verb_focus" ~ "Verb Focus",
                               condition == "embed_focus" ~ "Embedded Focus")) %>%
      group_by(condition,verb_type) %>%
      summarize(Mean = mean(acceptability_rating), 
                CILow = ci.low(acceptability_rating),
                CIHigh = ci.high(acceptability_rating)) %>%
      ungroup() %>%
      mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
      mutate(verb_type = fct_relevel(verb_type, "Good Filler", "Say", "MoS", "Bad Filler"),
             condition = fct_relevel(condition, "Embedded Focus", "Filler", "Verb Focus")) # reorder the factors

mos_data_bg_nofill <-  subset(mos_data_bg, condition %in% c("embed_focus","verb_focus"))

mos_data_bg_noprac <- subset(mos_data_bg_nofill, block_id != "practice")
mos_bg_means = mos_data_bg %>%
               filter(condition %in% c("embed_focus", "verb_focus")) %>%
              mutate(condition = ifelse(condition =="verb_focus", "Verb Focus", "Embedded Focus")) %>%
                  #  1 -> Adverb Focus, 0 -> noun focus;lower  value,  more backgrounded
              mutate(bg = case_when(bg_response == "embed" ~ 0,
                                    bg_response == "verb" ~ 1)) %>%
              group_by(condition, verb_type) %>%
              summarize(Mean = mean(bg),
                        CIHigh = ci.high(bg),
                        CILow = ci.low(bg)) %>%
              ungroup() %>%
              mutate(YMin=Mean-CILow,
                     YMax=Mean+CIHigh) %>%
  mutate(condition = fct_relevel(condition, "Embedded Focus", "Verb Focus"),
         verb_type = ifelse(verb_type == "say", "Say", "MoS"))


##########Acceptability plot########################
# by verb on x-axis
mos_acc_graph <- ggplot(mos_acc_means, 
                        aes(x=verb_type,
                            y=Mean,
                            fill=verb_type,
                            alpha=condition)) +
  geom_bar(stat="identity", 
           position=position_dodge(),
           width=0.8, 
           aes(color=verb_type)) +
  geom_errorbar(aes(ymin=YMin,
                    ymax=YMax),
                width=.2,
                position=position_dodge(width=0.9),
                show.legend = FALSE) +
  scale_fill_manual(values=cbPalette, name = NULL, guide="none") +
  theme_bw() +
  xlab("Verb Type") +
  scale_color_manual(values=cbPalette, name=NULL, guide="none") +
  scale_x_discrete(labels=c("Good Filler"="Good\nFiller", 
                            "Say"="Say",
                            "MoS"="MoS",
                            "Bad Filler"="Bad\nFiller")) +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1)) +
  scale_alpha_discrete(range = c(0.2, 0.9),
                       labels=c("Embedded Focus"="Embedded\nFocus",
                                "Verb Focus"="Verb\nFocs",
                                "Filler"="Filler")) +
  theme(legend.position = "top",
        axis.text=element_text(size=10),
        axis.title=element_text(size=14))
mos_acc_graph
ggsave(mos_acc_graph, file="../graphs/main/mos_acc.pdf", width=4, height=4)

# by condition on x-axis
mos_acc_means_by_focus <- mos_acc_means %>%
  mutate(condition=if_else(condition=="Filler", verb_type ,condition),
         verb_type=if_else(verb_type %in% c("Good Filler", "Bad Filler"), "Filler", verb_type)) %>% 
  mutate(condition=fct_relevel(condition, c("Good Filler", "Embedded Focus", "Verb Focus", "Bad Filler")),
         verb_type=fct_relevel(verb_type, c("MoS","Filler","Say")))

mos_acc_graph_by_focus <- ggplot(mos_acc_means_by_focus, 
                        aes(x=condition,
                            y=Mean,
                            fill=condition,
                            pattern=verb_type)) +
  geom_bar_pattern(
    position = "dodge",
    stat="identity",
    pattern_angle = 45,
    pattern_spacing = 0.02,
    pattern_fill="black",
    pattern_alpha=0.5,
    alpha=0.7) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,
                position=position_dodge(width=0.8),
                show.legend = FALSE) +
  scale_fill_manual(values=cbPalette, name = NULL, guide="none") +
  theme_bw() +
  xlab("Condition") +
  scale_color_manual(values=cbPalette, name=NULL, guide="none") +
  scale_x_discrete(labels=c("Good Filler"="Good\nFiller", 
                            "Embedded Focus"="Embedded\nFocus",
                            "Verb Focus"="Verb\nFocus",
                            "Bad Filler"="Bad\nFiller")) +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1)) +
  # scale_alpha_discrete(range = c(0.2, 0.9),
  #                      name="Verb Type") +
  scale_pattern_manual(values = c(MoS = "stripe", Say = "none", Filler="circle"),
                       name="Verb Type") +
  theme(legend.position = "right",
        axis.text=element_text(size=10),
        axis.title=element_text(size=14)) +
  geom_signif(comparisons = list(c("Embedded Focus", "Verb Focus")),
              annotations="***",y_position = 0.96) + 
  geom_signif(xmin = c(1.7, 2.7),
            xmax = c(2.2, 3.2),
            y_position = c(0.93, 0.68),
            annotations="***")
mos_acc_graph_by_focus
ggsave(mos_acc_graph_by_focus, file="../graphs/main/mos_acc_by_focus_legend.pdf", width=4, height=5)

##########Exp1a and Exp1b Acceptability plot########################
# load exp1a data and combine with exp1b for plotting
exp1a_mos_acc_means <- subset(exp1a_mos_data, acceptability_rating != "NA")
exp1a_mos_acc_means = exp1a_mos_acc_means %>%
  filter(block_id != "practice") %>% # exclude practice trials
  mutate(verb_type = case_when(condition == "filler_good" ~ "Good Filler",
                               condition == "filler_bad" ~ "Bad Filler",
                               condition == "say" ~ "Say",
                               condition == "mos" ~ "MoS"),
         condition = "No Focus") %>%
  group_by(workerid) %>% 
  mutate(mean = mean(acceptability_rating),
         sd = sd(acceptability_rating)) %>% 
  ungroup() %>% 
  group_by(condition,verb_type) %>%
  summarize(Mean = mean(acceptability_rating),
            CILow = ci.low(acceptability_rating),
            CIHigh = ci.high(acceptability_rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,YMax=Mean+CIHigh)

all_mos_acc_means <- bind_rows(lst(mos_acc_means, exp1a_mos_acc_means), .id="exp") %>% 
  mutate(exp = ifelse(exp=="exp1a_mos_acc_means", "exp1a","exp1b")) %>% 
  mutate(verb_type = fct_relevel(verb_type, "Good Filler", "Say", "MoS", "Bad Filler"),
         condition=fct_relevel(condition, c("No Focus", "Filler", "Embedded Focus", "Verb Focus")))

all_mos_acc_graph <- ggplot(all_mos_acc_means, 
                        aes(x=verb_type,
                            y=Mean,
                            fill=verb_type,
                            alpha=condition)) +
  geom_bar(stat="identity", position=position_dodge(), width=0.8, aes(color=verb_type)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,
                position=position_dodge(width=0.8),
                show.legend = FALSE) +
  scale_fill_manual(values=cbPalette, name = NULL, guide="none") +
  theme_bw() +
  xlab("Verb Type") +
  scale_color_manual(values=cbPalette, name=NULL, guide="none") +
  scale_x_discrete(labels=c("Good Filler"="Good\nFiller", 
                            "Say"="Say",
                            "MoS"="MoS",
                            "Bad Filler"="Bad\nFiller")) +
  facet_grid(~exp) +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1)) +
  scale_alpha_discrete(range = c(0.2, 0.9)) +
  # geom_signif(comparisons=list(c("Good Filler", "Adverb Focus")), annotations="***",y_position = 0.9) +
  # geom_signif(comparisons=list(c("Embedded Focus", "Adverb Focus")), annotations="***",y_position = 0.8) +
  # geom_signif(comparisons=list(c("Bad Filler", "Adverb Focus")), annotations="***",y_position = 0.7) +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=14))
all_mos_acc_graph

###########BG question plot#######################
# by verb on x-axis
mos_bg_graph <- ggplot(mos_bg_means,
                       aes(x=verb_type, y=Mean, fill=verb_type, alpha=condition)) +
  geom_bar(stat="identity", 
           position=position_dodge(),
           width=0.8,
           aes(color=verb_type)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),
                position=position_dodge(0.8),
                width=.2,
                show.legend = FALSE) +
  scale_fill_manual(values=c("#56B4E9", "#009E73"), name = NULL, guide="none") +
  theme_bw() +
  xlab("Verb Type") +
  scale_color_manual(values=c("#56B4E9", "#009E73"),name=NULL, guide="none") +
  scale_alpha_discrete(range=c(0.2,0.9),
                       labels=c("Embedded Focus"="Embedded\nFocus",
                                "Verb Focus"="Verb\nFocus")) +
  scale_y_continuous(name="Proportion of Backgrounded\nInterpretation of the Embedded Object", limits=c(0, 1)) + 
  theme(legend.position="top",
        axis.text=element_text(size=12),
        axis.title=element_text(size=12)) # + legend.position="bottom"
  # geom_signif(comparisons = list(c("Embedded Focus", "Adverb Focus")),
  #             annotations="***",y_position = 0.93)
mos_bg_graph
ggsave(mos_bg_graph, file="../graphs/main/mos_bg.pdf", width=4, height=4)

# by condition on x-axis
mos_bg_graph_by_focus <- ggplot(mos_bg_means,
                       aes(x=condition, y=Mean, fill=condition,pattern=verb_type)) +
  # geom_bar(stat="identity", 
  #          position=position_dodge(),
  #          width=0.8,
  #          aes(color=condition, alpha=verb_type)) +
  geom_bar_pattern(
    position = "dodge",
    stat="identity",
    pattern_angle = 45,
    pattern_spacing = 0.02,
    pattern_fill="black",
    pattern_alpha=0.5,
    alpha=0.7) + 
  geom_errorbar(aes(ymin=YMin,ymax=YMax),
                position=position_dodge(0.8),
                width=.2,
                show.legend = FALSE) +
  scale_fill_manual(values=c("#56B4E9", "#009E73"), name = NULL, guide="none") +
  theme_bw() +
  xlab("Condition") +
  scale_color_manual(values=c("#56B4E9", "#009E73"),name=NULL, guide="none") +
  scale_pattern_manual(values = c(MoS = "stripe", Say = "none"),
                       guide="none") +
  scale_y_continuous(name="Proportion of Backgrounded\nInterpretation of the Embedded Object", limits=c(0, 1)) + 
  scale_x_discrete(labels=c("Embedded Focus"="Embedded\nFocus",
                            "Verb Focus"="Verb\nFocus")) +
  theme(legend.position="top",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14)) +
  geom_signif(comparisons = list(c("Embedded Focus", "Verb Focus")),
            annotations="***",y_position = 0.92) +
  geom_signif(xmin = c(0.8, 1.8),
              xmax = c(1.2, 2.2),
              y_position = c(0.2, 0.87),
              annotations="***")
mos_bg_graph_by_focus
ggsave(mos_bg_graph_by_focus, file="../graphs/main/mos_bg_by_focus_no_legend.pdf", width=4, height=5)

###########Exp1a and Exp1b BG question plot#######################
exp1a_mos_data_bg <- subset(exp1a_mos_data, bg_response != "NA")
exp1a_mos_bg_means = exp1a_mos_data_bg %>%
  filter(condition %in% c("say", "mos")) %>%
  #  1 -> verb Focus, 0 -> embed focus;lower  value,  more backgrounded
  mutate(verb_type = ifelse(condition=="mos", "MoS", "Say"),
         condition = "No Focus",
         bg = case_when(bg_response == "embed" ~ 0,
                        bg_response == "verb" ~ 1)) %>%
  group_by(condition, verb_type) %>%
  summarize(Mean = mean(bg),
            CILow = ci.low(bg),
            CIHigh = ci.high(bg)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,
         YMax=Mean+CIHigh)

all_mos_bg_means <- bind_rows(lst(mos_bg_means, exp1a_mos_bg_means), .id="exp") %>% 
  mutate(exp = ifelse(exp=="exp1a_mos_bg_means", "exp1a","exp1b"),
         condition=fct_relevel(condition, c("No Focus", "Embedded Focus", "Verb Focus")),
         verb_type = fct_relevel(verb_type, c("Say", "MoS")))

all_mos_bg_graph <- ggplot(all_mos_bg_means,
                       aes(x=verb_type, y=Mean, fill=verb_type, alpha=condition)) +
  geom_bar(stat="identity", 
           position=position_dodge(),
           width=0.8,
           aes(color=verb_type)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),
                position=position_dodge(0.8),
                width=.2,
                show.legend = FALSE) +
  scale_fill_manual(values=c("#56B4E9", "#009E73"), name = NULL, guide="none") +
  theme_bw() +
  xlab("Verb Type") +
  scale_color_manual(values=c("#56B4E9", "#009E73"),name=NULL, guide="none") +
  scale_x_discrete(labels=c("Embedded Focus"="Embedded\nFocus",
                            "Verb Focus"="Verb\nFocus")) +
  scale_alpha_discrete(range=c(0.2,0.9)) +
  scale_y_continuous(name="Proportion of Backgrounded\nInterpretation of the Embedded Object", limits=c(0, 1)) + 
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=12)) +
  facet_grid(~exp)
all_mos_bg_graph
 
#######SCR plot############
mos_scr_means <- mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus")) %>%
  mutate(condition = ifelse(condition=="verb_focus", "Verb Focus", "Embedded Focus")) %>%
  group_by(verb, condition) %>%
  summarise( ACC = mean(acceptability_rating), 
              SCR = mean(scr)) %>% 
  mutate(label=verb) %>% 
  filter(verb != "say") %>% 
  mutate(label = case_when(verb %in% c("groan",  "stammer", "whisper","moan","yell",
                                       "scream", "mumble") & 
                             condition=="Embedded Focus" ~ verb,
                           verb %in% c("shout","shriek","murmur",
                                       "mutter","whine") & condition=="Verb Focus" ~ verb,
                           TRUE ~ ""))
 
mos_scr_plot <- ggplot(mos_scr_means %>%
                         mutate(condition = fct_relevel(condition, "Embedded Focus", "Verb Focus")),
                        aes(x = SCR, y = ACC, 
                            color = condition, 
                            fill=condition,
                            label=verb)) +
  geom_point() +
  geom_smooth(method = "lm") +
  # geom_text(size=3, color="black", alpha=0.6, hjust="inward", vjust="inward")+
  geom_label_repel(data=subset(mos_scr_means, verb%in%c("stammer", "whine","yell"
                                                        , "shout")),
                   aes(label=label),
                   color="black",fill="white",
                   box.padding=0.1,
                   segment.size=0.2, nudge_x=-0.16, direction="y")+
  geom_label_repel(data=subset(mos_scr_means, verb%in%c("groan", "whisper","shriek",
                                                        "scream", "mumble","murmur", "mutter","moan")),
                   aes(label=label),
                   color="black",fill="white",
                   box.padding=0.1,
                   segment.size=0.2, nudge_x=0.16, direction="y")+
  geom_line(aes(group=verb),
             color = "black",
             alpha = 0.6,
             linetype = "dashed") +
  # scale_x_continuous(expand=expansion(mult = 0.08)) +
  xlab("Log-transformed SCR score")+
  ylab("Mean Acceptability Rating") +
  scale_color_manual(values=c("#56B4E9", "#009E73"), 
                     labels=c("Embedded Focus", "Verb Focus"),
                     name = "Condition") +
  scale_fill_manual(values=c("#56B4E9", "#009E73"), 
                    labels=c("Embedded Focus", "Verb Focus"),
                    name = "Condition") +
  theme(legend.position="top",
        legend.title=element_text(size=16),
        legend.text=element_text(size=16),
        axis.text=element_text(size=16),
        axis.title=element_text(size=18)) 
mos_scr_plot
ggsave(mos_scr_plot, file="../graphs/main/scr.pdf", width=7, height=6)


#######VFF plot############
mos_vff_means = mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus")) %>%
  mutate(condition = ifelse(condition=="verb_focus", "Verb Focus", "Embedded Focus")) %>% 
  group_by(verb, condition) %>%
  summarise( ACC = mean(acceptability_rating), 
             VFF = mean(vff)) %>% 
  filter(!is.na(VFF)) %>% 
  mutate(label = case_when(verb %in% c("groan",  "stammer", "shout","shriek",
                                       "mumble","yell","mutter") & 
                             condition=="Embedded Focus" ~ verb,
                           verb %in% c("scream", "moan","whisper","whine",
                                       "murmur") & condition=="Verb Focus" ~ verb,
                           TRUE ~ ""))


mos_vff_plot <- ggplot(mos_vff_means %>% 
                         mutate(condition = fct_relevel(condition, "Embedded Focus", "Verb Focus")),
                       aes(x = VFF, y = ACC, 
                           color = condition, 
                           fill=condition, 
                           label=verb)) +
  geom_point() +
  geom_smooth(method = "lm") +
  # geom_text(size=3, color="black", alpha=0.6, hjust="inward", vjust="inward")+
  geom_label_repel(data=subset(mos_vff_means, verb%in%c("whine","mumble","mutter",
                                                        "shout","whisper","murmur")),
                   aes(label=label),
                   color="black",fill="white",
                   box.padding=0.1,
                   segment.size=0.2, nudge_x=-0.11, direction="y")+
  geom_label_repel(data=subset(mos_vff_means, verb%in%c("shriek","stammer","yell",
                                                        "scream")),
                   aes(label=label),
                   color="black",fill="white",
                   box.padding=0.1,
                   segment.size=0.2, nudge_x=0.11, direction="y")+
  geom_line(aes(group=verb),
            color = "black",
            alpha = 0.6,
            linetype = "dashed") +
  # scale_x_continuous(expand=expansion(mult = 0.08)) +
  xlab("Log-transformed verb-frame frequency")+
  ylab("Mean Acceptability Rating") +
  scale_color_manual(values=c("#56B4E9", "#009E73"), 
                     labels=c("Embedded Focus", "Verb Focus"),
                     name = "Condition") +
  scale_fill_manual(values=c("#56B4E9", "#009E73"), 
                    labels=c("Embedded Focus", "Verb Focus"),
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
######BG analysis###########
mos_data_bg_nofill<- mos_data_bg_nofill %>%
  # mutate(cond = ifelse(condition=="verb_focus", "Adverb Focus", "Embedded Focus")) %>%
  #  1 -> Adverb Focus, 0 -> noun focus; the lower the value, the more backgrounded it is
  mutate(bg = case_when(bg_response == "embed" ~ 0,
                        bg_response == "verb" ~ 1
  ))
mos_data_bg_nofill$bg <- as.numeric(mos_data_bg_nofill$bg)
# mos_data_bg_nofill$condition <- relevel(as.factor(mos_data_bg_nofill$condition), ref="verb_focus")
mos_data_bg_nofill$condition <- as.factor(mos_data_bg_nofill$condition)
mos_data_bg_nofill$condition <- relevel(mos_data_bg_nofill$condition, ref="embed_focus")
mos_data_bg_nofill$verb_type <- as.factor(mos_data_bg_nofill$verb_type)
mos_data_bg_nofill$verb_type <- relevel(mos_data_bg_nofill$verb_type, ref="mos")
mos_data_bg_nofill$item_id <- as.factor(mos_data_bg_nofill$item_id)
mos_data_bg_nofill$workerid <- as.factor(mos_data_bg_nofill$workerid)
contrasts(mos_data_bg_nofill$condition)=contr.sum(2)
levels(mos_data_bg_nofill$condition)
contrasts(mos_data_bg_nofill$verb_type)=contr.sum(2)
levels(mos_data_bg_nofill$verb_type)

bg_model <- glmer(bg~condition*verb_type+
                    # (1+condition|item_id)+ # fail to converge with random effect of item
                    (1+condition|workerid),
                  family = "binomial",
                  data=mos_data_bg_nofill)  
# or add the control: control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))
# source : https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
summary(bg_model)

# as a sanity check that data is fine and brms is giving
xtabs(data=mos_data_bg_nofill,item_id~condition+verb_type)
# # global setting -> will change everything to sum coded
# options(contrasts = c("contr.sum","contr.sum"))

bg_model_brms <- brm(bg~condition*verb_type+
                    (1+condition*verb_type|workerid)+
                    (1+condition|item_id),
                  family = "bernoulli",
                  data=mos_data_bg_nofill,
                iter = 4000,
                chains = 4,
                cores = 4)
post_samples = data.frame(fixef(bg_model_brms, summary = F))
sum(post_samples$verb_type1 > 0) / length(post_samples$verb_type1)
fixef(bg_model_brms)


#####acceptability analysis######
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$verb_type == "filler_bad"] <- "filler_bad"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$verb_type == "filler_good"] <- "filler_good"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "embed_focus"] <- "embed_focus"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "verb_focus"] <- "verb_focus"
mos_data_acc_noprac <- mos_data_acc_noprac %>% 
  filter(verb_type %in% c("mos", "say"))
mos_data_acc_noprac$prim_cond<- as.factor(mos_data_acc_noprac$prim_cond)
mos_data_acc_noprac$prim_cond<-relevel(mos_data_acc_noprac$prim_cond, ref = "embed_focus")
contrasts(mos_data_acc_noprac$prim_cond)=contr.sum(2)
levels(mos_data_acc_noprac$prim_cond)
mos_data_acc_noprac$verb_type<-relevel(factor(mos_data_acc_noprac$verb_type), ref = "mos")
contrasts(mos_data_acc_noprac$verb_type)=contr.sum(2)
levels(mos_data_acc_noprac$verb_type)

# global setting 
# options(contrasts = c('contr.treatment','contr.treatment'))
acc_model <- lmer(acceptability_rating ~ prim_cond * verb_type + 
                    (1+prim_cond|item_id)+
                    (1+prim_cond*verb_type|workerid),
                  data = mos_data_acc_noprac)
summary(acc_model)

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
  filter(condition %in% c("embed_focus", "verb_focus")) %>%
  filter(verb %notin% c("groan", "shriek", "moan"))

mos_vff_model_data$condition <- as.factor(mos_vff_model_data$condition)
mos_vff_model_data$condition <- relevel(mos_vff_model_data$condition, ref="embed_focus")
contrasts(mos_vff_model_data$condition) <- contr.sum(2)
levels(mos_vff_model_data$condition)
# xtabs(data=mos_vff_model_data,item_id~condition+vff)
model_vff <- lmer(acceptability_rating ~ condition * vff + 
                    (1+condition|item_id)+ # should not include vff in the by-item random effect 
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
