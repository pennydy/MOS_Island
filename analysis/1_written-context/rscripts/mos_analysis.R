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

`%notin%` <- Negate(`%in%`)
theme_set(theme_bw())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#######################Load Data ######################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('locative_helpers.R')
mos_data<-read.csv("../../../data/1_written-context/main/1_written-context-main-trials.csv")
#######################Participant Exclusion###########################
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
mos_data$scr <- scale(mos_data$scr, center = TRUE)

mos_data$vff[mos_data$verb == "stammer"] = -6.841637508
mos_data$vff[mos_data$verb == "shout"] = -5.603800653
mos_data$vff[mos_data$verb == "yell"] =	-5.987162775
mos_data$vff[mos_data$verb == "murmur"] =	-6.230622674
mos_data$vff[mos_data$verb == "whisper"] =	-5.53165267
mos_data$vff[mos_data$verb == "mutter"] =	-6.092051478
mos_data$vff[mos_data$verb == "scream"] =	-5.793174124
mos_data$vff[mos_data$verb == "mumble"] =	-6.395773947
mos_data$vff[mos_data$verb == "whine"] =	-6.549750892
mos_data$vff <- scale(mos_data$vff, center = TRUE)

# unique(mos_data_bg_nofill$verb)
   
###Exclude Subjects###
length(unique(mos_data$workerid))

# excluded based on non-native speaker status
excluded_subjects <- c(198,200) 
mos_data <- subset(mos_data, !is.element(mos_data$workerid, excluded_subjects))
# excluded based on performance in the practice trials
practice_data=subset(mos_data,block_id == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(mos_data, !is.element(workerid, practice_good_data$workerid))$workerid)
mos_data=subset(mos_data, is.element(workerid, practice_good_data$workerid))
length(unique(mos_data$workerid))
# excluded based on performance (ratings for unacceptable fillers > acceptable fillers)
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
     filter (block_id != "practice") %>%
     filter (condition %in% c("embed_focus","verb_focus"))%>%
     mutate(bg = case_when(condition == "verb_focus" & bg_response == "correct" ~ 1,
                           condition == "verb_focus" & bg_response == "incorrect" ~ 0,
                           condition == "embed_focus" & bg_response == "incorrect" ~ 1,
                           condition == "embed_focus" & bg_response == "correct" ~ 0)) %>%
      group_by(item_id, condition) %>%
      summarise(BG = mean(bg, na.rm=TRUE), AC = mean(acceptability_rating, na.rm=TRUE))
                
mos_data_nofill <-  subset(mos_data_acc, condition %in% c("embed_focus","verb_focus"))
mos_data_acc_noprac <- subset(mos_data_acc, block_id != "practice")

mos_means = mos_data_acc %>%
# exclude practice trials
filter(block_id != "practice") %>%
# combine all good/bad fillers together
      mutate(condition = case_when(condition == "filler_good_1" | condition == "filler_good_2" ~ "Good Filler",
      condition == "filler_bad_1" | condition == "filler_bad_2" ~ "Bad Filler",
      condition == "embed_focus" ~ "Embedded Focus",
      condition == "verb_focus" ~ "Verb Focus")) %>%
      group_by(condition) %>%
      summarize(Mean = mean(acceptability_rating), CILow = ci.low(acceptability_rating),
                            CIHigh = ci.high(acceptability_rating)) %>%
      ungroup() %>%
      mutate(YMin=Mean-CILow,YMax=Mean+CIHigh) %>%
# reorder the factors
      mutate(condition = fct_relevel(condition, "Good Filler", "Embedded Focus", "Verb Focus", "Bad Filler"))
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
                  #  1 -> verb focus, 0 -> noun focus;lower  value,  more backgrounded
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
              mutate(YMin=Mean-CILow,
                     YMax=Mean+CIHigh)
# reorder the factors
                # mutate(condition = fct_relevel(condition, "Filler Good", "Embedded Focus", "Verb Focus", "Filler Bad"))


##########Acceptability plot########################

mos_acc_graph <- ggplot(mos_means,
                        #     %>% filter(condition %in% c("Embedded Focus", "Verb Focus"))
                        aes(x=condition, y=Mean, fill=condition)) +
   geom_bar(stat="identity", width=0.8, aes(color=condition)) +
   geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,  show.legend = FALSE) +
   scale_fill_manual(values=cbPalette, name = NULL, guide="none") +
   theme_bw()+
   xlab("Condition") +
   scale_color_manual(values=cbPalette, name=NULL, guide="none") +
   theme(legend.position="bottom",
         axis.text.x = element_text(size=8)) +
   scale_x_discrete(labels=c("Good Filler"="Good\nFiller", 
                             "Embedded Focus"="Embedded\nFocus",
                             "Verb Focus"="Verb\nFocus",
                             "Bad Filler"="Bad\nFiller")) +
   scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1)) +
   geom_signif(comparisons=list(c("Good Filler", "Verb Focus")), annotations="***",y_position = 0.9) +
   geom_signif(comparisons=list(c("Embedded Focus", "Verb Focus")), annotations="***",y_position = 0.8) +
   geom_signif(comparisons=list(c("Bad Filler", "Verb Focus")), annotations="***",y_position = 0.7)
                      # + scale_x_discrete(labels = c("Embedded focus", "filler bad 1", "filler bad 2", "filler good 1", "filler good 2", "Verb focus"))
                      # +facet_wrap(~ID) 

 
mos_acc_graph
ggsave(mos_acc_graph, file="../graphs/main/mos_acc_large.pdf", width=2, height=3)

###########BG question plot#######################

mos_bg_graph <- ggplot(mos_bg_means, aes(x=condition, y=Mean, fill=condition)) +
                         geom_bar(stat="identity", width=0.8, aes(color=condition)) +
                         geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,  show.legend = FALSE) +
                         scale_fill_manual(values=c("#56B4E9", "#009E73"),
                                           name = NULL) +
             # scale_fill_manual(values=cbPalette, name = NULL) +
                        theme_bw()+
                        xlab("Condition") +
                        scale_color_manual(values=c("#56B4E9", "#009E73"),
                                           name=NULL) +
                        # scale_color_manual(values=cbPalette, name=NULL) +
   scale_x_discrete(labels=c("Embedded Focus" = "Embedded\nFocus", "Verb Focus"="Verb\nFocus")) +
                        scale_y_continuous(name="Proportion of Backgrounded\nInterpretation of the Embedded Clause", limits=c(0, 1)) + 
                          guides(color = "none") +
                          guides(fill = "none") +
                          theme(axis.text.x = element_text(size=8),
                                axis.title=element_text(size=10)) +
  geom_signif(comparisons = list(c("Embedded Focus", "Verb Focus")),
              annotations="***",y_position = 0.9)
mos_bg_graph
ggsave(mos_bg_graph, file="../graphs/main/mos_bg_large.pdf", width=2, height=3)
 
 
 #######SCR plot############
 mos_scr_means = mos_data_acc %>% 
   filter(condition %in% c("embed_focus", "verb_focus")) %>%
   mutate(condition = ifelse(condition=="verb_focus", "Verb Focus", "Embedded Focus")) %>% 
   filter(verb != "groan")%>%
   group_by(verb, condition) %>%
   summarise( ACC = mean(acceptability_rating), 
              SCR = mean(scr))
 

 mos_scr_plot <- ggplot(mos_scr_means,
                        aes(x = SCR, y = ACC, 
                            color = condition, 
                            fill=condition,
                            label=verb)) +
   geom_point() +
   geom_smooth(method = "lm") +
   geom_text(size=3, color="black", alpha=0.6, hjust="inward", vjust="inward")+
   geom_line(aes(group=verb),
             color = "black",
             alpha = 0.4,
             # size=0.4,
             linetype = "dashed") +
   # scale_x_continuous(expand=expansion(mult = 0.08)) +
   xlab("Log-transformed and mean-centered SCR score")+
   ylab("Mean Acceptability Rating") +
   scale_color_manual(values=c("#56B4E9", "#009E73"), 
                      labels=c("Embedded Focus", "Verb Focus"),
                      name = "Condition",
                      guide="none") +
   scale_fill_manual(values=c("#56B4E9", "#009E73"), 
                     labels=c("Embedded Focus", "Verb Focus"),
                     name = "Condition",
                     guide="none") +
   theme(legend.text=element_text(size=10),
         axis.text=element_text(size=14),
         axis.title=element_text(size=14),
         legend.position = "top")
mos_scr_plot
ggsave(mos_scr_plot, file="../graphs/main/mos_scr_plot_noLegend.pdf", width=6, height=4)
 
 
#######VFF plot############
mos_vff_means = mos_data_acc %>% 
   filter(condition %in% c("embed_focus", "verb_focus")) %>%
   mutate(condition = ifelse(condition=="verb_focus", "Verb Focus", "Embedded Focus")) %>% 
   filter(verb %notin% c("groan", "shriek", "moan")) %>%
   group_by(verb, condition) %>%
   summarise( ACC = mean(acceptability_rating), 
              VFF = mean(vff))
 
 
mos_vff_plot <- ggplot(mos_vff_means,
                        aes(x = VFF, y = ACC, 
                            color = condition, 
                            fill=condition, 
                            label=verb)) +
   geom_point() +
   geom_smooth(method = "lm") +
   # geom_text(size=3, color="black", alpha=0.6, hjust=-0.1, vjust=0.2)+
   geom_text(size=4, color="black", alpha=0.6, hjust="inward", vjust="inward") +
   geom_line(aes(group=verb),
             color = "black",
             alpha = 0.4,
             linetype = "dashed") +
   scale_y_continuous(limits = c(0, 1)) +
   xlab("Log-transformed and mean-centered verb-frame frequency score")+
   ylab("Mean Acceptability Rating") +
   scale_color_manual(values=c("#56B4E9", "#009E73"), 
                      labels=c("Embedded\nFocus", "Verb\nFocus"),
                      name = "Condition",
                      guide="none") +
   scale_fill_manual(values=c("#56B4E9", "#009E73"), 
                     labels=c("Embedded\nFocus", "Verb\nFocus"),
                     name = "Condition",
                     guide="none") +
   theme(legend.text=element_text(size=10),
         axis.text=element_text(size=14),
         axis.title=element_text(size=14))
mos_vff_plot
ggsave(mos_vff_plot, file="../graphs/main/mos_vff_plot_noLegend.pdf", width=6, height=4)
 
 
##############trial_order plot#############
mos_trial_means = mos_data_acc %>% 
   filter(condition %in% c("embed_focus", "verb_focus") )%>%
   group_by(trial_num, condition) %>%
   summarise( Mean = mean(acceptability_rating))
 
 
mos_trial_plot <- ggplot(mos_trial_means,aes(x = trial_num, y = Mean, color = condition, fill=condition)) +
   geom_point()+
   geom_smooth(method = "lm")+
   xlab("Presentation order")+
   ylab("Mean acceptability")
 
mos_trial_plot
 

#########################Stats##################################
 
######BG analysis###########
mos_data_bg_nofill<- mos_data_bg_nofill %>%
    mutate(cond = ifelse(condition=="verb_focus", "Verb Focus", "Embedded Focus")) %>%
    #  1 -> verb focus, 0 -> noun focus; the lower the value, the more backgrounded it is
    mutate(bg = case_when(cond == "Verb Focus" & bg_response == "correct" ~ 1,
                          cond == "Verb Focus" & bg_response == "incorrect" ~ 0,
                          cond == "Embedded Focus" & bg_response == "incorrect" ~ 1,
                          cond == "Embedded Focus" & bg_response == "correct" ~ 0
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
 
#####acceptability analysis######
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition %in% c("filler_bad_1","filler_bad_2")] <- "filler_bad"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition %in% c("filler_good_1","filler_good_2")] <- "filler_good"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "embed_focus"] <- "embed_focus"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "verb_focus"] <- "verb_focus"
mos_data_acc_noprac$prim_cond<- as.factor(mos_data_acc_noprac$prim_cond)
mos_data_acc_noprac$prim_cond<-relevel(mos_data_acc_noprac$prim_cond, ref = "verb_focus")

# convergence warning using lmerTest
acc_model <- lmer(acceptability_rating ~ prim_cond + 
                    (1+prim_cond|workerid)+
                    (1|item_id),
                  data = mos_data_acc_noprac)
summary(acc_model)

######SCR analysis#######
mos_scr_model_data <- mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus") )%>%
  filter(verb != "groan")

mos_scr_model_data$condition <- as.factor(mos_scr_model_data$condition)
contrasts(mos_scr_model_data$condition) <- contr.sum(2)
model_scr <- lmer(acceptability_rating ~ condition * scr + 
                    (1|item_id)+
                    (1+condition * scr|workerid),data = mos_scr_model_data)
summary(model_scr)


######VFF analysis#######
mos_vff_model_data <- mos_data_acc %>% 
  filter(condition %in% c("embed_focus", "verb_focus") )%>%
  filter(verb %notin% c("groan", "shriek", "moan"))

mos_vff_model_data$condition <- as.factor(mos_vff_model_data$condition)
contrasts(mos_vff_model_data$condition) <- contr.sum(2)
model_vff <- lmer(acceptability_rating ~ condition * vff + 
                    (1|item_id)+
                    (1+condition * vff|workerid),
                  data = mos_vff_model_data)
summary(model_vff)

####### Satiation analysis#########
mos_trial_data <- mos_data_acc %>%
                  filter(condition %in% c("embed_focus", "verb_focus") )%>%
                  mutate(condition = as.factor(condition)) 

contrasts(mos_trial_data$condition) <- contr.sum(2)       

model_trial <- lmer(acceptability_rating ~ condition * trial_num + 
                      (1+ condition |item_id)+
                      (1+ condition |workerid),data = mos_trial_data)
summary(model_trial)
