library(dplyr)
library(ggplot2)
library(ggsignif)
library(lme4)
# library(lmerTest)
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
mos_data<-read.csv("../../../data/3_sayAdv-context/pilot/3_sayAdv-context-trials.csv")
#######################Participant Exclusion###########################
##Add log SCR score to verbs###
all_data$scr[all_data$verb == "softly"] = log(0.001729355815)
all_data$scr[all_data$verb == "quietly"] = log(0.00321248279)
all_data$scr[all_data$verb == "loudly"] = log(0.01222493888)
all_data$scr[all_data$verb == "bluntly"] =log(0.09223300971)
all_data$scr[all_data$verb == "cheerfully"] =log(0.009230769231)
all_data$scr[all_data$verb == "wearily"] =log(0.01315789474)
all_data$scr[all_data$verb == "sternly"] =log(0.005235602094)
all_data$scr[all_data$verb == "gently"] =log(0.003521126761)
all_data$scr[all_data$verb == "wistfully"] =log(0.01973684211)
all_data$scr[all_data$verb == "ruefully"] =log(0.007407407407)
all_data$scr[all_data$verb == "calmly"]=log(0.009132420091)
all_data$scr[all_data$verb == "dryly"]=log(0.005882352941)
# Mean center SCR score (or convert it to a z-score, further divide it by sd?)
all_data$scr <- scale(all_data$scr, center = TRUE)
# all_data <- all_data |>
#   filter(condition %in% c("say", "say_adv")) |>
#   mutate(centered_scr = scr - mean(scr))

###Exclude Subjects###
excluded_subjects <- c(198,200) ##excluded due to non-native speaker status

mos_data <- subset(mos_data, !is.element(mos_data$workerid, excluded_subjects))
length(unique(all_data$workerid))

practice_data=subset(mos_data,block_id == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(mos_data, !is.element(workerid, practice_good_data$workerid))$workerid)
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
length(eligible_subjects)

is.element(excluded_subjects,eligible_subjects)
mos_data = subset(mos_data, workerid %in% eligible_subjects)
##################################Getting data ready for plotting and analysis#############################################
# Data cleaning      
mos_data_overall <- mos_data %>% 
     filter(block_id != "practice") %>%
     filter(condition %in% c("embed_focus","verb_focus"))%>%
     mutate(bg = case_when(bg_response == "embed" ~ 1,
                           bg_response == "verb" ~ 0)) %>%
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
              mutate(condition = ifelse(condition=="verb_focus", "Verb Focus", "Embedded Focus")) %>%
                  #  1 -> verb focus, 0 -> noun focus;lower  value,  more backgrounded
              mutate(bg = case_when(bg_response == "embed" ~ 1,
                                    bg_response == "verb" ~ 0)) %>%
              group_by(condition) %>%
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
                        geom_bar(stat="identity", width=0.6, aes(color=condition)) +
                        geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,  show.legend = FALSE) +
                        scale_fill_manual(values=cbPalette, name = NULL) +
                        theme_bw()+
                        xlab("Condition") +
                        scale_color_manual(values=cbPalette, name=NULL) +
                        guides(color = "none")+
                        guides(fill = "none")+
                        theme(legend.position="bottom",
                        axis.text.x = element_text(size=8)) +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1)) +
  geom_signif(comparisons=list(c("Good Filler", "Verb Focus")), annotations="***",y_position = 0.9) +
  geom_signif(comparisons=list(c("Embedded Focus", "Verb Focus")), annotations="***",y_position = 0.8) +
  geom_signif(comparisons=list(c("Bad Filler", "Verb Focus")), annotations="***",y_position = 0.7)
                      # + scale_x_discrete(labels = c("Embedded focus", "filler bad 1", "filler bad 2", "filler good 1", "filler good 2", "Verb focus"))
                      # +facet_wrap(~ID) 

 
mos_acc_graph
ggsave(mos_acc_graph, file="../graphs/main/mos_acc.pdf", width=4, height=3)

###########BG question plot#######################

mos_bg_graph <- ggplot(mos_bg_means, aes(x=condition, y=Mean, fill=condition)) +
                         geom_bar(stat="identity", width=0.6, aes(color=condition)) +
                         geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,  show.legend = FALSE) +
                         scale_fill_manual(values=c("#56B4E9", "#009E73"), name = NULL) +
             # scale_fill_manual(values=cbPalette, name = NULL) +
                        theme_bw()+
                        xlab("Condition") +
                        scale_color_manual(values=c("#56B4E9", "#009E73"),name=NULL) +
                        # scale_color_manual(values=cbPalette, name=NULL) +
                        scale_y_continuous(name="Proportion of Backgrounded\nInterpretation of the Embedded Clause", limits=c(0, 1)) + 
                          guides(color = "none") +
                          guides(fill = "none") +
                          theme(legend.position="bottom",
                                axis.text.x = element_text(size=10),
                                axis.title=element_text(size=10)) +
  geom_signif(comparisons = list(c("Embedded Focus", "Verb Focus")), 
              annotations="***",y_position = 0.9)
 mos_bg_graph
 ggsave(mos_bg_graph, file="../graphs/main/mos_bg.pdf", width=4, height=3)
 
 
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
   # geom_text(size=3, color="black", alpha=0.6, hjust=-0.1, vjust=0.2)+
   geom_text(size=3, color="black", alpha=0.6, hjust="inward", vjust="inward")+
   geom_line(aes(group=verb),
             color = "black",
             alpha = 0.6,
             linetype = "dashed") +
   # scale_x_continuous(expand=expansion(mult = 0.08)) +
   xlab("Log-transformed SCR score")+
   ylab("Mean Acceptability Rating") +
   scale_color_manual(values=c("#56B4E9", "#009E73"), 
                      labels=c("Embedded\nFocus", "Verb\nFocus"),
                      name = "Condition") +
   scale_fill_manual(values=c("#56B4E9", "#009E73"), 
                     labels=c("Embedded\nFocus", "Verb\nFocus"),
                     name = "Condition") +
   theme(legend.text=element_text(size=10))
 mos_scr_plot
 ggsave(mos_scr_plot, file="../graphs/main/mos_scr_plot.pdf", width=6, height=4)
 
 
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
   geom_text(size=3, color="black", alpha=0.6, hjust="inward", vjust="inward") +
   geom_line(aes(group=verb),
             color = "black",
             alpha = 0.6,
             linetype = "dashed") +
   # scale_x_continuous(expand=expansion(mult = 0.08)) +
   xlab("Log-transformed verb-frame frequency score")+
   ylab("Mean Acceptability Rating") +
   scale_color_manual(values=c("#56B4E9", "#009E73"), 
                      labels=c("Embedded\nFocus", "Verb\nFocus"),
                      name = "Condition") +
   scale_fill_manual(values=c("#56B4E9", "#009E73"), 
                     labels=c("Embedded\nFocus", "Verb\nFocus"),
                     name = "Condition") +
   theme(legend.text=element_text(size=10))
 mos_vff_plot
 ggsave(mos_vff_plot, file="../graphs/main/mos_vff_plot.pdf", width=6, height=4)
 

 
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
 
 
 #####acceptability analysis######
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition %in% c("filler_bad_1","filler_bad_2")] <- "filler_bad"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition %in% c("filler_good_1","filler_good_2")] <- "filler_good"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "embed_focus"] <- "embed_focus"
mos_data_acc_noprac$prim_cond[mos_data_acc_noprac$condition == "verb_focus"] <- "verb_focus"
mos_data_acc_noprac$prim_cond<- as.factor(mos_data_acc_noprac$prim_cond)
mos_data_acc_noprac$prim_cond<-relevel(mos_data_acc_noprac$prim_cond, ref = "verb_focus")

acc_model <- lmer(acceptability_rating ~ prim_cond + 
                    (1+condition|workerid)+
                    (1+condition|item_id),
                  data = mos_data_acc_noprac)
summary(acc_model)

######BG analysis###########
mos_data_bg_nofill<- mos_data_bg_nofill %>%
  mutate(cond = ifelse(condition=="verb_focus", "Verb Focus", "Embedded Focus")) %>%
  #  1 -> verb focus, 0 -> noun focus; the lower the value, the more backgrounded it is
  mutate(bg = case_when(bg_response == "embed" ~ 1,
                        bg_response == "verb" ~ 0
  ))
mos_data_bg_nofill$bg <- as.numeric(mos_data_bg_nofill$bg)
mos_data_bg_nofill$condition <- as.factor(mos_data_bg_nofill$condition)
contrasts(mos_data_bg_nofill$condition)=contr.sum(2)
bg_model <- glmer(bg~condition+
                    (1+condition|workerid),
                  family = "binomial",
                  data=mos_data_bg_nofill)
summary(bg_model)

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
