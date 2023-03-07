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
source('locative_helpers.R')

`%notin%` <- Negate(`%in%`)
theme_set(theme_bw())
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#######################Load Data ######################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
all_data<-read.csv("../../../data/2_sayAdv/pilot/2_sayAdv-pilot-ling145-trials.csv")
#######################Participant Exclusion###########################
##Add log SCR score to verbs###
all_data$scr[all_data$verb == "say softly"] = log(0.001729355815)
all_data$scr[all_data$verb == "say quietly"] = log(0.00321248279)
all_data$scr[all_data$verb == "say loudly"] = log(0.01222493888)
all_data$scr[all_data$verb == "say bluntly"] =log(0.09223300971)
all_data$scr[all_data$verb == "say cheerfully"] =log(0.009230769231)
all_data$scr[all_data$verb == "say wearily"] =log(0.01315789474)
all_data$scr[all_data$verb == "say sternly"] =log(0.005235602094)
all_data$scr[all_data$verb == "say gently"] =log(0.003521126761)
all_data$scr[all_data$verb == "say wistfully"] =log(0.01973684211)
all_data$scr[all_data$verb == "say ruefully"] =log(0.007407407407)
all_data$scr[all_data$verb == "say calmly"]=log(0.009132420091)
all_data$scr[all_data$verb == "say dryly"]=log(0.005882352941)
all_data$scr <- scale(all_data$scr, center = TRUE)


###Exclude Subjects###
excluded_subjects <- c(1076)  # remove duplicates
practice_data=subset(all_data,block_id == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(all_data, !is.element(workerid, practice_good_data$workerid))$workerid)
all_data=subset(all_data, is.element(workerid, practice_good_data$workerid))
length(unique(all_data$workerid))
critical_data <- subset(all_data,block_id != "practice")
critical_data$acceptability_rating <- as.numeric(critical_data$acceptability_rating)
filler_data = subset(critical_data, condition == "filler_good")
ungram_data = subset(critical_data, condition == "filler_bad")
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
all_data = subset(all_data, workerid %in% eligible_subjects)

# check the number of observations for each adv
all_data |>
  filter(condition=="say_adv") |>
  group_by(verb) |>
  summarize(count=n())
##################################Getting data ready for plotting and analysis#############################################
# Data cleaning      
df_summary <- all_data %>%
  # exclude practice trials
  filter(block_id != "practice") %>%
  # combine all good/bad fillers together
  mutate(condition = case_when(condition == "filler_good" ~ "Good Filler",
                               condition == "filler_bad" ~ "Bad Filler",
                               condition == "say" ~ "Say",
                               condition == "say_adv" ~ "Say + Adv")) %>%
  group_by(condition) %>%
  summarize(Mean = mean(acceptability_rating), 
            CILow = ci.low(acceptability_rating),
            CIHigh = ci.high(acceptability_rating)) %>%
  ungroup() %>%
  mutate(YMin=Mean-CILow,
         YMax=Mean+CIHigh) %>%
  # reorder the factors
  mutate(condition = fct_relevel(condition, "Good Filler", "Say", "Say + Adv", "Bad Filler"))

df_adverb <- all_data |>
  filter(condition %in% c("say", "say_adv")) |>
  mutate(verb = gsub("[a-z]+ ", "\\1", verb)) |>
  group_by(verb, condition) |>
  summarize(Mean = mean(acceptability_rating), 
            CILow = ci.low(acceptability_rating),
            CIHigh = ci.high(acceptability_rating)) |>
  ungroup() |>
  mutate(YMin=Mean-CILow,
         YMax=Mean+CIHigh) |>
  mutate(verb = fct_relevel(verb, "say"))
  

##########Acceptability plot########################
acc_graph <- ggplot(df_summary,
                    aes(x=condition, y=Mean, fill=condition)) +
  geom_bar(stat="identity", width=0.6, aes(color=condition)) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,show.legend = FALSE) +
  scale_fill_manual(values=cbPalette, name = NULL, guide="none") +
  xlab("Condition") +
  scale_color_manual(values=cbPalette, name=NULL, guide="none") +
  theme(legend.position="bottom",
        axis.text.x = element_text(size=8)) +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1))
acc_graph
ggsave(acc_graph, file="../graphs/pilot/mos_acc.pdf", width=4, height=3)

adv_graph <- ggplot(df_adverb,
                    aes(x=verb, y=Mean, color=condition)) +
  geom_point(position="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,show.legend = FALSE) +
  xlab("Adverb") +
  scale_color_manual(values=cbPalette[2:4], name=NULL, guide="none") +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1))
  # guides()
adv_graph
ggsave(adv_graph, file="../graphs/pilot/adv_graph.pdf", width=8, height=3)

#######SCR plot############
mos_scr_means <- all_data |>
  filter(condition == "say_adv") |>
  mutate(verb = gsub("[a-z]+ ", "\\1", verb)) |>
  group_by(verb) |>
  summarise(ACC = mean(acceptability_rating),
            CILow = ci.low(acceptability_rating),
            CIHigh = ci.high(acceptability_rating),
            SCR = mean(scr)) |>
  ungroup() |>
  mutate(YMin=ACC-CILow,
         YMax=ACC+CIHigh)
 

mos_scr_plot <- ggplot(mos_scr_means,
                        aes(x = SCR, y = ACC,
                            label=verb)) +
  geom_point() +
  geom_smooth(method = "lm", color="black") +
  geom_text(size=3, color="black", alpha=0.6, hjust=0.6, vjust=1.5) +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,show.legend = FALSE) +
  xlab("Log-transformed SCR score") +
  ylab("Mean Acceptability Rating")
  # scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1))
mos_scr_plot
ggsave(mos_scr_plot, file="../graphs/pilot/mos_scr_plot_error_bar.pdf", width=6, height=4)
 
 
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
  mutate(bg = case_when(cond == "Verb Focus" & bg_response == "correct" ~ 1,
                        cond == "Verb Focus" & bg_response == "incorrect" ~ 0,
                        cond == "Embedded Focus" & bg_response == "incorrect" ~ 1,
                        cond == "Embedded Focus" & bg_response == "correct" ~ 0
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
