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
all_data<-read.csv("../../../data/2_sayAdv/main/2_sayAdv-main-trials.csv")
freq_data <- read.csv("../../../data/exp2_freq.csv")
#######################Participant Exclusion###########################
##Add log SCR score and VFF to verbs###
# all_data$scr[all_data$verb == "say softly"] = log(0.001729355815)
# all_data$scr[all_data$verb == "say quietly"] = log(0.00321248279)
# all_data$scr[all_data$verb == "say loudly"] = log(0.01222493888)
# all_data$scr[all_data$verb == "say bluntly"] =log(0.09223300971)
# all_data$scr[all_data$verb == "say cheerfully"] =log(0.009230769231)
# all_data$scr[all_data$verb == "say wearily"] =log(0.01315789474)
# all_data$scr[all_data$verb == "say sternly"] =log(0.005235602094)
# all_data$scr[all_data$verb == "say gently"] =log(0.003521126761)
# all_data$scr[all_data$verb == "say wistfully"] =log(0.01973684211)
# all_data$scr[all_data$verb == "say ruefully"] =log(0.007407407407)
# all_data$scr[all_data$verb == "say calmly"]=log(0.009132420091)
# all_data$scr[all_data$verb == "say dryly"]=log(0.005882352941)

all_data <- left_join(all_data, freq_data, by="verb") %>% 
  rename(vff = v_sc) %>% 
  mutate(scr = log(scr),
         vff = log(vff))
# Mean center SCR score (or convert it to a z-score, further divide it by sd?)
all_data$scr <- scale(all_data$scr, center = TRUE)
all_data$vff <- scale(all_data$vff, center=TRUE)

# all_data <- all_data |>
#   filter(condition %in% c("say", "say_adv")) |>
#   mutate(centered_scr = scr - mean(scr))

length(unique(all_data$workerid))
###Exclude Subjects###
excluded_subjects <- c(1243, 1179, 1199, 1175, 1237)  # excluded due to non-native and bilingual speaker status

all_data <- subset(all_data, !is.element(all_data$workerid, excluded_subjects))
length(unique(all_data$workerid))

practice_data=subset(all_data,block_id == "practice")
practice_good_data=subset(practice_data, wrong_attempts <= 1)
excluded_subjects <- c(excluded_subjects, subset(all_data, !is.element(workerid, practice_good_data$workerid))$workerid)
all_data=subset(all_data, is.element(workerid, practice_good_data$workerid))

length(unique(all_data$workerid))
length(unique(practice_good_data$workerid))

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

excluded_subjects %in% eligible_subjects
all_data = subset(all_data, workerid %in% eligible_subjects)

# check the number of observations for each adv
all_data |>
  filter(condition=="say_adv") |>
  group_by(verb) |>
  summarize(count=n()) |>
  ungroup() |>
  summarize(mean=mean(count))
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
  geom_signif(comparisons=list(c("Say + Adv", "Say")), annotations="***",y_position = 0.86) +
  geom_signif(comparisons=list(c("Bad Filler", "Say")), annotations="***",y_position = 0.94) +
  scale_fill_manual(values=cbPalette, name = NULL, guide="none") +
  xlab("Condition") +
  scale_color_manual(values=cbPalette, name=NULL, guide="none") +
  theme(legend.position="bottom",
        axis.text.x = element_text(size=8)) +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1))
acc_graph
ggsave(acc_graph, file="../graphs/main/mos_acc_sig.pdf", width=4, height=3)

adv_graph <- ggplot(df_adverb,
                    aes(x=verb, y=Mean, color=condition)) +
  geom_point(position="identity") +
  geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,show.legend = FALSE) +
  xlab("Adverb") +
  scale_color_manual(values=cbPalette[2:4], name=NULL, guide="none") +
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1))
  # guides()
adv_graph
ggsave(adv_graph, file="../graphs/main/adv_graph.pdf", width=6, height=4)

#######SCR plot############
scr_means <- all_data |>
  filter(condition == "say_adv") |>
  mutate(verb = ifelse(condition=="say_adv", gsub("[a-z]+ ", "\\1", verb), verb)) |>
  group_by(verb) |>
  summarise(ACC = mean(acceptability_rating),
            CILow = ci.low(acceptability_rating),
            CIHigh = ci.high(acceptability_rating),
            SCR = mean(scr)) |>
  ungroup() |>
  mutate(YMin=ACC-CILow,
         YMax=ACC+CIHigh)

scr_plot <- ggplot(scr_means,
                   aes(x = SCR,
                       y = ACC,
                      label=verb)) +
  geom_point() +
  geom_smooth(method = "lm", color="black") +
  geom_text(size=3, color="black", alpha=0.6, hjust=0.8, vjust=1.5) +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,show.legend = FALSE) +
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16)) +
  xlab("Log-transformed SCR score") +
  # ylab("Mean Acceptability Rating")
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1))
scr_plot
ggsave(scr_plot, file="../graphs/main/mos_scr_plot.pdf", width=6, height=4)


#######VFF plot############
vff_means <- all_data |>
  filter(condition == "say_adv") |>
  mutate(verb = ifelse(condition=="say_adv", gsub("[a-z]+ ", "\\1", verb), verb)) |>
  group_by(verb) |>
  summarise(ACC = mean(acceptability_rating),
            CILow = ci.low(acceptability_rating),
            CIHigh = ci.high(acceptability_rating),
            VFF = mean(vff)) |>
  ungroup() |>
  mutate(YMin=ACC-CILow,
         YMax=ACC+CIHigh)

vff_plot <- ggplot(vff_means,
                   aes(x = VFF,
                       y = ACC,
                       label=verb)) +
  geom_point() +
  geom_smooth(method = "lm", color="black") +
  geom_text(size=3, color="black", alpha=0.6, vjust="inner") +
  # geom_errorbar(aes(ymin=YMin,ymax=YMax),width=.2,show.legend = FALSE) +
  xlab("Log-transformed Predicate-frame frequency") +
  # ylab("Mean Acceptability Rating")
  scale_y_continuous(name="Mean Acceptability Rating", limits=c(0, 1))
vff_plot
ggsave(vff_plot, file="../graphs/main/mos_vff_plot.pdf", width=6, height=4)

##############trial_order plot#############
trial_means = all_data %>% 
   filter(condition %in% c("say", "say_adv") )%>%
   group_by(trial_num, condition) %>%
   summarise(Count=n(),
             Mean = mean(acceptability_rating))
 
trial_plot <- ggplot(trial_means,aes(x = trial_num, 
                                     y = Mean, 
                                     color = condition, 
                                     fill=condition)) +
  geom_point(alpha=0.6) +
  scale_fill_manual(values=cbPalette[2:4], name="Condition", labels=c("Say", "Say Adv")) +
  scale_color_manual(values=cbPalette[2:4], name="Condition", labels=c("Say", "Say Adv")) +
   geom_smooth(method = "lm") +
   xlab("Presentation order") +
   ylab("Mean Acceptability Rating")
trial_plot
ggsave(trial_plot, file="../graphs/main/trial_plot.pdf", width=6, height=4)
 

#########################Stats##################################
#####acceptability analysis######
data_noprac <- all_data |>
  filter(block_id != "practice") |>
  mutate(prim_cond = relevel(as.factor(condition), ref="say"))

acc_model <- lmer(acceptability_rating ~ prim_cond + 
                    (1+prim_cond|workerid) +
                    (1+prim_cond|item_id),
                  data = data_noprac)
summary(acc_model)


######SCR analysis#######
scr_model_data <- all_data %>% 
  filter(condition == "say_adv") |>
  mutate(condition = as.factor(condition))


scr_model <- lmer(acceptability_rating ~ scr + 
                    (1+scr|item_id) +
                    (1+scr|workerid),
                  data=scr_model_data)
summary(scr_model)


######VFF analysis#######
vff_model_data <- all_data %>% 
  filter(condition == "say_adv") |>
  mutate(condition = as.factor(condition))


vff_model <- lmer(acceptability_rating ~ vff + 
                    (1+vff|item_id) +
                    (1+vff|workerid),
                  data=vff_model_data)
summary(vff_model)


####### Satiation analysis#########
trial_data <- all_data %>%
  filter(condition %in% c("say", "say_adv"))|>
  mutate(condition = relevel(as.factor(condition), ref="say"))
contrasts(trial_data$condition) <- contr.sum(2)
levels(trial_data$condition)

model_trial <- lmer(acceptability_rating ~ condition * trial_num + 
                      (1+condition * trial_num|item_id)+
                      (1+ condition * trial_num|workerid),
                    data = trial_data)
summary(model_trial)
