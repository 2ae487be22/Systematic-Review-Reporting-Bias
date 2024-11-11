#### Analysis code for evaluation of reporting bias in Cochrane SRs

# R version - analysed on 4.41
R.Version()$version.string


##### Import libraries ----
library(here)
library(data.table)
library(ggplot2)
library(tidyverse)
library(readxl)
library(table1)

##### Import data ----
# This will need adapting to your own project directory

here::i_am("ReportingBiasAnalysis1.R")

## SR level data

IncludedSRNums <- c(1:51, 54, 56, 58)
IncludedSRNums <- IncludedSRNums[c(-11,-22, -24, -34)]

SR_level_data <- read_xlsx(here("Cleaned Data", "SR_level_data_public.xlsx"))
SR_level_data <- data.table(SR_level_data)

## Trial level data

Trial_level_data <- read_xlsx(here("Cleaned Data", "Trial_level_data_public.xlsx"))
Trial_level_data <- data.table(Trial_level_data)

##### SR level summary ----

table1(~ Num_papers + Num_part + Comment_bias + Correct_bias + SR_reg + Outcomes_match +
         Primary_object + Primary_type + Int_type + Num_ongoing + Num_AC, data = SR_level_data,
       render.continuous=c(.="Mean (CV%)", .="Median [Q1, Q3]", .="Min, Max"))

##### Trial level summary ----


#### Pre-registration of included studies by trial ----

# Trial_level_data[Trial_type == "Included", .(IncPreRegRate = .SD[Pre_reg == "Y", .N]/.SD[, .N]), by = SR_ID]

Inc_pre_reg_rate_bytrial <- Trial_level_data[Trial_type == "Included", .(IncPreRegRate_bytrial = .SD[Pre_reg == "Y", .N]/.SD[, .N],
                                                                         IncPreReg_trials = .SD[Pre_reg == "Y", .N],
                                                                         IncTotal_trials = .SD[, .N]), by = SR_ID]

# Overall rate
sum(Inc_pre_reg_rate_bytrial$IncPreReg_trials)
sum(Inc_pre_reg_rate_bytrial$IncTotal_trials)
sum(Inc_pre_reg_rate_bytrial$IncPreReg_trials)/sum(Inc_pre_reg_rate_bytrial$IncTotal_trials)

mean(Inc_pre_reg_rate_bytrial$IncPreRegRate_bytrial)

#### Pre-registration of included studies by individual ----

Trial_level_data[Trial_type == "Included", .(IncPreRegRate_byperson = .SD[Pre_reg == "Y", sum(Num_part)]/.SD[, sum(Num_part)],
                                             IncPreReg_people = .SD[Pre_reg == "Y", sum(Num_part)],
                                             IncTotal_people = .SD[, sum(Num_part)]), by = SR_ID]


Inc_pre_reg_rate_byperson <- Trial_level_data[Trial_type == "Included", .(IncPreRegRate_byperson = .SD[Pre_reg == "Y", sum(Num_part, na.rm = TRUE)]/.SD[, sum(Num_part, na.rm = TRUE)],
                                                                          IncPreReg_people = .SD[Pre_reg == "Y", sum(Num_part, na.rm = TRUE)],
                                                                          IncTotal_people = .SD[, sum(Num_part, na.rm = TRUE)]), by = SR_ID]

# Overall rate

sum(Inc_pre_reg_rate_byperson$IncPreReg_people)
sum(Inc_pre_reg_rate_byperson$IncTotal_people)
sum(Inc_pre_reg_rate_byperson$IncPreReg_people)/sum(Inc_pre_reg_rate_byperson$IncTotal_people)

mean(Inc_pre_reg_rate_byperson$IncPreRegRate_byperson)

# Is it being driven up by SR37

sum(Inc_pre_reg_rate_byperson[SR_ID != 37,]$IncPreReg_people)
sum(Inc_pre_reg_rate_byperson[SR_ID != 37,]$IncTotal_people)
sum(Inc_pre_reg_rate_byperson[SR_ID != 37,]$IncPreReg_people)/sum(Inc_pre_reg_rate_byperson[SR_ID != 37,]$IncTotal_people)

#### Publication of relevant data by trial ----

# This represents published in a way that we can find or available for meta-analysis
# This variable is sensitive to timing of overdue and whether AC included

Trial_level_data[Trial_type == "Ongoing" & Includes_primary_outcome == "Y" & Overdue == "Y", Published_factor := "No"]
Trial_level_data[Trial_type == "AC" & Includes_primary_outcome == "Y" & Overdue == "Y" & AC_Included == "Y", Published_factor := "No"]


# Selects only relevant published
Trial_level_data[Trial_type == "Included", Published_factor := "Yes"]
Trial_level_data[Trial_type == "Ongoing" & Includes_primary_outcome == "Y" &
                   (Now_published == "Y" |Now_published == "PublishedNow" | Now_published == "PublishedPreviously"), 
                 Published_factor := "Yes"]
Trial_level_data[Trial_type == "AC" & Includes_primary_outcome == "Y" & AC_Included == "Y" &
                   (Now_published == "Y" | Now_published == "PublishedNow" | Now_published == "PublishedPreviously"), 
                 Published_factor := "Yes"]

summary(as.factor(Trial_level_data$Now_published))
summary(as.factor(Trial_level_data$Published_factor))

Published_trials <- Trial_level_data[,
                                     .(RelevantPublishedRate_bytrial = .SD[Published_factor == "Yes", .N]/.SD[!is.na(Published_factor), .N],
                                       RelevantPublished_trials = .SD[Published_factor == "Yes", .N],
                                       RelevantTotal_trials = .SD[!is.na(Published_factor), .N]), 
                                     by = SR_ID]

sum(Published_trials$RelevantPublished_trials)
sum(Published_trials$RelevantTotal_trials)
sum(Published_trials$RelevantPublished_trials)/sum(Published_trials$RelevantTotal_trials)

mean(Published_trials$RelevantPublishedRate_bytrial)

#### Publication of relevant data by individual ----

Published_byperson <- Trial_level_data[,
                                       .(RelevantPublishedRate_people = .SD[Published_factor == "Yes", sum(Num_part, na.rm = TRUE)]/.SD[!is.na(Published_factor), sum(Num_part, na.rm = TRUE)],
                                         RelevantPublished_people = .SD[Published_factor == "Yes", sum(Num_part, na.rm = TRUE)],
                                         RelevantTotal_people = .SD[!is.na(Published_factor), sum(Num_part, na.rm = TRUE)]), 
                                       by = SR_ID]

sum(Published_byperson$RelevantPublished_people)
sum(Published_byperson$RelevantTotal_people)
sum(Published_byperson$RelevantPublished_people)/sum(Published_byperson$RelevantTotal_people)

mean(Published_byperson$RelevantPublishedRate_people)


##### Registration rate of all relevant trials ----

Relevant_pre_reg_rate_bytrial <- Trial_level_data[Trial_type == "Included" | (Trial_type == "Ongoing" & Includes_primary_outcome == "Y")| (Trial_type == "AC" & Includes_primary_outcome == "Y" & AC_Included == "Y"), 
                                                  .(PreRegRate_bytrial = .SD[Pre_reg == "Y", .N]/.SD[, .N],
                                                    PreReg_trials = .SD[Pre_reg == "Y", .N],
                                                    Total_trials = .SD[, .N]), 
                                                  by = SR_ID]

sum(Relevant_pre_reg_rate_bytrial$PreReg_trials)
sum(Relevant_pre_reg_rate_bytrial$Total_trials)
sum(Relevant_pre_reg_rate_bytrial$PreReg_trials)/sum(Relevant_pre_reg_rate_bytrial$Total_trials)

mean(Relevant_pre_reg_rate_bytrial$PreRegRate_bytrial)

##### Registration rate of all relevant participants ----

Relevant_pre_reg_rate_byperson <- Trial_level_data[Trial_type == "Included" | (Trial_type == "Ongoing" & Includes_primary_outcome == "Y")| (Trial_type == "AC" & Includes_primary_outcome == "Y" & AC_Included == "Y"), 
                                                   .(PreRegRate_byperson = .SD[Pre_reg == "Y", sum(Num_part, na.rm = TRUE)]/.SD[, sum(Num_part, na.rm = TRUE)],
                                                     PreReg_people = .SD[Pre_reg == "Y", sum(Num_part, na.rm = TRUE)],
                                                     Total_people = .SD[, sum(Num_part, na.rm = TRUE)]), 
                                                   by = SR_ID]

sum(Relevant_pre_reg_rate_byperson$PreReg_people)
sum(Relevant_pre_reg_rate_byperson$Total_people)
sum(Relevant_pre_reg_rate_byperson$PreReg_people)/sum(Relevant_pre_reg_rate_byperson$Total_people)

mean(Relevant_pre_reg_rate_byperson$PreRegRate_byperson)

##### Registration by type of intervention (by trial) ----

for (i in SR_level_data$SR_ID){
  Trial_level_data[SR_ID == i, SR_Int_type := SR_level_data[SR_ID == i]$Int_type]
}

Trial_level_data[Trial_type == "Included", .(IncPreRegRate_bytrial = .SD[Pre_reg == "Y", .N]/.SD[, .N],
                                             IncPreReg_trials = .SD[Pre_reg == "Y", .N],
                                             IncTotal_trials = .SD[, .N]), by = SR_Int_type]

##### Registration by date (by trial) ----

Trial_level_data[Trial_type == "Included" & Trial_date < 2010
                 , .(IncPreRegRate_bytrial = .SD[Pre_reg == "Y", .N]/.SD[, .N],
                     IncPreReg_trials = .SD[Pre_reg == "Y", .N],
                     IncTotal_trials = .SD[, .N])]

Trial_level_data[Trial_type == "Included" & Trial_date < 2015 & Trial_date >= 2010
                 , .(IncPreRegRate_bytrial = .SD[Pre_reg == "Y", .N]/.SD[, .N],
                     IncPreReg_trials = .SD[Pre_reg == "Y", .N],
                     IncTotal_trials = .SD[, .N])]

Trial_level_data[Trial_type == "Included" & Trial_date < 2020 & Trial_date >= 2015
                 , .(IncPreRegRate_bytrial = .SD[Pre_reg == "Y", .N]/.SD[, .N],
                     IncPreReg_trials = .SD[Pre_reg == "Y", .N],
                     IncTotal_trials = .SD[, .N])]

Trial_level_data[Trial_type == "Included" & Trial_date < 2025 & Trial_date >= 2020
                 , .(IncPreRegRate_bytrial = .SD[Pre_reg == "Y", .N]/.SD[, .N],
                     IncPreReg_trials = .SD[Pre_reg == "Y", .N],
                     IncTotal_trials = .SD[, .N])]

##### Publication by date

Trial_level_data[Trial_date < 2010,
                 .(RelevantPublishedRate_bytrial = .SD[Published_factor == "Yes", .N]/.SD[!is.na(Published_factor), .N],
                   RelevantPublished_trials = .SD[Published_factor == "Yes", .N],
                   RelevantTotal_trials = .SD[!is.na(Published_factor), .N])
]

Trial_level_data[Trial_date < 2015 & Trial_date >= 2010,
                 .(RelevantPublishedRate_bytrial = .SD[Published_factor == "Yes", .N]/.SD[!is.na(Published_factor), .N],
                   RelevantPublished_trials = .SD[Published_factor == "Yes", .N],
                   RelevantTotal_trials = .SD[!is.na(Published_factor), .N])
]

Trial_level_data[Trial_date < 2020 & Trial_date >= 2015,
                 .(RelevantPublishedRate_bytrial = .SD[Published_factor == "Yes", .N]/.SD[!is.na(Published_factor), .N],
                   RelevantPublished_trials = .SD[Published_factor == "Yes", .N],
                   RelevantTotal_trials = .SD[!is.na(Published_factor), .N])
]

Trial_level_data[Trial_date < 2025 & Trial_date >= 2020,
                 .(RelevantPublishedRate_bytrial = .SD[Published_factor == "Yes", .N]/.SD[!is.na(Published_factor), .N],
                   RelevantPublished_trials = .SD[Published_factor == "Yes", .N],
                   RelevantTotal_trials = .SD[!is.na(Published_factor), .N])
]


##### Sensitivity analysis publication rate excluding a/c (by trial) -----

# Only 1 trial found to be A/C, overdue, and not published
dim(Trial_level_data[Trial_type == "AC" & Includes_primary_outcome == "Y" & AC_Included == "Y" & Now_published == "No" & Overdue == "Y", ])

# 13 trials fond to be A/C, relevant, and relevant
dim(Trial_level_data[Trial_type == "AC" & Includes_primary_outcome == "Y" & AC_Included == "Y" &
                       (Now_published == "Y" | Now_published == "PublishedNow" | Now_published == "PublishedPreviously"),  ])

summary(as.factor(Trial_level_data$Reason_AC))

# This version excludes awaiting classification
Trial_level_data[Trial_type == "Ongoing" & Includes_primary_outcome == "Y" & Overdue == "Y", Published_factor2 := "No"]


# Selects only relevant published
Trial_level_data[Trial_type == "Included", Published_factor2 := "Yes"]
Trial_level_data[Trial_type == "Ongoing" & Includes_primary_outcome == "Y" &
                   (Now_published == "Y" |Now_published == "PublishedNow" | Now_published == "PublishedPreviously"), 
                 Published_factor2 := "Yes"]


summary(as.factor(Trial_level_data$Now_published))
summary(as.factor(Trial_level_data$Published_factor2))

Published_trials_sens1 <- Trial_level_data[,
                                           .(RelevantPublishedRate_bytrial = .SD[Published_factor2 == "Yes", .N]/.SD[!is.na(Published_factor2), .N],
                                             RelevantPublished_trials = .SD[Published_factor2 == "Yes", .N],
                                             RelevantTotal_trials = .SD[!is.na(Published_factor2), .N]), 
                                           by = SR_ID]

sum(Published_trials_sens1$RelevantPublished_trials)
sum(Published_trials_sens1$RelevantTotal_trials)
sum(Published_trials_sens1$RelevantPublished_trials)/sum(Published_trials_sens1$RelevantTotal_trials)

mean(Published_trials_sens1$RelevantPublishedRate_bytrial)

##### Sensitivity analysis publication rate excluding a/c (by individual) -----

Published_byperson_sens1 <- Trial_level_data[,
                                             .(RelevantPublishedRate_people = .SD[Published_factor2 == "Yes", sum(Num_part, na.rm = TRUE)]/.SD[!is.na(Published_factor2), sum(Num_part, na.rm = TRUE)],
                                               RelevantPublished_people = .SD[Published_factor2 == "Yes", sum(Num_part, na.rm = TRUE)],
                                               RelevantTotal_people = .SD[!is.na(Published_factor2), sum(Num_part, na.rm = TRUE)]), 
                                             by = SR_ID]

sum(Published_byperson_sens1$RelevantPublished_people)
sum(Published_byperson_sens1$RelevantTotal_people)
sum(Published_byperson_sens1$RelevantPublished_people)/sum(Published_byperson_sens1$RelevantTotal_people)

mean(Published_byperson_sens1$RelevantPublishedRate_people)

#### Publication of pre-registered data by trial ----

# This looks at rate of pre-registered studies being published

Trial_level_data[Trial_type == "Included" & Pre_reg == "Y", Published_factor3 := "Yes"]
Trial_level_data[Trial_type == "Ongoing" & Includes_primary_outcome == "Y" & Overdue == "Y" & Pre_reg == "Y", Published_factor3 := "No"]
Trial_level_data[Trial_type == "AC" & Includes_primary_outcome == "Y" & Overdue == "Y" & AC_Included == "Y" & Pre_reg == "Y", Published_factor3 := "No"]

Trial_level_data[Trial_type == "Ongoing" & Includes_primary_outcome == "Y" & Pre_reg == "Y" &
                   (Now_published == "Y" |Now_published == "PublishedNow" | Now_published == "PublishedPreviously"), 
                 Published_factor3 := "Yes"]
Trial_level_data[Trial_type == "AC" & Includes_primary_outcome == "Y" & AC_Included == "Y" & Pre_reg == "Y" &
                   (Now_published == "Y" | Now_published == "PublishedNow" | Now_published == "PublishedPreviously"), 
                 Published_factor3 := "Yes"]

summary(as.factor(Trial_level_data$Now_published))
summary(as.factor(Trial_level_data$Published_factor3))

Published_prereg_trials <- Trial_level_data[,
                                            .(RelevantPublishedRegsRate_bytrial = .SD[Published_factor3 == "Yes", .N]/.SD[!is.na(Published_factor3), .N],
                                              RelevantPublishedRegs_trials = .SD[Published_factor3 == "Yes", .N],
                                              RelevantTotalRegs_trials = .SD[!is.na(Published_factor3), .N]), 
                                            by = SR_ID]

sum(Published_prereg_trials$RelevantPublishedRegs_trials)
sum(Published_prereg_trials$RelevantTotalRegs_trials)
sum(Published_prereg_trials$RelevantPublishedRegs_trials)/sum(Published_prereg_trials$RelevantTotalRegs_trials)

mean(Published_prereg_trials[!is.na(RelevantPublishedRegsRate_bytrial)]$RelevantPublishedRegsRate_bytrial)

#### Publication of pre-registered data by individual ----

Published_prereg_byperson <- Trial_level_data[,
                                              .(RelevantPublishedRegsRate_people = .SD[Published_factor3 == "Yes", sum(Num_part, na.rm = TRUE)]/.SD[!is.na(Published_factor3), sum(Num_part, na.rm = TRUE)],
                                                RelevantPublishedRegs_people = .SD[Published_factor3 == "Yes", sum(Num_part, na.rm = TRUE)],
                                                RelevantTotalRegs_people = .SD[!is.na(Published_factor3), sum(Num_part, na.rm = TRUE)]), 
                                              by = SR_ID]

sum(Published_prereg_byperson$RelevantPublishedRegs_people)
sum(Published_prereg_byperson$RelevantTotalRegs_people)
sum(Published_prereg_byperson$RelevantPublishedRegs_people)/sum(Published_prereg_byperson$RelevantTotalRegs_people)

mean(Published_prereg_byperson[!is.na(RelevantPublishedRegsRate_people)]$RelevantPublishedRegsRate_people)

#### Create combined database of results at SR level ----

Rates_by_SR <- Reduce(function (...) { merge(..., all = TRUE) },   # Full join
                      list(Inc_pre_reg_rate_bytrial, Published_trials, Inc_pre_reg_rate_byperson, Published_byperson)) 

Merged_SR_data <- merge(Rates_by_SR, SR_level_data, by = "SR_ID")


#### Graphical outputs ----

Trial_level_data[Trial_type == "Included",
                 Approx_weight := Num_part/sum(.SD[, Num_part], na.rm = TRUE),
                 by = SR_ID]


Trial_level_data[!is.na(Published_factor),
                 Approx_weight2 := Num_part/sum(.SD[, Num_part], na.rm = TRUE),
                 by = SR_ID]



Published_by_SR <- ggplot(data = Trial_level_data[!is.na(Published_factor),], aes(x = factor(SR_ID, levels = Merged_SR_data[order(Num_papers)]$SR_ID), y = Approx_weight2, colour = Published_factor, fill = Published_factor)) + 
  geom_bar(stat = 'identity', alpha = 0.6, width = 0.8) +
  theme(# axis.line = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) + 
  scale_fill_brewer(type = "qual", palette = "Dark2") + scale_colour_brewer(type = "qual", palette = "Dark2") +
  xlab("Reviews ordered by number of included trials") + scale_x_discrete(labels = NULL, breaks = NULL) + 
  ylab("Proportion of participants in relevant published or overdue trials") + labs(fill = "Published", colour = "Published")

ggsave(here("Published_by_SR.pdf"), Published_by_SR, device = "pdf")


# This uses only included trials by leveraging weight
Pre_reg_by_SR <- ggplot(data = Trial_level_data, aes(x = factor(SR_ID, levels = Merged_SR_data[order(Num_papers)]$SR_ID), y = Approx_weight, colour = Pre_reg, fill = Pre_reg)) + 
  geom_bar(stat = 'identity', alpha = 0.6, width = 0.8) +
  theme(# axis.line = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank()
  ) + 
  # scale_fill_brewer(type = "qual", palette = "Dark2") + scale_colour_brewer(type = "qual", palette = "Dark2") +
  xlab("Reviews ordered by number of included trials") + scale_x_discrete(labels = NULL, breaks = NULL) + 
  ylab("Proportion of included participants") + labs(fill = "Pre-registered", colour = "Pre-registered") +
  scale_colour_discrete(labels = c("No", "Yes", "Unknown")) + scale_fill_discrete(labels = c("No", "Yes", "Unknown"))

ggsave(here("Pre_reg_by_SR.pdf"), Pre_reg_by_SR, device = "pdf")


Proportion_boxplot_by_prereg <- ggplot(data = Trial_level_data[!is.na(Pre_reg)], aes(x = Pre_reg, y = Approx_weight)) + 
  geom_violin(aes(fill = Pre_reg)) + geom_boxplot(width = 0.05) + ylab("Proportion of included participants in review") + xlab("Pre-registered") +
  theme(# axis.line = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    legend.position="none"
  ) + scale_x_discrete(labels = c("No", "Yes")) # + scale_fill_brewer(type = "qual", palette = "Dark2")

ggsave(here("Proportion_boxplot_by_prereg.pdf"), Proportion_boxplot_by_prereg, device = "pdf")

