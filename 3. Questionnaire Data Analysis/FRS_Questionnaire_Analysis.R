library("dplyr")
library("ggpubr")
library("ggplot2")
library("tidyverse")
library("rstatix")
library("MASS")
library("lmtest")
library("aod")
library("FSA")
library("dunn.test")
library("coin")

#read in data (full dataset of FRS answers for all participants, belt & control)
FRS_full <- read.csv("6. FRS Questionnaire Analysis/SpaRe_FRS_Scores_BothConditions.csv")

#change name of conditions
FRS_full$Condition[FRS_full$Condition == 0] <- "Control"
FRS_full$Condition[FRS_full$Condition == 1] <- "Belt"


############################ Create datasets ###################################

#separate belt and control data
frs_belt <- filter(FRS_full, Condition == "Belt")
frs_control <- filter(FRS_full, Condition == "Control") %>%
  dplyr::select(-(27:45))

#split belt data into first and second frs
frs_belt_first <- frs_belt %>% dplyr::select(1:26)
frs_belt_second <- frs_belt %>% dplyr::select(c(1:7, 27:45))


#rename question column names
names(frs_control)[8:26] <- paste("Q", 1:19, sep = "")

names(frs_belt_first)[8:26] <- paste("Q", 1:19, sep = "")
names(frs_belt_second)[8:26] <- paste("Q", 1:19, sep = "")

#combine first and second frs belt data with control group
frs_first_belt_control <- rbind(frs_belt_first,frs_control)
frs_second_belt_control <- rbind(frs_belt_second,frs_control)

#add column defining frs
frs_belt_first$FRS <- "First"
frs_belt_second$FRS <- "Second"

#combine first and second frs belt data together into one dataset
frs_belt <- rbind(frs_belt_first,frs_belt_second)

#make dataframes long and reset index
Question <- factor(rep(1:19))

#combine all into one df
frs_control$FRS <- "Control"
frs_full_restructured <- rbind(frs_belt, frs_control)
frs_full_long = reshape(frs_full_restructured, direction="long", varying=8:26, sep = "",
                        idvar = c("Participant_ID", "FRS"), timevar = "Question",
                        v.names = "Answer") %>% dplyr::select(-"Index")

rownames(frs_full_long) <- 1:nrow(frs_full_long)

##all belt participants
frs_belt_long = reshape(frs_belt, direction="long", varying=8:26, sep = "",
               idvar = c("Participant_ID", "FRS"), timevar = "Question",
               v.names = "Answer") %>% dplyr::select(-"Index")

rownames(frs_belt_long) <- 1:nrow(frs_belt_long)


##first FRS of belt participants combined with control participants
frs_first_belt_control_long = reshape(frs_first_belt_control, direction="long", varying=8:26, sep = "",
                        idvar = c("Participant_ID"), timevar = "Question",
                        v.names = "Answer") %>% dplyr::select(-"Index")

rownames(frs_first_belt_control_long) <- 1:nrow(frs_first_belt_control_long)


##second FRS of belt participants combined with control participants
frs_second_belt_control_long = reshape(frs_second_belt_control, direction="long", varying=8:26, sep = "",
                           idvar = c("Participant_ID"), timevar = "Question",
                           v.names = "Answer") %>% dplyr::select(-"Index")

rownames(frs_second_belt_control_long) <- 1:nrow(frs_second_belt_control_long)


#split belt data into item groups: egocentric, allocentric, cardinal

egocentric_belt <- frs_belt_long[frs_belt_long$Question %in% c(1, 4, 5, 8, 10, 12, 13, 14, 15, 18), ]

allocentric_belt <- frs_belt_long[frs_belt_long$Question %in% c(2, 3, 7, 9, 11, 16, 19), ]

cardinal_belt <- frs_belt_long[frs_belt_long$Question %in% c(6, 17), ]


#split 1st FRS of belt & control data into item groups: egocentric, allocentric, cardinal

egocentric_first_belt_control <- frs_first_belt_control_long[frs_first_belt_control_long$Question %in% c(1, 4, 5, 8, 10, 12, 13, 14, 15, 18), ]

allocentric_first_belt_control <- frs_first_belt_control_long[frs_first_belt_control_long$Question %in% c(2, 3, 7, 9, 11, 16, 19), ]

cardinal_first_belt_control <- frs_first_belt_control_long[frs_first_belt_control_long$Question %in% c(6, 17), ]



#split 2nd FRS of belt & control data into item groups: egocentric, allocentric, cardinal

egocentric_second_belt_control <- frs_second_belt_control_long[frs_second_belt_control_long$Question %in% c(1, 4, 5, 8, 10, 12, 13, 14, 15, 18), ]

allocentric_second_belt_control <- frs_second_belt_control_long[frs_second_belt_control_long$Question %in% c(2, 3, 7, 9, 11, 16, 19), ]

cardinal_second_belt_control <- frs_second_belt_control_long[frs_second_belt_control_long$Question %in% c(6, 17), ]



##################### Create summaries for all data groups ########################

#summarise all FRS & grouped data - belt
summary_belt <- group_by(frs_belt_long, FRS, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )

summary_egocentric_belt <- group_by(egocentric_belt, FRS, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )

summary_allocentric_belt <- group_by(allocentric_belt, FRS, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )

summary_cardinal_belt <- group_by(cardinal_belt, FRS, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )



#summarise all FRS & grouped data - 1st FRS of belt & control
summary_first_belt_control <- group_by(frs_first_belt_control_long, Condition, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )

summary_egocentric_first_belt_control <- group_by(egocentric_first_belt_control, Condition, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )

summary_allocentric_first_belt_control <- group_by(allocentric_first_belt_control, Condition, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )

summary_cardinal_first_belt_control <- group_by(cardinal_first_belt_control, Condition, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )


#summarise all FRS & grouped data - 2nd FRS of belt & control
summary_second_belt_control <- group_by(frs_second_belt_control_long, Condition, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )

summary_egocentric_second_belt_control <- group_by(egocentric_second_belt_control, Condition, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )

summary_allocentric_second_belt_control <- group_by(allocentric_second_belt_control, Condition, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )

summary_cardinal_second_belt_control <- group_by(cardinal_second_belt_control, Condition, Question) %>%
  summarise(
    count = n(),
    mean = mean(Answer, na.rm = TRUE),
    SD = sd(Answer, na.rm = TRUE),
    median = median(Answer, na.rm = TRUE),
    IQR = IQR(Answer, na.rm = TRUE)
  )


# Calculate correlation between 1st and second FRS (belt condition)
belt_second_FRS <- frs_belt_long[frs_belt_long$FRS=="Second",]
belt_first_FRS <- frs_belt_long[frs_belt_long$FRS=="First",]
control_FRS <- frs_first_belt_control_long[frs_first_belt_control_long$FRS_2_bool==FALSE,]

correlation_belt_prepost <- cor.test(belt_first_FRS$Answer, belt_second_FRS$Answer, method = "pearson", use = 'complete.obs')
correlation_belt_prepost

### Analysis 
# First, repeating previous steps for the dataframe containing all three FRS conditions
egocentric_full <- frs_full_long[frs_full_long$Question %in% c(1, 4, 5, 8, 10, 12, 13, 14, 15, 18), ]
allocentric_full <- frs_full_long[frs_full_long$Question %in% c(2, 3, 7, 9, 11, 16, 19), ]
cardinal_full <- frs_full_long[frs_full_long$Question %in% c(6, 17), ]

# Adding the "scale" variable
egocentric_full$Scale <- "Egocentric"
allocentric_full$Scale <- "Allocentric"
cardinal_full$Scale <- "Cardinal"

# Combining them back together and thereby updating the frs_full_long dataframe
df_ego_allo <- rbind(egocentric_full, allocentric_full)
frs_full_long <- rbind(df_ego_allo, cardinal_full)

# renaming the Conditions
# frs_full_long$FRS <- as.factor(frs_full_long$FRS)
# frs_full_long$FRS <- recode_factor(frs_full_long$FRS, Control = "Control group", First = "Pre-Training", 
                                #  Second = "Post-Training")


# Plotting the distribution of Answer for the baseline FRs, the post-training FRS, and the control group (after VR exploration) broken down by the 3 FRS subscales
ggplot(frs_full_long, aes(x = FRS, y = Answer)) +
  geom_boxplot(size = .5) +
  geom_jitter(alpha = .25) +
  facet_grid(~fct_relevel(Scale, 'Egocentric', 'Allocentric', 'Cardinal')) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) 


frs_full_long %>%
  ggboxplot(x = "FRS", y = "Answer", 
            fill = "FRS", palette = c("#0276ab", "#ffc308", "#e18335")) +
  geom_point(shape = 1) +
  geom_jitter(alpha = 0.3, width=0.2, height=0) +
  ggtitle("Average") +
  scale_y_continuous(breaks = pretty( c(0,7) , n = 7) ) +
  facet_grid(~fct_relevel(Scale, 'Egocentric', 'Allocentric', 'Cardinal')) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, size = 11),
        axis.title.x = element_text(size = 9),
        axis.title.y = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.text = element_text(size = 7),
        legend.position = 'none')

write.csv(frs_full_long, "6. FRS Questionnaire Analysis/FRS_data.csv")

# Analyzing the data
frs_full_long$Scale <- as.factor(frs_full_long$Scale)
frs_full_long$Subscale <- recode_factor(frs_full_long$Scale, egocentric = "egocentric", allocentric = "allocentric", 
                                   cardinal = "cardinal")

df_analysis <- frs_full_long

frs_full_long$Answer <- as.factor(frs_full_long$Answer)
frs_full_long$Answer <- recode_factor(frs_full_long$Answer, "1" = "Strongly disagree", "2" = "Moderately disagree", "3" = "Slightly disagree", "4" = "Neutral",
                                        "5" = "Slightly agree", "6" = "Moderately agree", "7" = "Strongly agree")


# create the dataframes for the baseline and comparison groups
temp <- filter(df_analysis, FRS=="Control")
df_baseline <- rbind(temp, filter(df_analysis, FRS=="First"))
temp <- filter(df_analysis, FRS=="First")
df_comparison <- rbind(temp, filter(df_analysis, FRS=="Second"))

length(df_comparison$FRS[df_comparison$FRS=="First"])


# All scales together

FRS_OrdinalLogReg <- polr(Answer ~ FRS, data = frs_full_long, Hess = TRUE) 
sum <- summary(FRS_OrdinalLogReg)

wilcox.test(Answer ~ FRS, data=df_baseline)
wilcox_effsize(Answer ~ FRS, data=df_baseline)

wilcox.test(Answer ~ FRS, data=df_comparison, paired=TRUE)
wilcox_effsize(Answer ~ FRS, data=df_comparison, paired = TRUE)

OIM <- polr(Answer ~ 1, data = frs_full_long, Hess = TRUE)
summary(OIM)

# The coefficients can be directly interpreted as ordered log odds. So for the belt condition pre-training we can say that for
# Pre-Training we would expect a 0.078 increase in the participant's self-evaluation compared to the control group ("for a one unit increase in FRS condition (control, pre, post)").
# For Post-training we would expect a 0.717 increase in the responses as compared to the control group. 

# For p-values we create a table of the model coefficients:
(ctable <- coef(summary(FRS_OrdinalLogReg)))
# Calculate and store the p-values, and combine them with our table:
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

(ci <- confint(FRS_OrdinalLogReg)) # default is profiled CI
confint.default(FRS_OrdinalLogReg) # CI assuming normality
# The CI for FRS Post-Training does not contain 0, while the one for FRS pre-training does. 

## odds ratios are easier to interpret than coefficients. 
odd_ratio <- exp(coef(FRS_OrdinalLogReg))
ctable <- cbind(ctable, "odd ratio" = odd_ratio)


## Analysis for baseline

# EGOCENTRIC SCALE
frs_full_long_ego <- frs_full_long[frs_full_long$Scale=="Egocentric",]
FRS_ego_OrdinalLogReg <- polr(as.factor(Answer) ~ FRS, data = frs_full_long_ego, Hess = TRUE) 
sum_ego <- summary(FRS_ego_OrdinalLogReg)

frs_full_long_ego <- df_baseline[df_baseline$Scale=="Egocentric",]
wilcox.test(Answer ~ FRS, data=frs_full_long_ego)
wilcox_effsize(Answer ~ FRS, data=frs_full_long_ego)

OIM_ego <- polr(as.factor(Answer) ~ 1, data = frs_full_long_ego, Hess = TRUE)
summary(OIM_ego)

# The coefficients can be directly interpreted as ordered log odds. So for the belt condition pre-training we can say that for
# Pre-Training we would expect a 0.078 increase in the participant's self-evaluation compared to the control group ("for a one unit increase in FRS condition (control, pre, post)").
# For Post-training we would expect a 0.717 increase in the responses as compared to the control group. 

# For p-values we create a table of the model coefficients:
(ctable_ego <- coef(summary(FRS_ego_OrdinalLogReg)))
# Calculate and store the p-values, and combine them with our table:
p <- pnorm(abs(ctable_ego[, "t value"]), lower.tail = FALSE) * 2
ctable_ego <- cbind(ctable_ego, "p value" = p)
ctable_ego

(ci_ego <- confint(FRS_ego_OrdinalLogReg)) # default is profiled CI
confint.default(FRS_ego_OrdinalLogReg) # CI assuming normality
# The CI for FRS Post-Training does not contain 0, while the one for FRS pre-training does. 

## odds ratios are easier to interpret than coefficients. 
odd_ratio <- exp(coef(FRS_ego_OrdinalLogReg))
ctable_ego <- cbind(ctable_ego, "odd ratio" = odd_ratio)
# Interpretation: For participants in the belt condition, the likelihood of rating themselves higher on the egocentric FRS scale is 1.117 times compared to the controls,  holding all other variables constant.
# Post-training the odds of rating themselves higher on the egocentric scale were 2.048 times that of the control condition, holding all other variables constant.


# AllOCENTRIC SCALE
frs_full_long_allo <- frs_full_long[frs_full_long$Subscale=="Allocentric",]
FRS_allo_OrdinalLogReg <- polr(as.factor(Answer) ~ FRS, data = frs_full_long_allo, Hess = TRUE) 
sum_allo <- summary(FRS_allo_OrdinalLogReg)

frs_full_long_allo <- df_baseline[df_baseline$Scale=="Allocentric",]
wilcox.test(Answer ~ FRS, data=frs_full_long_allo)
wilcox_effsize(Answer ~ FRS, data=frs_full_long_allo)

OIM_allo <- polr(as.factor(Answer) ~ 1, data = frs_full_long_allo, Hess = TRUE)
summary(OIM_allo)

# The coefficients can be directly interpreted as ordered log odds. So for the belt condition pre-training we can say that for
# Pre-Training we would expect a 0.166 increase in the participant's self-evaluation compared to the control group ("for a one unit increase in FRS condition (control, pre, post)").
# For Post-training we would expect a 1.141 increase in the responses as compared to the control group. 

# For p-values we create a table of the model coefficients:
(ctable_allo <- coef(summary(FRS_allo_OrdinalLogReg)))
# Calculate and store the p-values, and combine them with our table:
p <- pnorm(abs(ctable_allo[, "t value"]), lower.tail = FALSE) * 2
ctable_allo <- cbind(ctable_allo, "p value" = p)
ctable_allo

(ci_allo <- confint(FRS_allo_OrdinalLogReg)) # default is profiled CI
confint.default(FRS_allo_OrdinalLogReg) # CI assuming normality
# The CI for FRS Post-Training does not contain 0, while the one for FRS pre-training does. 

## odds ratios are easier to interpret than coefficients. 
odd_ratio <- exp(coef(FRS_allo_OrdinalLogReg))
ctable_allo <- cbind(ctable_allo, "odd ratio" = odd_ratio)
# Interpretation: For participants in the belt condition, the likelihood of rating themselves higher on the allocentric FRS scale is 1.181 times compared to the controls,  holding all other variables constant.
# Post-training the odds of rating themselves higher on the allocentric scale were 3.128 times that of the control condition, holding all other variables constant.


# CARDINAL SCALE
frs_full_long_card <- frs_full_long[frs_full_long$Subscale=="Cardinal",]
FRS_card_OrdinalLogReg <- polr(as.factor(Answer) ~ FRS, data = frs_full_long_card, Hess = TRUE) 
sum_card <- summary(FRS_card_OrdinalLogReg)

frs_full_long_card <- df_baseline[df_baseline$Scale=="Cardinal",]
wilcox.test(Answer ~ FRS, data=frs_full_long_card)
wilcox_effsize(Answer ~ FRS, data=frs_full_long_card)

OIM_card <- polr(as.factor(Answer) ~ 1, data = frs_full_long_card, Hess = TRUE)
summary(OIM_card)

# The coefficients can be directly interpreted as ordered log odds. So for the belt condition pre-training we can say that for
# Pre-Training we would expect a 0.079 increase in the participant's self-evaluation compared to the control group ("for a one unit increase in FRS condition (control, pre, post)").
# For Post-training we would expect a 1.831 increase in the responses as compared to the control group ("for a one unit increase in FRS condition (control, pre, post)").

# For p-values we create a table of the model coefficients:
(ctable_card <- coef(summary(FRS_card_OrdinalLogReg)))
# Calculate and store the p-values, and combine them with our table:
p <- pnorm(abs(ctable_card[, "t value"]), lower.tail = FALSE) * 2
ctable_card <- cbind(ctable_card, "p value" = p)
ctable_card

(ci_card <- confint(FRS_card_OrdinalLogReg)) # default is profiled CI
confint.default(FRS_card_OrdinalLogReg) # CI assuming normality
# The CI for FRS Post-Training does not contain 0, while the one for FRS pre-training does. 

## odds ratios are easier to interpret than coefficients. 
odd_ratio <- exp(coef(FRS_card_OrdinalLogReg))
ctable_card <- cbind(ctable_card, "odd ratio" = odd_ratio)
# Interpretation: For participants in the belt condition, the likelihood of rating themselves higher on the cardinal FRS scale is 1.083 times compared to the controls,  holding all other variables constant.
# Post-training the odds of rating themselves higher on the cardinal scale were 6.242 times that of the control condition, holding all other variables constant.

### AFTER NEW INSPECTION OF DATA: Main effect for the cardinal FRS scale is smaller than for the allocentric FRS scale because the Std. Error is highly increased (>*2).


####################################################

# Analysis for comparison

##changing order
frs_full_long_a <- frs_full_long
frs_full_long_a$FRS <- as.factor(frs_full_long_a$FRS)

frs_full_long_a$FRS <- recode_factor(frs_full_long_a$FRS, "Pre-Training" = "First", 
                                     "Post-Training" = "Second", "Control group" = "Third")


#egocentric
frs_full_long_ego_a <- frs_full_long_a[frs_full_long_a$Subscale=="Egocentric",]

FRS_ego_OrdinalLogReg_a<- polr(Answer ~ FRS, data = frs_full_long_ego_a, Hess = TRUE) 
sum_ego_a <- summary(FRS_ego_OrdinalLogReg_a)

frs_full_long_ego_a <- df_comparison[df_comparison$Scale=="Egocentric",]
wilcox.test(Answer ~ FRS, data=frs_full_long_ego_a, paired = TRUE)
wilcox_effsize(Answer ~ FRS, data=frs_full_long_ego_a, paired = TRUE)

OIM_ego_a <- polr(as.factor(Answer) ~ FRS=="First", data = frs_full_long_ego_a, Hess = TRUE)
summary(OIM_ego_a)

# For p-values we create a table of the model coefficients:
(ctable_ego_a <- coef(summary(FRS_ego_OrdinalLogReg_a)))
# Calculate and store the p-values, and combine them with our table:
p <- pnorm(abs(ctable_ego_a[, "t value"]), lower.tail = FALSE) * 2
ctable_ego_a <- cbind(ctable_ego_a, "p value" = p)
ctable_ego_a

(ci_ego_a <- confint(FRS_ego_OrdinalLogReg_a)) # default is profiled CI
confint.default(FRS_ego_OrdinalLogReg_a) # CI assuming normality

## odds ratios are easier to interpret than coefficients. 
odd_ratio <- exp(coef(FRS_ego_OrdinalLogReg_a))
ctable_ego_a <- cbind(ctable_ego_a, "odd ratio" = odd_ratio)


#allocentric
frs_full_long_allo_a <- frs_full_long_a[frs_full_long_a$Subscale=="Allocentric",]
FRS_allo_OrdinalLogReg_a <- polr(Answer ~ FRS, data = frs_full_long_allo_a, Hess = TRUE) 
sum_allo_a <- summary(FRS_allo_OrdinalLogReg_a)

frs_full_long_allo_a <- df_comparison[df_comparison$Scale=="Allocentric",]
wilcox.test(Answer ~ FRS, data=frs_full_long_allo_a, paired = TRUE)
wilcox_effsize(Answer ~ FRS, data=frs_full_long_allo_a, paired = TRUE)


# For p-values we create a table of the model coefficients:
(ctable_allo_a <- coef(summary(FRS_allo_OrdinalLogReg_a)))
# Calculate and store the p-values, and combine them with our table:
p <- pnorm(abs(ctable_allo_a[, "t value"]), lower.tail = FALSE) * 2
ctable_allo_a <- cbind(ctable_allo_a, "p value" = p)
ctable_allo_a

(ci_allo_a <- confint(FRS_allo_OrdinalLogReg_a)) # default is profiled CI
confint.default(FRS_allo_OrdinalLogReg_a) # CI assuming normality

## odds ratios are easier to interpret than coefficients. 
odd_ratio <- exp(coef(FRS_allo_OrdinalLogReg_a))
ctable_allo_a <- cbind(ctable_allo_a, "odd ratio" = odd_ratio)


#cardinal
frs_full_long_card_a <- frs_full_long_a[frs_full_long_a$Subscale=="Cardinal",]
FRS_card_OrdinalLogReg_a <- polr(Answer ~ FRS, data = frs_full_long_card_a, Hess = TRUE) 
sum_card_a <- summary(FRS_card_OrdinalLogReg_a)

frs_full_long_card_a <- df_comparison[df_comparison$Scale=="Cardinal",]
wilcox.test(Answer ~ FRS, data=frs_full_long_card_a, paired = TRUE)
wilcox_effsize(Answer ~ FRS, data=frs_full_long_card_a, paired = TRUE)

# For p-values we create a table of the model coefficients:
(ctable_card_a <- coef(summary(FRS_card_OrdinalLogReg_a)))
# Calculate and store the p-values, and combine them with our table:
p <- pnorm(abs(ctable_card_a[, "t value"]), lower.tail = FALSE) * 2
ctable_card_a <- cbind(ctable_card_a, "p value" = p)
ctable_card_a

(ci_card_a <- confint(FRS_card_OrdinalLogReg_a)) # default is profiled CI
confint.default(FRS_card_OrdinalLogReg_a) # CI assuming normality

## odds ratios are easier to interpret than coefficients. 
odd_ratio <- exp(coef(FRS_card_OrdinalLogReg_a))
ctable_card_a <- cbind(ctable_card_a, "odd ratio" = odd_ratio)

###################################################
