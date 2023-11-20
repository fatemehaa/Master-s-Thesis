# loading in packages
library("dplyr")
library("tidyverse")

#read in data
df_GHP_Ctrl <- read.csv('3. Angle Calculation/Resulting Dataframes/df_GHP_Ctrl_Preprocessed.csv') 
df_GHP_Exp <- read.csv('3. Angle Calculation/Resulting Dataframes/df_GHP_Exp_Preprocessed.csv')

# combine dataframes
ghp <- rbind(df_GHP_Ctrl, df_GHP_Exp)

ghp$Condition <- as.factor(ghp$Condition)
typeof(ghp$Condition)

# # Descriptives for signed angles
ghp_summary_direction_signed <- group_by(ghp, Condition) %>%
  summarise(
    count = n(),
    mean = mean(SignedAngle, na.rm = TRUE),
    SD = sd(SignedAngle, na.rm = TRUE),
    median = median(SignedAngle, na.rm = TRUE),
    IQR = IQR(SignedAngle, na.rm = TRUE)
  )

# Descriptives for absolute angles
ghp_summary_direction_absolute <- group_by(ghp, Condition) %>%
  summarise(
    count = n(),
    mean = mean(RecalculatedAngle, na.rm = TRUE),
    SD = sd(RecalculatedAngle, na.rm = TRUE),
    median = median(RecalculatedAngle, na.rm = TRUE),
    IQR = IQR(RecalculatedAngle, na.rm = TRUE)
  )

## Statistical tests for PLACEMENT DIRECTION signed angles (syst. differences)
#options(scipen=999)
#wilcox.test(ghp$SignedAngle~ghp$Condition)
#res_dir_syst <- wilcox.test(df_GHP_Ctrl$SignedAngle, df_GHP_Exp$SignedAngle)
#res_dir_syst

# Statistical tests for PLACEMENT DIRECTION absolutes angles (main result)

ghp.direction.test <- ghp  %>%
  rstatix::wilcox_test(RecalculatedAngle~Condition, paired = FALSE) %>%
  add_significance()
ghp.direction.test

# Efffect size PLACEMENT DIRECTION & median
ghp  %>%
  wilcox_effsize(RecalculatedAngle~Condition, paired = FALSE)
ghp_PD_ctrl <- ghp[ghp$Condition=="Control",]
ghp_PD_exp <- ghp[ghp$Condition=="Belt",]
median(ghp_PD_ctrl$RecalculatedAngle)
median(ghp_PD_exp$RecalculatedAngle)

# Statistical test for DISTANCE from the placement location (where the building was placed by the participant) to the correct target location.
# Summary
ghp_summary_distance <- group_by(ghp, Condition) %>%
  summarise(
    count = n(),
    mean = mean(GhostHouseDistanceToTargetHouse, na.rm = TRUE),
    SD = sd(GhostHouseDistanceToTargetHouse, na.rm = TRUE),
    median = median(GhostHouseDistanceToTargetHouse, na.rm = TRUE),
    IQR = IQR(GhostHouseDistanceToTargetHouse, na.rm = TRUE)
  )

## Standardize (just experimenting a bit here)
#ghp$dist_stand <- scale(ghp$GhostHouseDistanceToTargetHouse)
#t.test(dist_stand ~ Condition, data = ghp, var.equal = TRUE, alternative="less")

# MAIN ANALYSIS: T-test
# Assumption 1: Normality
with(ghp, shapiro.test(GhostHouseDistanceToTargetHouse[Condition == "Belt"]))# p < .001
with(ghp, shapiro.test(GhostHouseDistanceToTargetHouse[Condition == "Control"])) # p < .001
# Normality Assumption is violated! We will continue with the non-parametric alternative

#For normally distributed data, use this code:
#res_dist_Ttest <- t.test(GhostHouseDistanceToTargetHouse ~ Condition, data = ghp, var.equal = TRUE, alternative="less")
#res_dist_Ttest 

# Nonparametric equivalent (WRS)
ghp.distance.test <- ghp  %>%
  rstatix::wilcox_test(GhostHouseDistanceToTargetHouse~Condition, paired = FALSE) %>%
  add_significance()
ghp.distance.test

# Effect size DISTANCE & median
ghp  %>%
  wilcox_effsize(GhostHouseDistanceToTargetHouse~Condition, paired = FALSE)
median(ghp_PD_ctrl$GhostHouseDistanceToTargetHouse)
median(ghp_PD_exp$GhostHouseDistanceToTargetHouse)


# Statistical test for the global BUILDING ROTATION (how the building was rotated by the participant) as opposed to the buildings' global rotation in the city (the actual rotation of the building). 
# First, we will convert the angles to range from minimum -180 to maximum +180. Therefore, angles <-180 or >180 need to be converted by adding 360, or subtracting 360 respectively. 
RotDiffList <- list()
for (e in seq(length(ghp$GhostHouseRotDifferenceToTargetHouse))) {
  RotDifference_corrected <- ghp$GhostHouseRotDifferenceToTargetHouse[e]
  if (RotDifference_corrected > 180) {
    RotDifference_corrected <- RotDifference_corrected - 360
    RotDiffList = c(RotDiffList, list(RotDifference_corrected))
  } else if (RotDifference_corrected < -180) {
    RotDifference_corrected <- RotDifference_corrected + 360
    RotDiffList = c(RotDiffList, list(RotDifference_corrected))
  } else {
    RotDiffList = c(RotDiffList, list(RotDifference_corrected))
  }
}
ghp$GHP_RotDiff_signed <- unlist(RotDiffList)
ghp$GHP_RotDiff <- abs(unlist(RotDiffList))

# Summaries (signed to eclude syst. differences)
ghp_summary_rotation_signed <- group_by(ghp, Condition) %>%
  summarise(
    count = n(),
    mean = mean(GHP_RotDiff_signed, na.rm = TRUE),
    SD = sd(GHP_RotDiff_signed, na.rm = TRUE),
    median = median(GHP_RotDiff_signed, na.rm = TRUE),
    IQR = IQR(GHP_RotDiff_signed, na.rm = TRUE)
  )
ghp_summary_rotation_absolute <- group_by(ghp, Condition) %>%
  summarise(
    count = n(),
    mean = mean(GHP_RotDiff, na.rm = TRUE),
    SD = sd(GHP_RotDiff, na.rm = TRUE),
    median = median(GHP_RotDiff, na.rm = TRUE),
    IQR = IQR(GHP_RotDiff, na.rm = TRUE)
  )

 
# WRS Test to calculate the statistical effect (with signed angles to exclude systematic differences and) with the absolute values to evaluate the performance difference between the two conditions.
res_rot_syst <- wilcox.test(ghp$GHP_RotDiff_signed~ghp$Condition)
res_rot_syst
# Main Result:
ghp.rotation.test <- ghp  %>%
  rstatix::wilcox_test(GHP_RotDiff~Condition, paired = FALSE) %>%
  add_significance()
ghp.rotation.test

ghp  %>%
  wilcox_effsize(GHP_RotDiff~Condition, paired = FALSE)
median(ghp_PD_ctrl$GHP_RotDiff)
median(ghp_PD_exp$GHP_RotDiff)

#write.csv(ghp, "GHP_CompleteDF.csv")

