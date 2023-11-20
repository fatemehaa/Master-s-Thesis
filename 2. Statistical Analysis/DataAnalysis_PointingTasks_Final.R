library("dplyr")
library("coin")
library("rstatix")
library("dplyr")


#read in data
df_PTN_Ctrl <- read.csv('3. Angle Calculation/Resulting Dataframes/df_PTN_Ctrl_Preprocessed.csv') 
df_PTN_Exp <- read.csv('3. Angle Calculation/Resulting Dataframes/df_PTN_Exp_Preprocessed.csv')
df_PTB_Ctrl <- read.csv('3. Angle Calculation/Resulting Dataframes/df_PTB_Ctrl_Preprocessed.csv') 
df_PTB_Exp <- read.csv('3. Angle Calculation/Resulting Dataframes/df_PTB_Exp_Preprocessed.csv') 


df_PTB_Ctrl$Condition <- "Control Group"
df_PTN_Ctrl$Condition <- "Control Group"
df_PTB_Exp$Condition <- "Belt Group"
df_PTN_Exp$Condition <- "Belt Group"

# Renaming the recalculated angle columns:
df_PTN_Ctrl <- df_PTN_Ctrl %>% rename(rounded_UnityAngle = rounded_ctrl_Angle, rounded_recalcAngle = rounded_ctrl_recalcAngle)

df_PTN_Exp <- df_PTN_Exp %>% rename(rounded_UnityAngle = rounded_exp_Angle,rounded_recalcAngle = rounded_exp_recalcAngle)

# Combining the dataframes by task
ptb <- rbind(df_PTB_Ctrl, df_PTB_Exp)
ptn <- rbind(df_PTN_Ctrl, df_PTN_Exp)


ptb$Condition <- as.factor(ptb$Condition)
ptn$Condition <- as.factor(ptn$Condition)

# Descriptives
ptb_summary <- group_by(ptb, Condition) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(RecalculatedAngle, na.rm = TRUE),
    SD = sd(RecalculatedAngle, na.rm = TRUE),
    median = median(RecalculatedAngle, na.rm = TRUE),
    IQR = IQR(RecalculatedAngle, na.rm = TRUE)
  )

ptn_summary <- group_by(ptn, Condition) %>%
  dplyr::summarise(
    count = n(),
    mean = mean(RecalculatedAngle, na.rm = TRUE),
    SD = sd(RecalculatedAngle, na.rm = TRUE),
    median = median(RecalculatedAngle, na.rm = TRUE),
    IQR = IQR(RecalculatedAngle, na.rm = TRUE)
  )

# To display the complete actual number:
options(scipen=999)

### Wiloxon for independent samples

ptb.test <- ptb  %>%
  rstatix::wilcox_test(RecalculatedAngle~Condition, paired = FALSE) %>%
  add_significance()
ptb.test

ptn.test <- ptn  %>%
  rstatix::wilcox_test(RecalculatedAngle~Condition, paired = FALSE) %>%
  add_significance()
ptn.test

### Effect sizes

ptb  %>%
  wilcox_effsize(RecalculatedAngle~Condition, paired = FALSE)
# An effect size of 0.0736 is very small.

ptn  %>%
  wilcox_effsize(RecalculatedAngle~Condition, paired = FALSE)
# There is a larger effect with an effect size of exactly 0.5!

# Systematic differences: 
ptb.test <- ptb  %>%
  rstatix::wilcox_test(SignedAngle~Condition, paired = FALSE) %>%
  add_significance()
ptb.test

ptn.test <- ptn  %>%
  rstatix::wilcox_test(SignedAngle~Condition, paired = FALSE) %>%
  add_significance()
ptn.test



