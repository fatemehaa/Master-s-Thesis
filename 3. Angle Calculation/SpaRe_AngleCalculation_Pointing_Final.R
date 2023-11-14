# Use the unnesting script in python to flatten the dataframe first, then continue here! Make sure not to remove any data before (i.e. timeouts should still be in the df and will be removed later)

library('dplyr')
library('REdaS')
library('stringr')

df_PTN_Ctrl <- read.csv('2. Unnesting/Resulting Dataframes/unnested_PTN_control.csv') 
df_PTN_Exp <- read.csv('2. Unnesting/Resulting Dataframes/unnested_PTN_belt.csv')
df_PTB_Ctrl <- read.csv('2. Unnesting/Resulting Dataframes/unnested_PTB_control.csv') 
df_PTB_Exp <- read.csv('2. Unnesting/Resulting Dataframes/unnested_PTB_belt.csv') 

# df_PTN_Exp has 3025 instead of 3024 observations --> Why?
table(df_PTN_Exp$SubjectID)
# 1060 had 113 instead of 112 observations, let's investigate this further:
P1060 <- df_PTN_Exp$SubjectID == 1060
df_1060 <- filter(df_PTN_Exp, P1060)
# We can see that trial 35 in the repetition of the PTN task is duplicated with the same values. The index of the duplicated entry is X=1547, let's remove it and update the count (X).
# Keep in mind that after the count (X) has been updated, it now starts with a 1 not a 0 as it would be in Python. Re-run the code above to see whether the index has been changed and the duplicate has been removed.
df_PTN_Exp <- df_PTN_Exp[!(df_PTN_Exp$X==1547),]
df_PTN_Exp$X <- seq(length(df_PTN_Exp$SubjectID))



############################### Timeouts ##############################

# Participant 1011 timed out in all trials of PTB and 1/2 of the PTN trials. As the participant selected answers but forgot to confirm these, we can recalulate the RTs and the angles for the timed out trials, before removing the remaining timeouts.

# Inspecting the Subject 1011 that timed out during all trials of the first block
S1011_PTN <- filter(df_PTN_Ctrl, df_PTN_Ctrl$SubjectID==1011)
S1011_PTB <- filter(df_PTB_Ctrl, df_PTB_Ctrl$SubjectID==1011)

# Filtering out all timed out trials
TimedOut_PTN <- filter(df_PTN_Ctrl, df_PTN_Ctrl$TimeOut=="True")
TimedOut_PTN$TimeOut <- "False"
TimedOut_PTN$TimeOutStr <- "False"
TimedOut_PTB <- filter(df_PTB_Ctrl, df_PTB_Ctrl$TimeOut=="True")
TimedOut_PTB$TimeOut <- "False"
TimedOut_PTB$TimeOutStr <- "False"

TimedOut_PTN$TriggerPressedTimeStamps <- gsub("\\[|\\]", "", TimedOut_PTN$TriggerPressedTimeStamps)
TimedOut_PTN$TriggerPressedTimeStamps <- str_replace_all(TimedOut_PTN$TriggerPressedTimeStamps, " ", "")
TimedOut_PTN$TriggerPressedTimeStamps <- strsplit(TimedOut_PTN$TriggerPressedTimeStamps, ",")

TimedOut_PTB$TriggerPressedTimeStamps <- gsub("\\[|\\]", "", TimedOut_PTB$TriggerPressedTimeStamps)
TimedOut_PTB$TriggerPressedTimeStamps <- str_replace_all(TimedOut_PTB$TriggerPressedTimeStamps, " ", "")
TimedOut_PTB$TriggerPressedTimeStamps <- strsplit(TimedOut_PTB$TriggerPressedTimeStamps, ",")

# adjusting the ending time of the respective trial with the last entry of TriggerPressedTimeStamps

options(digits = 22)
for (i in seq(length(TimedOut_PTN$TriggerPressedTimeStamps))) {
  TimedOut_PTN$TimeStampEnd[i] <- TimedOut_PTN$TriggerPressedTimeStamps[[i]][length(TimedOut_PTN$TriggerPressedTimeStamps[[i]])]
  # Due to my system time being GMT + 02, 2 hours (=7200 seconds) have been falsely added onto the recalculated unix timestamps.
  # Here, I correct for this:
  TimedOut_PTN$TimeStampEnd[i] <- as.numeric(as.character(TimedOut_PTN$TimeStampEnd[i])) - 7200
}
TimedOut_PTN$TimeStampEnd <- as.numeric(TimedOut_PTN$TimeStampEnd)

options(digits = 22)
for (i in seq(length(TimedOut_PTB$TriggerPressedTimeStamps))) {
  TimedOut_PTB$TimeStampEnd[i] <- TimedOut_PTB$TriggerPressedTimeStamps[[i]][length(TimedOut_PTB$TriggerPressedTimeStamps[[i]])]
  # Due to my system time being GMT + 02, 2 hours (=7200 seconds) have been falsely added onto the recalculated unix timestamps.
  # Here, I correct for this:
  TimedOut_PTB$TimeStampEnd[i] <- as.numeric(as.character(TimedOut_PTB$TimeStampEnd[i])) - 7200
}
TimedOut_PTB$TimeStampEnd <- as.numeric(TimedOut_PTB$TimeStampEnd)


# Reformating the beginning timestamps (can only executed once and will throw an error afterwards)
TimedOut_PTN$TimeStampBegin <- as.numeric(as.POSIXct(TimedOut_PTN$TimeStampBegin, format="%Y-%m-%d %H:%M:%OS"))
TimedOut_PTN$TrialDuration <- as.numeric(TimedOut_PTN$TimeStampEnd) - as.numeric(TimedOut_PTN$TimeStampBegin)

TimedOut_PTB$TimeStampBegin <- as.numeric(as.POSIXct(TimedOut_PTB$TimeStampBegin, format="%Y-%m-%d %H:%M:%OS"))
TimedOut_PTB$TrialDuration <- as.numeric(TimedOut_PTB$TimeStampEnd) - as.numeric(TimedOut_PTB$TimeStampBegin)

# Adding them to the dataframe
# 1. again filter out timed out trials (all Subject 1011)
df_PTN_Ctrl_TimedOut <- filter(df_PTN_Ctrl, TrialDuration >= 30)
df_PTN_Ctrl_NOTimeOut <- filter(df_PTN_Ctrl, TrialDuration <= 30)

df_PTB_Ctrl_TimedOut <- filter(df_PTB_Ctrl, TrialDuration >= 30)
df_PTB_Ctrl_NOTimeOut <- filter(df_PTB_Ctrl, TrialDuration <= 30)


# Adding TimedOut dataframe back to our df_PTN_RecalcAngles Dataframe! WATCH OUT TO ONLY RUN ONCE!
df_PTN_Ctrl <- rbind(df_PTN_Ctrl_NOTimeOut, TimedOut_PTN)
df_PTB_Ctrl <- rbind(df_PTB_Ctrl_NOTimeOut, TimedOut_PTB)

# Checking the result - if df_PTN_Ctrl_NoTimeout_1011 is empty, the timed out trials have been recalculated succesfully.
df_PTN_Ctrl_NoTimeout_1011 <- filter(df_PTN_Ctrl, df_PTN_Ctrl$TimeOutStr == "True")
df_PTB_Ctrl_NoTimeout_1011 <- filter(df_PTB_Ctrl, df_PTB_Ctrl$TimeOutStr == "True")

# Resorting
df_PTN_Ctrl <- df_PTN_Ctrl[order(df_PTN_Ctrl$X),]
df_PTB_Ctrl <- df_PTB_Ctrl[order(df_PTB_Ctrl$X),]

### Cleaning the Timeouts from the Belt condition
TimedOut_PTN_belt <- filter(df_PTN_Exp, df_PTN_Exp$TimeOut=="True")
TimedOut_PTB_belt <- filter(df_PTB_Exp, df_PTB_Exp$TimeOut=="True")

df_PTN_Exp <- df_PTN_Exp[df_PTN_Exp$TimeOut!="True",]
df_PTB_Exp <- df_PTB_Exp[df_PTB_Exp$TimeOut!="True",]


############################# Angle Recalculation PTN ################################



### For-loop creating a list of vectors with y-axis removed (projecting onto the xz-pane)
vectorlist_ctrl_PTN <- list()
for (i in seq(length(df_PTN_Ctrl$PointerDirection_x))) {
  vector_ctrl_PTN <- c(df_PTN_Ctrl$PointerDirection_x[i], df_PTN_Ctrl$PointerDirection_z[i])
  vectorlist_ctrl_PTN = c(vectorlist_ctrl_PTN, list(vector_ctrl_PTN))
  i + 1
}
vectorlist_exp_PTN <- list()
for (i in seq(length(df_PTN_Exp$PointerDirection_x))) {
  vector_exp_PTN <- c(df_PTN_Exp$PointerDirection_x[i], df_PTN_Exp$PointerDirection_z[i])
  vectorlist_exp_PTN = c(vectorlist_exp_PTN, list(vector_exp_PTN))
  i + 1
}


# Defining the North direction (precise to 12 digits after the comma)
North <- c(0.766044443119, -0.642787609687)

# Creating functions for Magnitude, normalizing and dot product
Magnitude <- function(v) {return(sqrt(sum(v ^ 2)))}
Normalize <- function(v){v/ sqrt(sum(v ^ 2)) }
dot <- function(v1,v2){
  return(sum(v1*v2))
}

# Creating function that calculates the angle between two vectors v1 and v2
AngleBetween <- function(v1,v2){
  v1Norm <- Normalize(v1)
  v2Norm <- Normalize(v2)
  dot_product <- dot(v1Norm , v2Norm)
  cross = crossprod(v1,v2)
  angle <- acos(dot_product)
  return(rad2deg(angle))
}

# Creating function that calculates the angle between two vectors v1 and v2
SignedAngleBetween2d <- function(v1,v2){
  v1Norm <- Normalize(v1)
  v2Norm <- Normalize(v2)
  
  angle <- atan2(v2[2],v2[1]) - atan2(v1[2],v1[1])
  return(rad2deg(angle))
}


# This is reproducing the results from "Angle", BUT without +/- sign
anglelist_ctrl_PTN <- list()
for (e in seq(length(vectorlist_ctrl_PTN))) {
  angle_ctrl_PTN <- AngleBetween(unlist(vectorlist_ctrl_PTN[e]),North)
  anglelist_ctrl_PTN = c(anglelist_ctrl_PTN, list(angle_ctrl_PTN))
  e + 1
}
anglelist_exp_PTN <- list()
for (e in seq(length(vectorlist_exp_PTN))) {
  angle_exp_PTN <- AngleBetween(unlist(vectorlist_exp_PTN[e]),North)
  anglelist_exp_PTN = c(anglelist_exp_PTN, list(angle_exp_PTN))
  e + 1
}

# This is reproducing the results from "Angle" with +/- sign
signedAnglelist_ctrl_PTN <- list()
for (e in seq(length(vectorlist_ctrl_PTN))) {
  signedAngle_ctrl_PTN <- SignedAngleBetween2d(unlist(vectorlist_ctrl_PTN[e]),North)
  if (signedAngle_ctrl_PTN > 180) {
    signedAngle_ctrl_PTN <- signedAngle_ctrl_PTN - 360
    signedAnglelist_ctrl_PTN = c(signedAnglelist_ctrl_PTN, list(signedAngle_ctrl_PTN))
  } else if (signedAngle_ctrl_PTN < -180) {
    signedAngle_ctrl_PTN <- signedAngle_ctrl_PTN + 360
    signedAnglelist_ctrl_PTN = c(signedAnglelist_ctrl_PTN, list(signedAngle_ctrl_PTN))
  } else {
    signedAnglelist_ctrl_PTN = c(signedAnglelist_ctrl_PTN, list(signedAngle_ctrl_PTN))
  }
}


signedAnglelist_exp_PTN <- list()
for (e in seq(length(vectorlist_exp_PTN))) {
  signedAngle_exp_PTN <- SignedAngleBetween2d(unlist(vectorlist_exp_PTN[e]),North)
  if (signedAngle_exp_PTN > 180) {
    signedAngle_exp_PTN <- signedAngle_exp_PTN - 360
    signedAnglelist_exp_PTN = c(signedAnglelist_exp_PTN, list(signedAngle_exp_PTN))
  } else if (signedAngle_exp_PTN < -180) {
    signedAngle_exp_PTN <- signedAngle_exp_PTN + 360
    signedAnglelist_exp_PTN = c(signedAnglelist_exp_PTN, list(signedAngle_exp_PTN))
  } else {
    signedAnglelist_exp_PTN = c(signedAnglelist_exp_PTN, list(signedAngle_exp_PTN))
  }
}



# Add them to the Dataframe
df_PTN_Ctrl$RecalculatedAngle <- unlist(anglelist_ctrl_PTN)
df_PTN_Exp$RecalculatedAngle <- unlist(anglelist_exp_PTN)

df_PTN_Ctrl$SignedAngle <- unlist(signedAnglelist_ctrl_PTN)
df_PTN_Exp$SignedAngle <- unlist(signedAnglelist_exp_PTN)

### for comparison you can also add the unity angles (again)
df_PTN_Ctrl$UnityAngle <- df_PTN_Ctrl$Angle
df_PTN_Exp$UnityAngle <- df_PTN_Exp$Angle

### Testing whether UnityAngle equals Recalculated Angle
df_PTN_Ctrl$rounded_ctrl_Angle <- round(df_PTN_Ctrl$UnityAngle, 3)
df_PTN_Exp$rounded_exp_Angle <- round(df_PTN_Exp$UnityAngle, 3)

df_PTN_Ctrl$rounded_ctrl_recalcAngle <- round(df_PTN_Ctrl$RecalculatedAngle, 3)
df_PTN_Exp$rounded_exp_recalcAngle <- round(df_PTN_Exp$RecalculatedAngle, 3)

# To test this, we filter out and inspect the data where the rounded recalulated angle does not match the angle given by Unity (Unity angle)
unequal_cases_ctrl <- df_PTN_Ctrl$rounded_ctrl_Angle != df_PTN_Ctrl$rounded_ctrl_recalcAngle
table(unequal_cases_ctrl)
Unequal_RoundedAngles_Ctrl <- filter(df_PTN_Ctrl, unequal_cases_ctrl) 

unequal_cases_exp <- df_PTN_Exp$rounded_exp_Angle != df_PTN_Exp$rounded_exp_recalcAngle
table(unequal_cases_exp)
Unequal_RoundedAngles_Exp <- filter(df_PTN_Exp, unequal_cases_exp) 


# descriptives for PTN
min(df_PTN_Ctrl$RecalculatedAngle)
min(df_PTN_Exp$RecalculatedAngle)

# Saving the resulting dataframes
typeof(df_PTN_Ctrl$TriggerPressedTimeStamps)
for (e in seq(length(df_PTN_Ctrl$TriggerPressedTimeStamps))) {
  df_PTN_Ctrl$TriggerPressedTimeStamps[e] <- toString(df_PTN_Ctrl$TriggerPressedTimeStamps[e])
}
typeof(df_PTN_Exp$TriggerPressedTimeStamps)
for (e in seq(length(df_PTN_Exp$TriggerPressedTimeStamps))) {
  df_PTN_Exp$TriggerPressedTimeStamps[e] <- toString(df_PTN_Exp$TriggerPressedTimeStamps[e])
}

df_PTN_Ctrl$TriggerPressedTimeStamps <- unlist(df_PTN_Ctrl$TriggerPressedTimeStamps, use.names = FALSE)
df_PTN_Exp$TriggerPressedTimeStamps <- unlist(df_PTN_Exp$TriggerPressedTimeStamps, use.names = FALSE)


 
write.csv(df_PTN_Ctrl, "df_PTN_Ctrl_Preprocessed.csv")
write.csv(df_PTN_Exp, "df_PTN_Exp_Preprocessed.csv")





########################## POINTING TO A BUILDING ##############################



# Loading in and checking the correct starting locations, exported from Unity
BuildingLocations <- read.csv("BuildingCoordinates.csv")

# checking all 8 target building locations 
Target_df <- unique(df_PTB_Exp[,c("ImageName", "StartingPositionIndex", "TargetBuildingPosition_x", "TargetBuildingPosition_y", "TargetBuildingPosition_z")])

# This looks good and consistent, but let's compare the target building locations in detail
target1 <- Target_df[Target_df$ImageName=="TaskBuilding_1",3:5][1,] == BuildingLocations[BuildingLocations$BuildingName=="TaskBuilding_1",5:7][1,]
target2 <- Target_df[Target_df$ImageName=="TaskBuilding_2",3:5][1,] == BuildingLocations[BuildingLocations$BuildingName=="TaskBuilding_2",5:7][1,]
target3 <- Target_df[Target_df$ImageName=="TaskBuilding_3",3:5][1,] == BuildingLocations[BuildingLocations$BuildingName=="TaskBuilding_3",5:7][1,]
target4 <- Target_df[Target_df$ImageName=="TaskBuilding_4",3:5][1,] == BuildingLocations[BuildingLocations$BuildingName=="TaskBuilding_4",5:7][1,]
target5 <- Target_df[Target_df$ImageName=="TaskBuilding_5",3:5][1,] == BuildingLocations[BuildingLocations$BuildingName=="TaskBuilding_5",5:7][1,]
target6 <- Target_df[Target_df$ImageName=="TaskBuilding_6",3:5][1,] == BuildingLocations[BuildingLocations$BuildingName=="TaskBuilding_6",5:7][1,]
target7 <- Target_df[Target_df$ImageName=="TaskBuilding_7",3:5][1,] == BuildingLocations[BuildingLocations$BuildingName=="TaskBuilding_7",5:7][1,]
target8 <- Target_df[Target_df$ImageName=="TaskBuilding_8",3:5][1,] == BuildingLocations[BuildingLocations$BuildingName=="TaskBuilding_8",5:7][1,]
# The target building locations are correct. The starting location positions were logged in a separate file (i.e. for distance analysis), as we used the Pointer Position instead for the calculation as the pointing was performed with the controller.




### Recaluclating the angles ###
# For-loop creating a list of vectors with y-axis removed (projecting onto the xz-pane) for PointerDirection
vectorlist_belt_PTB <- list()
for (i in seq(length(df_PTB_Exp$PointerDirection_x))) {
  vector_belt_PTB <- c(df_PTB_Exp$PointerDirection_x[i], df_PTB_Exp$PointerDirection_z[i])
  vectorlist_belt_PTB = c(vectorlist_belt_PTB, list(vector_belt_PTB))
  i + 1
}

vectorlist_control_PTB <- list()
for (i in seq(length(df_PTB_Ctrl$PointerDirection_x))) {
  vector_control_PTB <- c(df_PTB_Ctrl$PointerDirection_x[i], df_PTB_Ctrl$PointerDirection_z[i])
  vectorlist_control_PTB = c(vectorlist_control_PTB, list(vector_control_PTB))
  i + 1
}

# For-loop creating a list of vectors with y-axis removed (projecting onto the xz-pane) for Participant Position
vectorlist_PartPos_belt_PTB  <- list()
for (i in seq(length(df_PTB_Exp$ParticipantPosition_x))) {
  vector_PartPos_belt_PTB <- c(df_PTB_Exp$ParticipantPosition_x[i], df_PTB_Exp$ParticipantPosition_z[i])
  vectorlist_PartPos_belt_PTB = c(vectorlist_PartPos_belt_PTB, list(vector_PartPos_belt_PTB))
  i + 1
}

vectorlist_PartPos_control_PTB  <- list()
for (i in seq(length(df_PTB_Ctrl$ParticipantPosition_x))) {
  vector_PartPos_control_PTB <- c(df_PTB_Ctrl$ParticipantPosition_x[i], df_PTB_Ctrl$ParticipantPosition_z[i])
  vectorlist_PartPos_control_PTB = c(vectorlist_PartPos_control_PTB, list(vector_PartPos_control_PTB))
  i + 1
}

# For-loop creating a list of vectors with y-axis removed (projecting onto the xz-pane) for Target Position
vectorlist_TargPos_belt_PTB  <- list()
for (i in seq(length(df_PTB_Exp$TargetBuildingPosition_x))) {
  vector_TargPos_belt_PTB <- c(df_PTB_Exp$TargetBuildingPosition_x[i], df_PTB_Exp$TargetBuildingPosition_z[i])
  vectorlist_TargPos_belt_PTB = c(vectorlist_TargPos_belt_PTB, list(vector_TargPos_belt_PTB))
  i + 1
}


vectorlist_TargPos_control_PTB  <- list()
for (i in seq(length(df_PTB_Ctrl$TargetBuildingPosition_x))) {
  vector_TargPos_control_PTB <- c(df_PTB_Ctrl$TargetBuildingPosition_x[i], df_PTB_Ctrl$TargetBuildingPosition_z[i])
  vectorlist_TargPos_control_PTB = c(vectorlist_TargPos_control_PTB, list(vector_TargPos_control_PTB))
  i + 1
}

### Calculating direction vector from Participant Position (a) to Target Building Position (b): b-a
TargetDiections_belt_PTB <- list()
for (e in seq(length(vectorlist_TargPos_belt_PTB))) {
  TargetDirection_vector_belt_PTB <- unlist(vectorlist_TargPos_belt_PTB[e]) - unlist(vectorlist_PartPos_belt_PTB[e])
  TargetDiections_belt_PTB = c(TargetDiections_belt_PTB, list(TargetDirection_vector_belt_PTB))
  e + 1
}

TargetDiections_control_PTB <- list()
for (e in seq(length(vectorlist_TargPos_control_PTB))) {
  TargetDirection_vector_control_PTB <- unlist(vectorlist_TargPos_control_PTB[e]) - unlist(vectorlist_PartPos_control_PTB[e])
  TargetDiections_control_PTB = c(TargetDiections_control_PTB, list(TargetDirection_vector_control_PTB))
  e + 1
}

######## CREATING REQUIRED FUNCTIONS FOR FINAL ANGLE RECALCULATION #########

# Creating functions for Magnitude, normalizing and dot product
Magnitude <- function(v) {return(sqrt(sum(v ^ 2)))}
Normalize <- function(v){v/ sqrt(sum(v ^ 2)) }
dot <- function(v1,v2){
  return(sum(v1*v2))
}


############ CALCULATING THE ANGLES ################
# Absolute Angles (Without +/- signs)

anglelist_belt_PTB <- list()
for (e in seq(length(vectorlist_belt_PTB))) {
  angle_belt_PTB <- AngleBetween(unlist(vectorlist_belt_PTB[e]),unlist(TargetDiections_belt_PTB[e]))
  anglelist_belt_PTB = c(anglelist_belt_PTB, list(angle_belt_PTB))
  e + 1
}

anglelist_control_PTB <- list()
for (e in seq(length(vectorlist_control_PTB))) {
  angle_control_PTB <- AngleBetween(unlist(vectorlist_control_PTB[e]),unlist(TargetDiections_control_PTB[e]))
  anglelist_control_PTB = c(anglelist_control_PTB, list(angle_control_PTB))
  e + 1
}

# Signed Angles
signedAnglelist_belt_PTB <- list()
for (e in seq(length(vectorlist_belt_PTB))) {
  signedAngle_belt_PTB <- SignedAngleBetween2d(unlist(vectorlist_belt_PTB[e]), unlist(TargetDiections_belt_PTB[e]))
  if (signedAngle_belt_PTB > 180) {
    signedAngle_belt_PTB <- signedAngle_belt_PTB - 360
    signedAnglelist_belt_PTB = c(signedAnglelist_belt_PTB, list(signedAngle_belt_PTB))
  } else if (signedAngle_belt_PTB < -180) {
    signedAngle_belt_PTB <- signedAngle_belt_PTB + 360
    signedAnglelist_belt_PTB = c(signedAnglelist_belt_PTB, list(signedAngle_belt_PTB))
  } else {
    signedAnglelist_belt_PTB = c(signedAnglelist_belt_PTB, list(signedAngle_belt_PTB))
  }
}

signedAnglelist_control_PTB <- list()
for (e in seq(length(vectorlist_control_PTB))) {
  signedAngle_control_PTB <- SignedAngleBetween2d(unlist(vectorlist_control_PTB[e]), unlist(TargetDiections_control_PTB[e]))
  if (signedAngle_control_PTB > 180) {
    signedAngle_control_PTB <- signedAngle_control_PTB - 360
    signedAnglelist_control_PTB = c(signedAnglelist_control_PTB, list(signedAngle_control_PTB))
  } else if (signedAngle_control_PTB < -180) {
    signedAngle_control_PTB <- signedAngle_control_PTB + 360
    signedAnglelist_control_PTB = c(signedAnglelist_control_PTB, list(signedAngle_control_PTB))
  } else {
    signedAnglelist_control_PTB = c(signedAnglelist_control_PTB, list(signedAngle_control_PTB))
  }
}




# Add them to the dataframe
df_PTB_Exp$RecalculatedAngle <- unlist(anglelist_belt_PTB)
df_PTB_Ctrl$RecalculatedAngle <- unlist(anglelist_control_PTB)

df_PTB_Exp$SignedAngle <-unlist(signedAnglelist_belt_PTB)
df_PTB_Ctrl$SignedAngle <- unlist(signedAnglelist_control_PTB)

## If necessary. or desired, the following code can be used to check the result.
#Anglecheck_PTN_Exp <- select(df_PTB_Exp, SubjectID, Angle, RecalculatedAngle, SignedAngle)
#Anglecheck_PTN_Ctrl <- select(df_PTB_Ctrl, SubjectID, Angle, RecalculatedAngle, SignedAngle)

#write.csv(Anglecheck_df_belt, "Anglecheck_belt_PtB")
#write.csv(Anglecheck_df_control, "Anglecheck_control_PtB")



# Data saving

typeof(df_PTB_Ctrl$TriggerPressedTimeStamps)
for (e in seq(length(df_PTB_Ctrl$TriggerPressedTimeStamps))) {
  df_PTB_Ctrl$TriggerPressedTimeStamps[e] <- toString(df_PTB_Ctrl$TriggerPressedTimeStamps[e])
}
df_PTB_Ctrl$TriggerPressedTimeStamps <- unlist(df_PTB_Ctrl$TriggerPressedTimeStamps, use.names = FALSE)


write.csv(df_PTB_Exp, "df_PTB_Exp_Preprocessed.csv")
write.csv(df_PTB_Ctrl, "df_PTB_Ctrl_Preprocessed.csv")





