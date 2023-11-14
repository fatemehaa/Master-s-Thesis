# Use the unnesting script in python to flatten the dataframe first, then continue here! Make sure not to remove any data before (i.e. timeouts should be left in the df and will be removed later)

library('dplyr')
library('REdaS')
library('stringr')

# Creating functions that we will use later to calculate the (signed) angles
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

# Reading in the data files
df_GHP_Ctrl <- read.csv('2. Unnesting/Resulting Dataframes/unnested_GHP_control.csv') 
df_GHP_Exp <- read.csv('2. Unnesting/Resulting Dataframes/unnested_GHP_belt.csv')

# checking whether the number of observations is the same for each subject
table(df_GHP_Exp$SubjectID)
table(df_GHP_Ctrl$SubjectID)


############################### Timeouts ##############################
# Inspecting the timed out trials.
TimedOut_GHP_Ctrl <- filter(df_GHP_Ctrl, df_GHP_Ctrl$TimeOut=="True")
TimedOut_GHP_Exp <- filter(df_GHP_Exp, df_GHP_Exp$TimeOut=="True")
# Although some participants pointed somewhere, they did not confirm their selection within the 90 seconds trial duration. All trials where participants did not repond or failed
# to log in their asnwer in time will be removed in the next step. This leaves a total of 728-4=724 trials in the control condition and 756-12=744 trials in the bel condition

# Filtering out all timed out trials
df_GHP_Ctrl <- filter(df_GHP_Ctrl, df_GHP_Ctrl$TimeOut=="False")
df_GHP_Exp <- filter(df_GHP_Exp, df_GHP_Exp$TimeOut=="False")

############################# Updating the building locations ################################

BuildingLocations_old <- read.csv("BuildingCoordinates_old.csv")

P1027 <- filter(df_GHP_Ctrl, df_GHP_Ctrl$SubjectID ==1005)
df1_P1027 <- data.frame(cbind(P1027$StartingPositionIndex, P1027$GhostHouseName, P1027$TargetHousePos_x, P1027$TargetHousePos_y, P1027$TargetHousePos_z))
write.csv(df1_P1027, "df_P1027_TargetBuildingLocations.csv")
# After manually comparing the locations to the recalculated ones, we can see that most locations are correct. Some are slightly off and we can adjust them by updating all locations
# to the recalculated ones. The differences are so minor though that they should not affect the results.


################################# CHECKING BUILDING LOCATIONS ##############################################


# checking all 8 target building locations 
Target_df <- unique(df_GHP_Exp[,c("TargetHouseName", "StartingPositionIndex", "TargetHousePos_x", "TargetHousePos_y", "TargetHousePos_z")])

# This looks good and consistent with the updated location sheet, but let's compare the target building locations in detail
target1 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_1",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_1",5:7][1,]
target2 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_2",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_2",5:7][1,]
target3 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_3",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_3",5:7][1,]
target4 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_4",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_4",5:7][1,]
target6 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_6",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_6",5:7][1,]
target11 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_11",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_11",5:7][1,]
target12 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_12",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_12",5:7][1,]
target14 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_14",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_14",5:7][1,]
target17 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_17",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_17",5:7][1,]
target18 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_18",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_18",5:7][1,]
target21 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_21",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_21",5:7][1,]
target23 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_23",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_23",5:7][1,]
target26 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_26",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_26",5:7][1,]
target28 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_28",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_28",5:7][1,]
target29 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_29",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_29",5:7][1,]
target30 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_30",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_30",5:7][1,]
target32 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_32",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_32",5:7][1,]
target34 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_34",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_34",5:7][1,]
target35 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_35",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_35",5:7][1,]
target36 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_36",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_36",5:7][1,]
target39 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_39",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_39",5:7][1,]
target41 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_41",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_41",5:7][1,]
target42 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_42",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_42",5:7][1,]
target43 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_43",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_43",5:7][1,]
target45 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_45",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_45",5:7][1,]
target47 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_47",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_47",5:7][1,]
target53 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_53",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_53",5:7][1,]
target55 <- Target_df[Target_df$TargetHouseName=="TaskBuilding_55",3:5][1,] == BuildingLocations_old[BuildingLocations_old$BuildingName=="TaskBuilding_55",5:7][1,]
# The target building locations are correct. The starting location positions were logged in a separate file (i.e. for distance analysis), as we used the Pointer Position instead for the calculation as the pointing was performed with the controller.

##################################### HERE IT CONTINUES #################################################################################


### Recaluclating the angles ###
# For-loop creating a list of vectors with y-axis removed (projecting onto the xz-pane) for PointerDirection
vectorlist_belt_GHP <- list()
for (i in seq(length(df_GHP_Exp$PointerDirection_x))) {
  vector_belt_GHP <- c(df_GHP_Exp$PointerDirection_x[i], df_GHP_Exp$PointerDirection_z[i])
  vectorlist_belt_GHP = c(vectorlist_belt_GHP, list(vector_belt_GHP))
  i + 1
}

vectorlist_control_GHP <- list()
for (i in seq(length(df_GHP_Ctrl$PointerDirection_x))) {
  vector_control_GHP <- c(df_GHP_Ctrl$PointerDirection_x[i], df_GHP_Ctrl$PointerDirection_z[i])
  vectorlist_control_GHP = c(vectorlist_control_GHP, list(vector_control_GHP))
  i + 1
}

# For-loop creating a list of vectors with y-axis removed (projecting onto the xz-pane) for Participant Position
vectorlist_PartPos_belt_GHP  <- list()
for (i in seq(length(df_GHP_Exp$ParticipantPosition_x))) {
  vector_PartPos_belt_GHP <- c(df_GHP_Exp$ParticipantPosition_x[i], df_GHP_Exp$ParticipantPosition_z[i])
  vectorlist_PartPos_belt_GHP = c(vectorlist_PartPos_belt_GHP, list(vector_PartPos_belt_GHP))
  i + 1
}

vectorlist_PartPos_control_GHP  <- list()
for (i in seq(length(df_GHP_Ctrl$ParticipantPosition_x))) {
  vector_PartPos_control_GHP <- c(df_GHP_Ctrl$ParticipantPosition_x[i], df_GHP_Ctrl$ParticipantPosition_z[i])
  vectorlist_PartPos_control_GHP = c(vectorlist_PartPos_control_GHP, list(vector_PartPos_control_GHP))
  i + 1
}

# For-loop creating a list of vectors with y-axis removed (projecting onto the xz-pane) for Target Position
vectorlist_TargPos_belt_GHP  <- list()
for (i in seq(length(df_GHP_Exp$TargetHousePos_x))) {
  vector_TargPos_belt_GHP <- c(df_GHP_Exp$TargetHousePos_x[i], df_GHP_Exp$TargetHousePos_z[i])
  vectorlist_TargPos_belt_GHP = c(vectorlist_TargPos_belt_GHP, list(vector_TargPos_belt_GHP))
  i + 1
}


vectorlist_TargPos_control_GHP  <- list()
for (i in seq(length(df_GHP_Ctrl$TargetHousePos_x))) {
  vector_TargPos_control_GHP <- c(df_GHP_Ctrl$TargetHousePos_x[i], df_GHP_Ctrl$TargetHousePos_z[i])
  vectorlist_TargPos_control_GHP = c(vectorlist_TargPos_control_GHP, list(vector_TargPos_control_GHP))
  i + 1
}

### Calculating direction vector from Participant Position (a) to Target Building Position (b): b-a
TargetDiections_belt_GHP <- list()
for (e in seq(length(vectorlist_TargPos_belt_GHP))) {
  TargetDirection_vector_belt_GHP <- unlist(vectorlist_TargPos_belt_GHP[e]) - unlist(vectorlist_PartPos_belt_GHP[e])
  TargetDiections_belt_GHP = c(TargetDiections_belt_GHP, list(TargetDirection_vector_belt_GHP))
  e + 1
}

TargetDiections_control_GHP <- list()
for (e in seq(length(vectorlist_TargPos_control_GHP))) {
  TargetDirection_vector_control_GHP <- unlist(vectorlist_TargPos_control_GHP[e]) - unlist(vectorlist_PartPos_control_GHP[e])
  TargetDiections_control_GHP = c(TargetDiections_control_GHP, list(TargetDirection_vector_control_GHP))
  e + 1
}


############ CALCULATING THE ANGLES ################
# Absolute Angles (Without +/- signs)

anglelist_belt_GHP <- list()
for (e in seq(length(vectorlist_belt_GHP))) {
  angle_belt_GHP <- AngleBetween(unlist(vectorlist_belt_GHP[e]),unlist(TargetDiections_belt_GHP[e]))
  anglelist_belt_GHP = c(anglelist_belt_GHP, list(angle_belt_GHP))
  e + 1
}

anglelist_control_GHP <- list()
for (e in seq(length(vectorlist_control_GHP))) {
  angle_control_GHP <- AngleBetween(unlist(vectorlist_control_GHP[e]),unlist(TargetDiections_control_GHP[e]))
  anglelist_control_GHP = c(anglelist_control_GHP, list(angle_control_GHP))
  e + 1
}

# Signed Angles
signedAnglelist_belt_GHP <- list()
for (e in seq(length(vectorlist_belt_GHP))) {
  signedAngle_belt_GHP <- SignedAngleBetween2d(unlist(vectorlist_belt_GHP[e]), unlist(TargetDiections_belt_GHP[e]))
  if (signedAngle_belt_GHP > 180) {
    signedAngle_belt_GHP <- signedAngle_belt_GHP - 360
    signedAnglelist_belt_GHP = c(signedAnglelist_belt_GHP, list(signedAngle_belt_GHP))
  } else if (signedAngle_belt_GHP < -180) {
    signedAngle_belt_GHP <- signedAngle_belt_GHP + 360
    signedAnglelist_belt_GHP = c(signedAnglelist_belt_GHP, list(signedAngle_belt_GHP))
  } else {
    signedAnglelist_belt_GHP = c(signedAnglelist_belt_GHP, list(signedAngle_belt_GHP))
  }
}

signedAnglelist_control_GHP <- list()
for (e in seq(length(vectorlist_control_GHP))) {
  signedAngle_control_GHP <- SignedAngleBetween2d(unlist(vectorlist_control_GHP[e]), unlist(TargetDiections_control_GHP[e]))
  if (signedAngle_control_GHP > 180) {
    signedAngle_control_GHP <- signedAngle_control_GHP - 360
    signedAnglelist_control_GHP = c(signedAnglelist_control_GHP, list(signedAngle_control_GHP))
  } else if (signedAngle_control_GHP < -180) {
    signedAngle_control_GHP <- signedAngle_control_GHP + 360
    signedAnglelist_control_GHP = c(signedAnglelist_control_GHP, list(signedAngle_control_GHP))
  } else {
    signedAnglelist_control_GHP = c(signedAnglelist_control_GHP, list(signedAngle_control_GHP))
  }
}




# Add them to the dataframe
df_GHP_Exp$RecalculatedAngle <- unlist(anglelist_belt_GHP)
df_GHP_Ctrl$RecalculatedAngle <- unlist(anglelist_control_GHP)

df_GHP_Exp$SignedAngle <-unlist(signedAnglelist_belt_GHP)
df_GHP_Ctrl$SignedAngle <- unlist(signedAnglelist_control_GHP)

## If necessary. or desired, the following code can be used to check the result.
#Anglecheck_PTN_Exp <- select(df_GHP_Exp, SubjectID, Angle, RecalculatedAngle, SignedAngle)
#Anglecheck_PTN_Ctrl <- select(df_GHP_Ctrl, SubjectID, Angle, RecalculatedAngle, SignedAngle)

#write.csv(Anglecheck_df_belt, "Anglecheck_belt_GHP")
#write.csv(Anglecheck_df_control, "Anglecheck_control_GHP")

# Data saving
write.csv(df_GHP_Exp, "df_GHP_Exp_Preprocessed.csv")
write.csv(df_GHP_Ctrl, "df_GHP_Ctrl_Preprocessed.csv")





