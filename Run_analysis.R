
##################################################################
# 1 - first I place all data files (txt) in the working directory
##################################################################
#reading datasets
subject_train <- read.table("subject_train.txt", header = FALSE)
y_train <- read.table("y_train.txt", header = FALSE)
X_train <- read.table("X_train.txt", header = FALSE)

subject_test <- read.table("subject_test.txt", header = FALSE)
y_test <- read.table("y_test.txt", header = FALSE)
X_test <- read.table("X_test.txt", header = FALSE)

##################################################################
# 2 -  joining x datasets
##################################################################
ds_xtr_xte <- rbind(X_train,X_test)



#joining headers("features")
YYY <- read.table("features.txt" , header=F, sep="")
zzz <- YYY[, -c(1)] # excluding 1st column
www <- t(zzz) # transposing matrix


head(www)

# [1] tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X  tBodyAcc-std()-Y  tBodyAcc-std()-Z 
# 477 Levels: angle(tBodyAccJerkMean),gravityMean) angle(tBodyAccMean,gravity) ... tGravityAccMag-std()


##################################################################
# 3 - setting  "features" to column names
##################################################################

#colnames(ds_xtr_xte, do.NULL = TRUE, prefix = "col")
colnames(ds_xtr_xte) <- www
str(ds_xtr_xte)

# 'data.frame':  10299 obs. of  561 variables:
#   $ tBodyAcc-mean()-X                   : num  0.289 0.278 0.28 0.279 0.277 ...
# $ tBodyAcc-mean()-Y                   : num  -0.0203 -0.0164 -0.0195 -0.0262 -0.0166 ...
# $ tBodyAcc-mean()-Z                   : num  -0.133 -0.124 -0.113 -0.123 -0.115 ...
# $ tBodyAcc-std()-X                    : num  -0.995 -0.998 -0.995 -0.996 -0.998 ...
# $ tBodyAcc-std()-Y                    : num  -0.983 -0.975 -0.967 -0.983 -0.981 ...
# $ tBodyAcc-std()-Z                    : num  -0.914 -0.96 -0.979 -0.991 -0.99 ...
# $ tBodyAcc-mad()-X                    : num  -0.995 -0.999 -0.997 -0.997 -0.998 ...
# $ tBodyAcc-mad()-Y                    : num  -0.983 -0.975 -0.964 -0.983 -0.98 ...
# $ tBodyAcc-mad()-Z                    : num  -0.924 -0.958 -0.977 -0.989 -0.99 ...
# $ tBodyAcc-max()-X                    : num  -0.935 -0.943 -0.939 -0.939 -0.942 ...



##################################################################
# 4 - joining y datasets
##################################################################

activ <- rbind(y_train,y_test)


##################################################################
#  5 change  activities names and headers
##################################################################
class(activ)
as.factor(activ$V1)


library(plyr)

new_act_names <- mapvalues(activ$V1, from = c("1","2","3","4","5","6"), to = c("walking", "WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

ds_activ <- cbind(new_act_names,ds_xtr_xte ) # attaching column to x dataset
names(ds_activ)[names(ds_activ)=="new_act_names"] <- "activity" # changing names of column

person <- rbind(subject_train,subject_test)
ds_person <- cbind(person,ds_activ)  # attaching column of subjects to ds_activ dataset
names(ds_person)[names(ds_person)=="V1"] <- "person"  # changing names of column

# changing name of ds_person dataset 

ds_complete <- ds_person

str(ds_complete)

# 'data.frame':  10299 obs. of  563 variables:
#   $ person                              : int  1 1 1 1 1 1 1 1 1 1 ...
# $ activity                            : Factor w/ 6 levels "LAYING","SITTING",..: 3 3 3 3 3 3 3 3 3 3 ...
# $ tBodyAcc-mean()-X                   : num  0.289 0.278 0.28 0.279 0.277 ...
# $ tBodyAcc-mean()-Y                   : num  -0.0203 -0.0164 -0.0195 -0.0262 -0.0166 ...
# $ tBodyAcc-mean()-Z                   : num  -0.133 -0.124 -0.113 -0.123 -0.115 ...
# $ tBodyAcc-std()-X                    : num  -0.995 -0.998 -0.995 -0.996 -0.998 ... ...

##################################################################
# 6 - selecting columns (means, sd)
##################################################################
complete_final <- ds_complete[, c(1:8,43:48,83:88,123:128,163:168, 203:204,229:230,242:243,255:256,268,273,296:298,347:352,426:431,505:506,515,516:519,528,531:532,544:545,554,557:563 )]


#missed on first selection
complete_final <- complete_final[, -c(62,61)]
str(complete_final)
# 'data.frame':  10299 obs. of  75 variables:
#   $ person                              : int  1 1 1 1 1 1 1 1 1 1 ...
# $ activity                            : int  5 5 5 5 5 5 5 5 5 5 ...
# $ tBodyAcc-mean()-X                   : num  0.289 0.278 0.28 0.279 0.277 ...
# $ tBodyAcc-mean()-Y                   : num  -0.0203 -0.0164 -0.0195 -0.0262 -0.0166 ...
# $ tBodyAcc-mean()-Z                   : num  -0.133 -0.124 -0.113 -0.123 -0.115 ...
# $ tBodyAcc-std()-X                    : num  -0.995 -0.998 -0.995 -0.996 -0.998 ...
# $ tBodyAcc-std()-Y                    : num  -0.983 -0.975 -0.967 -0.983 -0.981 ...
# $ tBodyAcc-std()-Z                    : num  -0.914 -0.96 -0.979 -0.991 -0.99 ...
# $ tGravityAcc-mean()-X                : num  0.963 0.967 0.967 0.968 0.968 ...
# $ tGravityAcc-mean()-Y                : num  -0.141 -0.142 -0.142 -0.144 -0.149 ...
# $ tGravityAcc-mean()-Z                : num  0.1154 0.1094 0.1019 0.0999 0.0945 ...






#tidy <- aggregate(. ~ person+activity, data = complete_final, mean)

##################################################################
# 7 - change labels of activities 
##################################################################


#names(complete_final)[names(complete_final)=="activityLabels$V2"] <- "Activ"


names(complete_final)<-gsub("^t", "time", names(complete_final))
names(complete_final)<-gsub("^f", "frequency", names(complete_final))
names(complete_final)<-gsub("Acc", "Accelerometer", names(complete_final))
names(complete_final)<-gsub("Gyro", "Gyroscope", names(complete_final))
names(complete_final)<-gsub("Mag", "Magnitude", names(complete_final))
names(complete_final)<-gsub("BodyBody", "Body", names(complete_final))

names(complete_final)

# [1] "person"                                             "activity"                                          
# [3] "timeBodyAccelerometer-mean()-X"                     "timeBodyAccelerometer-mean()-Y"                    
# [5] "timeBodyAccelerometer-mean()-Z"                     "timeBodyAccelerometer-std()-X"                     
# [7] "timeBodyAccelerometer-std()-Y"                      "timeBodyAccelerometer-std()-Z"                     
# [9] "timeGravityAccelerometer-mean()-X"                  "timeGravityAccelerometer-mean()-Y"                 
# [11] "timeGravityAccelerometer-mean()-Z"                  "timeGravityAccelerometer-std()-X"                  
# [13] "timeGravityAccelerometer-std()-Y"                   "timeGravityAccelerometer-std()-Z"  


##################################################################
# 8 -  From complete_final , creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.
##################################################################


tidy <- aggregate(. ~ person+activity, data = complete_final, mean)

head(tidy)

# person activity timeBodyAccelerometer-mean()-X timeBodyAccelerometer-mean()-Y timeBodyAccelerometer-mean()-Z timeBodyAccelerometer-std()-X
# 1      1   LAYING                      0.2215982                    -0.04051395                     -0.1132036                    -0.9280565
# 2      2   LAYING                      0.2813734                    -0.01815874                     -0.1072456                    -0.9740595
# 3      3   LAYING                      0.2755169                    -0.01895568                     -0.1013005                    -0.9827766
# 4      4   LAYING                      0.2635592                    -0.01500318                     -0.1106882                    -0.9541937
# 5      5   LAYING                      0.2783343                    -0.01830421                     -0.1079376                    -0.9659345
# 6      6   LAYING                      0.2486565                    -0.01025292                     -0.1331196                    -0.9340494


# checking the dataset
table(tidy[, c("activity", "person")])

person
activity             1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
LAYING             1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
SITTING            1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
STANDING           1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
walking            1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
WALKING_DOWNSTAIRS 1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
WALKING_UPSTAIRS   1 1 1 1 1 1 1 1 1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1  1
##################################################################
# 9 - saving tidy.txt in wd
##################################################################
write.table(tidy, file = "tidy.txt", row.names = FALSE)





