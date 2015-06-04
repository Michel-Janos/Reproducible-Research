
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
ds_activ <- cbind(activ,ds_xtr_xte ) # attaching column to x dataset
names(ds_activ)[names(ds_activ)=="V1"] <- "activity" # changing names of column



person <- rbind(subject_train,subject_test)
ds_person <- cbind(person,ds_activ)  # attaching column of subjects to ds_activ dataset
names(ds_person)[names(ds_person)=="V1"] <- "person"  # changing names of column

# changing name of ds_person dataset 

ds_complete <- ds_person

str(ds_complete)

# 'data.frame':  10299 obs. of  563 variables:
#   $ person                              : int  1 1 1 1 1 1 1 1 1 1 ...
# $ activity                            : int  5 5 5 5 5 5 5 5 5 5 ...
# $ tBodyAcc-mean()-X                   : num  0.289 0.278 0.28 0.279 0.277 ...
# $ tBodyAcc-mean()-Y                   : num  -0.0203 -0.0164 -0.0195 -0.0262 -0.0166 ...
# $ tBodyAcc-mean()-Z                   : num  -0.133 -0.124 -0.113 -0.123 -0.115 ...
# $ tBodyAcc-std()-X                    : num  -0.995 -0.998 -0.995 -0.996 -0.998 ...
# $ tBodyAcc-std()-Y                    : num  -0.983 -0.975 -0.967 -0.983 -0.981 ...

##################################################################
# 5 - selecting columns (means, sd)
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


##################################################################
# 6 -  From complete_final , creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.
##################################################################



tidy <- aggregate(. ~ person+activity, data = complete_final, mean)

##################################################################
# 7 - change labels of activities 
##################################################################


atri_factors<- read.table("act_labels.txt" , header = FALSE)

tidy <-cbind(atri_factors, tidy)


names(tidy)[names(tidy)=="V1"] <- "Activity"

##################################################################
# 8 - Delete column "activity"
##################################################################

tidy <- tidy[, -c(3)]

head(tidy)

# Activity person tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y tBodyAcc-std()-Z tGravityAcc-mean()-X
# 1  WALKING      1         0.2773308       -0.01738382        -0.1111481       -0.2837403       0.11446134       -0.2600279            0.9352232
# 2  WALKING      2         0.2764266       -0.01859492        -0.1055004       -0.4236428      -0.07809125       -0.4252575            0.9130173
# 3  WALKING      3         0.2755675       -0.01717678        -0.1126749       -0.3603567      -0.06991407       -0.3874120            0.9365067
# 4  WALKING      4         0.2785820       -0.01483995        -0.1114031       -0.4408300      -0.07882674       -0.5862528            0.9639997
# 5  WALKING      5         0.2778423       -0.01728503        -0.1077418       -0.2940985       0.07674840       -0.4570214            0.9726250
# 6  WALKING      6         0.2836589       -0.01689542        -0.1103032       -0.2965387       0.16421388       -0.5043242            0.9580675

tail(tidy)

# Activity person tBodyAcc-mean()-X tBodyAcc-mean()-Y tBodyAcc-mean()-Z tBodyAcc-std()-X tBodyAcc-std()-Y tBodyAcc-std()-Z tGravityAcc-mean()-X
# 175   LAYING     25         0.2507918       -0.01889437        -0.1004288       -0.9091165       -0.6917707       -0.7172620           -0.4611025
# 176   LAYING     26         0.2716459       -0.01918957        -0.1050025       -0.9694454       -0.9832314       -0.9845000           -0.6213097
# 177   LAYING     27         0.2741025       -0.01798676        -0.1076997       -0.9784552       -0.9837360       -0.9866370           -0.5304346
# 178   LAYING     28         0.2759135       -0.01675379        -0.1083449       -0.9688883       -0.9453868       -0.9564503           -0.4903345
# 179   LAYING     29         0.2872952       -0.01719655        -0.1094621       -0.9842196       -0.9902409       -0.9872551           -0.3467898
# 180   LAYING     30         0.2810339       -0.01944941        -0.1036582       -0.9763625       -0.9542018       -0.9670442           -0.3447378





table(tidy[, c("Activity", "person")])


##################################################################
# 9 - saving tidy.txt in wd
##################################################################
write.table(tidy, file = "tidy.txt", row.names = FALSE)



