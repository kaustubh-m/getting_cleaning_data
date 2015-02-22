# Read all the required data from " " delimited text files, and store in data frames

df_X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
df_features <- read.table("./UCI HAR Dataset/features.txt")
df_X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
df_Y_test <- read.table("./UCI HAR Dataset/test/Y_test.txt")
df_Y_train <- read.table("./UCI HAR Dataset/train/Y_train.txt")
df_subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
df_subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
df_activity_names <- read.table("./UCI HAR Dataset/activity_labels.txt")



# Name columns in df_X_test using list of feature names in df_features

colnames(df_X_test) <- unname(unlist(df_features[,2]))



# Name the single column in df_Y_test with the text "activity_id"

colnames(df_Y_test) <- "activity_id"



# Name the single column in df_Y_test with the text "subject"

colnames(df_subject_test) <- c("subject")



# 'Clip' (cbind) df_X_test to df_subject_test, and store to new data frame
# Output: Subject and 'test' feature data in one data frame (1 + 561 columns, 2947 rows)

df_subject_X_test <- cbind(df_subject_test, df_X_test)



# 'Clip' (cbind) data frame obtained above to df_Y_test (activity_id), store in new data frame
# Output: Activity ID, Subject, and 'test' feature data in one data frame (1 + 1 + 561 columns, 2947 rows)

df_actid_sub_test <- cbind(df_Y_test, df_subject_X_test)



# Name columns in df_X_test using list of feature names in df_features
# This is required for later appending 'train' data to 'test' data - column names have to match in rbind'

colnames(df_X_train) <- unname(unlist(df_features[,2]))
colnames(df_Y_train) <- "activity_id"



# Name the single column in df_Y_train with the text "subject"

colnames(df_subject_train) <- c("subject")



# 'Clip' (cbind) df_X_train to df_subject_train, and store to new data frame
# Output: Subject and 'train' feature data in one data frame (1 + 561 columns, 7352 rows)

df_subject_X_train <- cbind(df_subject_train, df_X_train)




# 'Clip' (cbind) data frame obtained above to df_Y_train (activity_id), store in new data frame
# Output: Activity ID, Subject, and 'train' feature data in one data frame (1 + 1 + 561 columns, 7352 rows)

df_actid_sub_train <- cbind(df_Y_train, df_subject_X_train)



# 'Clip' (rbind) data frame for 'train' data to data frame for 'test' data, store in new data frame
# Output: Activity ID, Subject, and 'test' + 'train' feature data in one data frame (1 + 1 + 561 columns, 10299 rows)

df_actid_sub_tst_trn <- rbind(df_actid_sub_test,df_actid_sub_train)



# Merge or Join data frame obtained above to df_activity_names to add the activity name to each record

colnames(df_activity_names) <- c("activity_id", "activity_name")
df_actnm_sub_tst_trn <- merge(df_actid_sub_tst_trn,df_activity_names, by="activity_id")



# Function used to bring column activity_name to the extreme left of data frame - for better readability of the data
#Source of code: http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe

moveMe <- function(data, tomove, where = "last", ba = NULL) {
  temp <- setdiff(names(data), tomove)
  x <- switch(
    where,
    first = data[c(tomove, temp)],
    last = data[c(temp, tomove)],
    before = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)-1))]
    },
    after = {
      if (is.null(ba)) stop("must specify ba column")
      if (length(ba) > 1) stop("ba must be a single character string")
      data[append(temp, values = tomove, after = (match(ba, temp)))]
    })
  x
}

df_actnm_sub_tst_trn <- moveMe(df_actnm_sub_tst_trn,c("activity_name"),"first")




# Select only the columns (features) with 'mean()' or 'std()' as part of the feature name

df_actnm_sub_mean_std <- select(df_actnm_sub_tst_trn,activity_name,subject,contains("mean()"), contains("std()"))



# Group by activity name, subject ID

df_grpby_actnm_sub <- group_by(df_actnm_sub_mean_std,activity_name,subject)



# Clean-up names of the features for better readability, replace hyphens with underscores, eliminate parentheses

colnames(df_grpby_actnm_sub) <- c(
  
  "activity_name",               "subject",                     "tBodyAcc_mean_X",          
  "tBodyAcc_mean_Y",           "tBodyAcc_mean_Z",           "tGravityAcc_mean_X",       
  "tGravityAcc_mean_Y",        "tGravityAcc_mean_Z",        "tBodyAccJerk_mean_X",      
  "tBodyAccJerk_mean_Y",       "tBodyAccJerk_mean_Z",       "tBodyGyro_mean_X",         
  "tBodyGyro_mean_Y",          "tBodyGyro_mean_Z",          "tBodyGyroJerk_mean_X",     
  "tBodyGyroJerk_mean_Y",      "tBodyGyroJerk_mean_Z",      "tBodyAccMag_mean",         
  "tGravityAccMag_mean",       "tBodyAccJerkMag_mean",      "tBodyGyroMag_mean",        
  "tBodyGyroJerkMag_mean",     "fBodyAcc_mean_X",           "fBodyAcc_mean_Y",          
  "fBodyAcc_mean_Z",           "fBodyAccJerk_mean_X",       "fBodyAccJerk_mean_Y",      
  "fBodyAccJerk_mean_Z",       "fBodyGyro_mean_X",          "fBodyGyro_mean_Y",         
  "fBodyGyro_mean_Z",          "fBodyAccMag_mean",          "fBodyBodyAccJerkMag_mean", 
  "fBodyBodyGyroMag_mean",     "fBodyBodyGyroJerkMag_mean", "tBodyAcc_std_X",           
  "tBodyAcc_std_Y",            "tBodyAcc_std_Z",            "tGravityAcc_std_X",        
  "tGravityAcc_std_Y",         "tGravityAcc_std_Z",         "tBodyAccJerk_std_X",       
  "tBodyAccJerk_std_Y",        "tBodyAccJerk_std_Z",        "tBodyGyro_std_X",          
  "tBodyGyro_std_Y",           "tBodyGyro_std_Z",           "tBodyGyroJerk_std_X",      
  "tBodyGyroJerk_std_Y",       "tBodyGyroJerk_std_Z",       "tBodyAccMag_std",          
  "tGravityAccMag_std",        "tBodyAccJerkMag_std",       "tBodyGyroMag_std",         
  "tBodyGyroJerkMag_std",      "fBodyAcc_std_X",            "fBodyAcc_std_Y",           
  "fBodyAcc_std_Z",            "fBodyAccJerk_std_X",        "fBodyAccJerk_std_Y",       
  "fBodyAccJerk_std_Z",        "fBodyGyro_std_X",           "fBodyGyro_std_Y",          
  "fBodyGyro_std_Z",           "fBodyAccMag_std",           "fBodyBodyAccJerkMag_std",  
  "fBodyBodyGyroMag_std",      "fBodyBodyGyroJerkMag_std"
) 



# Aggregate data frame obtained above to find the average value of each feature
df_actnm_sub_mean_feature <- summarise_each(df_grpby_actnm_sub, funs(mean))


# Write the data frame obtained above to the final output file
write.table(df_actnm_sub_mean_feature, file="./tidy_data.txt", row.names=FALSE)


# Remove all the intermediate data frames - optional - activate if required
# rm(df_X_test, df_features, df_X_train, df_X_train, df_Y_test, df_Y_train, df_subject_test, df_subject_train, df_activity_names, df_subject_X_test, df_actid_sub_test, df_subject_X_train, df_actid_sub_train, df_actid_sub_tst_trn, df_actnm_sub_tst_trn, df_actnm_sub_tst_trn, df_actnm_sub_mean_std,df_grpby_actnm_sub, df_actnm_sub_mean_feature)
