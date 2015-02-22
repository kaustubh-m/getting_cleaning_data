# getting_cleaning_data
Getting and Cleaning Data - course project

run_analysis.R
Creation of a tidy data set using the raw data in the folder UCI HAR Dataset 
(a complete description of the raw data, as provided by the authors of the study, is appended to this text, at its end)

The unzipped and complete folder 'UCI HAR Dataset' must exist in the working directory, for run_analysis.R to work.

The output is a " " delimited txt file 'tidy_data.txt', which is created in the working directory

This script (run_analysis.R) includes a custom function (moveMe) used for moving columns within a data frame 
- this is a code snippet authored by Ananda Mahto, and publicly available at: 
http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe


Listed below are the steps in this script, along with the names of the intermediate data frames it creates.
These intermediate data frames can be removed by activating (un-commenting) the rm command at the end of the script (in run_analysis.R).

All the required data is read from " " delimited text files, and stored in data frames:

df_X_test
df_features
df_X_train
df_Y_test
df_Y_train
df_subject_test
df_subject_train
df_activity_names



'Clip' (cbind) df_X_test to df_subject_test, and store in new data frame
Output: Subject and 'test' feature data in one data frame (1 + 561 columns, 2947 rows)

df_subject_X_test 



'Clip' (cbind) data frame obtained above to df_Y_test (activity_id), store in new data frame
Output: Activity ID, Subject, and 'test' feature data in one data frame (1 + 1 + 561 columns, 2947 rows)

df_actid_sub_test



'Clip' (cbind) df_X_train to df_subject_train, and store to new data frame
Output: Subject and 'train' feature data in one data frame (1 + 561 columns, 7352 rows)

df_subject_X_train 



'Clip' (cbind) data frame obtained above to df_Y_train (activity_id), store in new data frame
Output: Activity ID, Subject, and 'train' feature data in one data frame (1 + 1 + 561 columns, 7352 rows)

df_actid_sub_train 



'Clip' (rbind) data frame for 'train' data to data frame for 'test' data, store in new data frame
Output: Activity ID, Subject, and 'test' + 'train' feature data in one data frame (1 + 1 + 561 columns, 10299 rows)

df_actid_sub_tst_trn 



Merge or Join data frame obtained above to df_activity_names to add the activity name to each record

df_actnm_sub_tst_trn 



Function used to bring column activity_name to the extreme left of data frame - for better readability of the data
Source of code: http://stackoverflow.com/questions/18339370/reordering-columns-in-a-large-dataframe

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






Select only the columns (features) with 'mean()' or 'std()' as part of the feature name

df_actnm_sub_mean_std 



Group by activity name, subject ID

df_grpby_actnm_sub 


Aggregate data frame obtained above to find the average value of each feature
df_actnm_sub_mean_feature <- summarise_each(df_grpby_actnm_sub, funs(mean))


Write the data frame obtained above to the final output file
"./tidy_data.txt"


End of text - original ReadMe (by authors of dataset) follows
=====================================================================================================================================================================================




==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. 
Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. 
Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. 
The experiments have been video-recorded to label the data manually. 
The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:
======================================

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:
=========================================

- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 
======
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
========
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

