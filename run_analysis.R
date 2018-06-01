#0. Prepearation before start
  #Set working directory
     setwd("C:/Users/yh2915/Desktop/Getting and Cleaning Data/Week 4/getdata%2Fprojectfiles%2FUCI HAR Dataset/UCI HAR Dataset")
     wd<-getwd()
  #Read required datasets in UCI HAR Dataset
     features<-read.table("features.txt")
  #Read required dataset in test file
     test_file<-file.path(wd,"test")
     subject_test_name<-file.path(test_file,"subject_test.txt")
     X_test_name<-file.path(test_file,"X_test.txt")
     y_test_name<-file.path(test_file,"y_test.txt")
   
     subject_test<-read.table(subject_test_name)
     X_test<-read.table(X_test_name)
     y_test<-read.table(y_test_name)
  #Read required dataset in train file
     train_file<-file.path(wd,"train")
     subject_train_name<-file.path(train_file,"subject_train.txt")
     X_train_name<-file.path(train_file,"X_train.txt")
     y_train_name<-file.path(train_file,"y_train.txt")
    
     subject_train<-read.table(subject_train_name)
     X_train<-read.table(X_train_name)
     y_train<-read.table(y_train_name)

#-----------------------------------------------------------------------------------------------------#
#1. Merges the training and the test sets to create one data set.
   test_set<-cbind(subject_test,y_test,X_test)
   train_set<-cbind(subject_train,y_train,X_train)
   combined_set<-rbind(test_set,train_set)

#-----------------------------------------------------------------------------------------------------#
#4.Appropriately labels the data set with descriptive variable names.
   feature_labels<-as.character(features[,2])
   colnames(combined_set)<-c("subjectNumber_Performer","Activities",feature_labels)
   
#-----------------------------------------------------------------------------------------------------#   
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
   #obtain the names of the columns containing mean and standard deviation measurements.
   mean_feature<-grep("mean",features[,2],value=TRUE)
   std_feature<-grep("std",features[,2],value=TRUE)
   #Save these columns to a new variable
   new_combined_set<-combined_set[,c("subjectNumber_Performer","Activities",mean_feature,std_feature)]
   
#-----------------------------------------------------------------------------------------------------#
#3. Uses descriptive activity names to name the activities in the data set.
   activity_labels<-read.table(file.path(wd,"activity_labels.txt"))
   for (i in 1:nrow(activity_labels)){
     labels<-as.character(activity_labels[i,2])
     new_combined_set$Activities<-replace(new_combined_set$Activities,new_combined_set$Activities==i,labels)
   }
  #Output data to a new file
   file.create("Step4.csv")
   write.csv(new_combined_set,file="Step4.csv",row.name=FALSE)

#-----------------------------------------------------------------------------------------------------#
   #5. From the data set in step 4, creates a second, independent tidy data set with the average of 
   #   each variable for each activity and each subject.
   #Read data set from step 4
   new_combined_set<-read.csv("Step4.csv")
   #Summarize dataset 
   attach(new_combined_set)
   CleanData <-aggregate(new_combined_set[,c(3:81)], by=list(subjectNumber_Performer,Activities), 
                         FUN=mean)
   colnames(CleanData)<-c("subjectNumber_Performer","Activities",
                          #tBodyAcc-mean()-XYZ#
                          "Mean_rawAccelerometer_Body_Time.X","Mean_rawAccelerometer_Body_Time.Y","Mean_rawAccelerometer_Body_Time.Z",
                          #tGravityAcc-mean()-XYZ#
                          "Mean_rawAccelerometer_Gravity_Time.X", "Mean_rawAccelerometer_Gravity_Time.Y", "Mean_rawAccelerometer_Gravity_Time.Z",
                          #tBodyAccJerk-mean()-XYZ#
                          "Mean_LinearAcceleration_Body_Time.X","Mean_LinearAcceleration_Body_Time.Y","Mean_LinearAcceleration_Body_Time.Z",
                          #tBodyGyro-mean()-XYZ#
                          "Mean_rawGyroscope_Body_Time.X","Mean_rawGyroscope_Body_Time.Y","Mean_rawGyroscope_Body_Time.Z",
                          #tBodyGyroJerk-mean()-X#
                          "Mean_AngularVelocity_Body_Time.X", "Mean_AngularVelocity_Body_Time.Y", "Mean_AngularVelocity_Body_Time.Z",
                          #tBodyAccMag-mean()#
                          "Mean_rawAccelerometer_Body_Time_Magnitude",
                          #tGravityAccMag-mean()#
                          "Mean_rawAccelerometer_Gravity_Time_Magnitude",
                          #tBodyAccJerkMag-mean()#
                          "Mean_LinearAcceleration_Body_Time_Magnitude",
                          #tBodyGyroMag-mean()#
                          "Mean_rawGyroscope_Body_Time_Magnitude",
                          #tBodyGyroJerkMag-mean()#
                          "Mean_AngularVelocity_Body_Time_Magnitude",
                          
                          #fBodyAcc-mean()-XYZ#
                          "Mean_rawAccelerometer_Body_Frequency.X","Mean_rawAccelerometer_Body_Frequency.Y","Mean_rawAccelerometer_Body_Frequency.Z",
                          #fBodyAcc-meanFreq()-X#
                          "WeightedMean_rawAccelerometer_Body_Frequency.X","WeightedMean_rawAccelerometer_Body_Frequency.Y","WeightedMean_rawAccelerometer_Body_Frequency.Z",
                          #fBodyAccJerk-mean()-XYZ#
                          "Mean_LinearAcceleration_Body_Frequency.X","Mean_LinearAcceleration_Body_Frequency.Y","Mean_LinearAcceleration_Body_Frequency.Z",
                          #fBodyAccJerk-meanFreq()-XYZ#
                          "WeightedMean_LinearAcceleration_Body_Frequency.X","WeightedMean_LinearAcceleration_Body_Frequency.Z","WeightedMean_LinearAcceleration_Body_Frequency.Z",
                          #fBodyGyro-mean()-XYZ#
                          "Mean_rawGyroscope_Body_Frequency.X","Mean_rawGyroscope_Body_Frequency.Y","Mean_rawGyroscope_Body_Frequency.Z",
                          #fBodyGyro-meanFreq()-XYZ#
                          "WeightedMean_rawGyroscope_Body_Frequency.X","WeightedMean_rawGyroscope_Body_Frequency.Y","WeightedMean_rawGyroscope_Body_Frequency.Z",
                          #fBodyAccMag-mean()#
                          "Mean_rawAccelerometer_Body_Frequency_Magnitude",
                          #fBodyAccMag-meanFreq()#
                          "WeightedMean_rawAccelerometer_Body_Frequency_Magnitude",
                          #fBodyBodyAccJerkMag-mean()#
                          "Mean_LinearAcceleration_Body_Frequency_Magnitude",
                          #fBodyBodyAccJerkMag-meanFreq()" 
                          "WeightedMean_LinearAcceleration_Body_Frequency_Magnitude",
                          #fBodyBodyGyroMag-mean()"#
                          "Mean_rawGyroscope_Body_Frequency_Magnitude",
                          #fBodyBodyGyroMag-meanFreq()"#
                          "WeightedMean_rawGyroscope_Body_Frequency_Magnitude",
                          #fBodyBodyGyroJerkMag-mean()"   
                          "Mean_AngularVelocity_Body_Frequency_Magnitude",
                          #fBodyBodyGyroJerkMag-meanFreq()#
                          "WeightedMean_AngularVelocity_Body_Frequency_Magnitude",
                          
                          #tBodyAcc-std()-XYZ#
                          "Mean_Std_rawAccelerometer_Body_Time.X","Mean_Std_rawAccelerometer_Body_Time.Y","Mean_Std_rawAccelerometer_Body_Time.Z",
                          #tGravityAcc-std()-XYZ#
                          "Mean_Std_rawAccelerometer_Gravity_Time.X", "Mean_Std_rawAccelerometer_Gravity_Time.Y", "Mean_Std_rawAccelerometer_Gravity_Time.Z",
                          #tBodyAccJerk-std()-XYZ#
                          "Mean_Std_LinearAcceleration_Body_Time.X", "Mean_Std_LinearAcceleration_Body_Time.Y", "Mean_Std_LinearAcceleration_Body_Time.Z",
                          #tBodyGyro-std()-XYZ#
                          "Mean_Std_rawGyroscope_Body_Time.X","Mean_Std_rawGyroscope_Body_Time.Y","Mean_Std_rawGyroscope_Body_Time.Z",
                          #tBodyGyroJerk-std()-X#
                          "Mean_Std_AngularVelocity_Body_Time.X","Mean_Std_AngularVelocity_Body_Time.Y","Mean_Std_AngularVelocity_Body_Time.Z",
                          #tBodyAccMag-std()#
                          "Mean_Std_rawAccelerometer_Body_Time_Magnitude",
                          #tGravityAccMag-std()#
                          "Mean_Std_rawAccelerometer_Gravity_Time_Magnitude",
                          #tBodyAccJerkMag-std()#
                          "Mean_Std_LinearAcceleration_Body_Time_Magnitude",
                          #tBodyGyroMag-std()#
                          "Mean_Std_rawGyroscope_Body_Time_Magnitude",
                          #tBodyGyroJerkMag-std()#
                          "Mean_Std_AngularVelocity_Body_Time_Magnitude",   
                          
                          #fBodyAcc-std()-XYZ#
                          "Mean_Std_rawAccelerometer_Body_Frequency.X","Mean_Std_rawAccelerometer_Body_Frequency.Y","Mean_Std_rawAccelerometer_Body_Frequency.Z",
                          #fBodyAccJerk-std()-XYZ#
                          "Mean_Std_LinearAcceleration_Body_Frequency.X","Mean_Std_LinearAcceleration_Body_Frequency.Y","Mean_Std_LinearAcceleration_Body_Frequency.Z",
                          #fBodyGyro-std()-XYZ#
                          "Mean_Std_rawGyroscope_Body_Frequency.X","Mean_Std_rawGyroscope_Body_Frequency.Y","Mean_Std_rawGyroscope_Body_Frequency.Z",
                          #fBodyAccMag-std()#
                          "Mean_Std_rawAccelerometer_Body_Frequency_Magnitude",
                          #fBodyBodyAccJerkMag-std()#
                          "Mean_Std_LinearAcceleration_Body_Frequency_Magnitude",
                          #fBodyBodyGyroMag-std()"#
                          "Mean_Std_rawGyroscope_Body_Frequency_Magnitude",
                          #fBodyBodyGyroJerkMag-std()"   
                          "Mean_Std_AngularVelocity_Body_Frequency_Magnitude")
   detach(new_combined_set)
   print(CleanData)
   
   #Output data to a new file
   file.create("Assignmentoutput.txt")
   write.table(CleanData,file="Assignmentoutput.txt",row.name=FALSE)
   