rm(list=ls())

x_test<-read.table("~/Documents/R/c3w4/X_test.txt")
y_test<-read.table("~/Documents/R/c3w4/Y_test.txt")
subject_test <- read.table("~/Documents/R/c3w4/subject_test.txt")
x_train<-read.table("~/Documents/R/c3w4/X_train.txt")
y_train<-read.table("~/Documents/R/c3w4/y_train.txt")
subject_train <- read.table("~/Documents/R/c3w4/subject_train.txt")
feature<-read.table("~/Documents/R/c3w4/features.txt")
activity_labels<-read.table("~/Documents/R/c3w4/activity_labels.txt", colClasses = "character")



# y_test and y_train match to activity label
# x_test and x_train match to feature 

## Step 1 Merges the training and the test sets to create one data set

# headering to each variable
names(x_test) <- feature[,2]
names(y_test) <- "activity"
names(subject_test)   <- "subject"

names(x_train) <- feature[,2]
names(y_train) <- "activity"
names(subject_train)   <- "subject"


# bind

xy_test <- cbind(subject_test, y_test, x_test)
xy_train <- cbind(subject_train, y_train, x_train)

m_data <- rbind(xy_train, xy_test)



## Step 2 Extracts only the measurements on the mean and standard deviation for each measurement

sub_data<-m_data[,c(1,2)]

for (i in 3:563) {
  if (grepl("mean", names(m_data[i]), ignore.case=TRUE)) {     #ignore.care!!!
    sub_data<-cbind(sub_data, m_data[i])
  }
  else if (grepl("std", names(m_data[i]), ignore.case=TRUE)) {
    sub_data<-cbind(sub_data, m_data[i])
  }
}


## Step 3 Uses descriptive activity names to name the activities in the data set

sub_data[,2]<-as.character(sub_data[,2])

for (i in 1:10299) {
  sub_data[i,2] <- activity_labels[sub_data[i,2],2]
}



## Step 4 Appropriately labels the data set with descriptive variable names 

names(sub_data)<-gsub("Acc", "Accelerometer", names(sub_data))
names(sub_data)<-gsub("Gyro", "Gyroscope", names(sub_data))
names(sub_data)<-gsub("BodyBody", "Body", names(sub_data))
names(sub_data)<-gsub("Mag", "Magnitude", names(sub_data))
names(sub_data)<-gsub("^t", "Time", names(sub_data))
names(sub_data)<-gsub("^f", "Frequency", names(sub_data))
names(sub_data)<-gsub("tBody", "TimeBody", names(sub_data))
names(sub_data)<-gsub("-mean()", "Mean", names(sub_data), ignore.case = TRUE)
names(sub_data)<-gsub("-std()", "STD", names(sub_data), ignore.case = TRUE)
names(sub_data)<-gsub("-freq()", "Frequency", names(sub_data), ignore.case = TRUE)
names(sub_data)<-gsub("angle", "Angle", names(sub_data))
names(sub_data)<-gsub("gravity", "Gravity", names(sub_data))


## Step5  From the data set in step 4, creates a second, independent tidy data 
##        set with the average of each variable for each activity and each subject.

library(dplyr)
library(tidyr)


sub_data[,1]<-as.factor(sub_data[,1])
sub_data[,2]<-as.factor(sub_data[,2])

tbl_data<-tbl_df(sub_data)
tbl_data

group_data<-group_by(tbl_data, subject, activity)

Tidy<-summarize_all(group_data, mean)

write.table(Tidy, "~/Documents/R/c3w4/Tidy.txt", sep=",", row.names=TRUE, col.names=TRUE)

