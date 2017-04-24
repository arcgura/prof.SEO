rm(list=ls())

x_test<-read.table("~/Documents/R/c3w4/test/X_test.txt")
y_test<-read.table("~/Documents/R/c3w4/test/Y_test.txt")
subject_test <- read.table("~/Documents/R/c3w4/test/subject_test.txt")
x_train<-read.table("~/Documents/R/c3w4/train/X_train.txt")
y_train<-read.table("~/Documents/R/c3w4/train/y_train.txt")
subject_train <- read.table("~/Documents/R/c3w4/train/subject_train.txt")
feature<-read.table("~/Documents/R/c3w4/features.txt")
activity_labels<-read.table("~/Documents/R/c3w4/activity_labels.txt")



# y_  는 activity label과 matching
# x_  는 feature 와 matching


# 각 variable에 headering
names(x_test) <- feature[,2]
names(y_test) <- "activity"
names(subject_test)   <- "subject"

names(x_train) <- feature[,2]
names(y_train) <- "activity"
names(subject_train)   <- "subject"

## bind

xy_test <- cbind(subject_test, y_test, x_test)
xy_train <- cbind(subject_train, y_train, x_train)

m_data <- rbind(xy_train, xy_test)



## extracts only the measurements on the mean and SD


names(m_data[2,])
