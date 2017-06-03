## Coursera, Data Science Specialisation, Course: Getting and Cleaning Data, Course Project
## Abhey Kumar
## 03 June 2017

## This script will perform the following steps on the UCI HAR data set downloaded from
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive variable names.
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(plyr)

## 1. Merges the training and the test sets to create one data set. (Ensure the correct folder is the working directory)

# 1.1 Read the data from the files
features <- read.table("features.txt")
activity <- read.table ("activity_labels.txt")

xtrain <- read.table("train/X_train.txt")
ytrain <- read.table("train/y_train.txt")
subjecttrain <- read.table("train/subject_train.txt")

xtest <- read.table("test/X_test.txt")
ytest <- read.table("test/y_test.txt")
subjecttest <- read.table("test/subject_test.txt")

# 1.2 Create the data sets
xdata <- rbind(xtrain, xtest)
ydata <- rbind(ytrain, ytest)
subjectdata <- rbind(subjecttrain, subjecttest)


## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

mean_and_std_dev <- grep("-(mean|std)\\(\\)", features[, 2])
xdata <- xdata[, mean_and_std_dev]

names(xdata) <- features[mean_and_std_dev, 2]


## 3. Uses descriptive activity names to name the activities in the data set

# 3.1 Update values with correct activity names
ydata[, 1] <- activity[ydata[, 1], 2]

# 3.2 Correct column name
names(ydata) <- "Activity"


## 4. Appropriately label the data set with descriptive activity names.

names(subjectdata) <- "Subject"

# Bind all the data in a single data set
alldata <- cbind(xdata, ydata, subjectdata)

# Rename the columns descriptively
colNames <- colnames (alldata)
for (i in 1:length(colNames)) {
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("std","StdDev",colNames[i])
  colNames[i] = gsub("mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","Time",colNames[i])
  colNames[i] = gsub("^(f)","Freq",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

colnames (alldata) <- colNames

## 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

tidydata <- ddply(alldata, .(Subject, Activity), function(x) colMeans(x[, 1:66]))
write.table(tidydata, "tidydata.txt", row.name=FALSE)
