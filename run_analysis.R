filesPath <- "C:\Users\alexmun\OneDrive\Data Science\Week4\UCI HAR Dataset"
setwd(filesPath)
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip",method="curl")

##Unzip DataSet to /data directory
unzip(zipfile="./data/Dataset.zip",exdir="./data")

##Load required packages
library(dplyr)
library(data.table)
library(tidyr)

##Read files
dataActivityTrain <- read.table("train/y_train.txt", quote="\"")
dataActivityTest <- read.table("test/y_test.txt", quote="\"")

features <- read.table("features.txt", quote="\"")
activitylabels <- read.table("activity_labels.txt", quote="\"")

dataSubjectTrain <- read.table("train/subject_train.txt", quote="\"")
dataSubjectTest <- read.table("test/subject_test.txt", quote="\"")

dataTrain <- read.table("train/X_train.txt", quote="\"")
dataTest <- read.table("test/X_test.txt", quote="\"")

##Merge the Subject training and the test sets by row binding 
##Rename variable "subject"
alldataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
setnames(alldataSubject, "V1", "subject")

##Merge the activity training and the test sets by row binding 
##Rename variable "activityNum"
alldataActivity<- rbind(dataActivityTrain, dataActivityTest)
setnames(alldataActivity, "V1", "activityNum")

##Combine the data training and test files
dataTable <- rbind(dataTrain, dataTest)

##Name variables according to feature e.g.(V1 = "tBodyAcc-mean()-X")
dataFeatures <- tbl_df(read.table(file.path(filesPath, "features.txt")))
setnames(dataFeatures, names(dataFeatures), c("featureNum", "featureName"))
colnames(dataTable) <- dataFeatures$featureName

##Column names for activity labels
activityLabels<- tbl_df(read.table(file.path(filesPath, "activity_labels.txt")))
setnames(activityLabels, names(activityLabels), c("activityNum","activityName"))

##Merge columns
alldataSubjAct<- cbind(alldataSubject, alldataActivity)
dataTable <- cbind(alldataSubjAct, dataTable)

##Read "features.txt" and extracting only the mean and standard deviation
dataFeaturesMeanStd <- grep("mean\\(\\)|std\\(\\)",dataFeatures$featureName,value=TRUE) #var name

##Taking only measurements for the mean and standard deviation and add "subject","activityNum"
dataFeaturesMeanStd <- union(c("subject","activityNum"), dataFeaturesMeanStd)
dataTable<- subset(dataTable,select=dataFeaturesMeanStd) 

##Enter name of activity into dataTable
dataTable <- merge(activityLabels, dataTable , by="activityNum", all.x=TRUE)
dataTable$activityName <- as.character(dataTable$activityName)

##Create dataTable with variable means sorted by subject and Activity
dataTable$activityName <- as.character(dataTable$activityName)
dataAggr<- aggregate(. ~ subject - activityName, data = dataTable, mean) 
dataTable<- tbl_df(arrange(dataAggr,subject,activityName))

##Label data with descriptive variable names
names(dataTable)<-gsub("std()", "SD", names(dataTable))
names(dataTable)<-gsub("mean()", "MEAN", names(dataTable))
names(dataTable)<-gsub("^t", "Time", names(dataTable))
names(dataTable)<-gsub("^f", "Frequency", names(dataTable))
names(dataTable)<-gsub("Acc", "Accelerometer", names(dataTable))
names(dataTable)<-gsub("Gyro", "Gyroscope", names(dataTable))
names(dataTable)<-gsub("Mag", "Magnitude", names(dataTable))
names(dataTable)<-gsub("BodyBody", "Body", names(dataTable))

##Create file
write.table(dataTable, "TidyData.txt", row.name=FALSE)