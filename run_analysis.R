## 0.1 Loads the necessary packages

library(dplyr)
library(tidyr)

## 0.2 Downloads, the data set and reads in the required txt files 

URL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(URL,"", method = "libcurl", mode = "wb")

zipF<-"./UCIHARData.zip" 
outDir<-getwd()  
unzip(zipF,exdir=outDir)

testx<-read.table("./UCI HAR Dataset/test/X_test.txt")
testy<-read.table("./UCI HAR Dataset/test/y_test.txt")
trainx<-read.table("./UCI HAR Dataset/train/X_train.txt")
trainy<-read.table("./UCI HAR Dataset/train/y_train.txt")
testsub<-read.table("./UCI HAR Dataset/test/subject_test.txt")
trainsub<-read.table("./UCI HAR Dataset/train/subject_train.txt")

## 1.0 Merge the training and the test sets to create one data set.

test<-cbind(testsub,testy,testx)
train<-cbind(trainsub,trainy,trainx)
master<-rbind(test,train)

## 2.0 Extracts only the measurements on the mean and standard deviation for each measurement. 

features<-read.table("./UCI HAR Dataset/features.txt")
featurecolumnid<-c(grep("\\b-mean()\\b",features$V2)+2,grep("std",features$V2)+2)
mastermeanstd<-master[,c(1:2,featurecolumnid)]

## 3.0 Uses descriptive activity names to name the activities in the data set

activitylabels<-read.table("./UCI HAR Dataset/activity_labels.txt")
activitylabels$V2<-gsub("_"," ",activitylabels$V2)

mastermeanstd$V1.1<-sapply(mastermeanstd$V1.1, function(x){activitylabels$V2[match(x,activitylabels$V1)]})

## 4.0 Appropriately labels the data set with descriptive variable names.
col_name<-data.frame(ID=1:68,Name=c("SubjectID","ActivityName",features[featurecolumnid-2,2]))

col_name$Name<-sub("fB","FourierTransB",col_name$Name)
col_name$Name<-sub("fG","FourierTransG",col_name$Name)
col_name$Name<-sub("tB","TimeB",col_name$Name)
col_name$Name<-sub("tG","TimeG",col_name$Name)
col_name$Name<-sub("\\(\\)","",col_name$Name)
col_name$Name<-sub("mean","Mean",col_name$Name)
col_name$Name<-sub("std","StD",col_name$Name)

names(mastermeanstd)<-col_name[,2]

## 5.0 Create a second, independent tidy data set with the average of each variable for each activity and each subject.

mastercolmeans<-mastermeanstd%>%group_by(SubjectID,ActivityName)%>%summarise_all(mean)
TidyUCIHCAR<-mastercolmeans%>%pivot_longer(-(SubjectID:ActivityName),"Measure",values_to = "Value")

write.table(TidyUCIHCAR,file="TidyUCIHCAR.txt",row.names=FALSE)





