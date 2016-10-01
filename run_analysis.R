# Getting and Cleaning Data
# Course Project
# 2016.10.01


# DOWNLOAD AND UNZIP FILES
# CAN BE SKIPPED IF ALREADY HAVE THE DOWNLOADED DATA FROM THE LINK BELOW IN THE WORKING DIRECTORY
# dataURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
# download.file(dataURL,"dataDownload.zip")
# unzip("dataDownload.zip")

library(dplyr)

# Set WD to location of the files
setwd("UCI HAR Dataset")

# Pull in label information for reference
features <- read.table("features.txt",stringsAsFactors = FALSE)
activity_labels <- rename(read.table("activity_labels.txt",stringsAsFactors=FALSE),activityLabel=V2)

# function to run on each folder of data indvidually
# renames the appropriate columns and merges them
mergeFiles <- function (dir) {
      a<-rename(read.table(paste(dir,"/subject_",dir,".txt",sep="")),Subject=V1)
      b<-rename(read.table(paste(dir,"/y_",dir,".txt",sep="")),activityID=V1)
      b<-merge(b,activity_labels,by.x="activityID",by.y="V1")
      c<-read.table(paste(dir,"/X_",dir,".txt",sep=""))
      colnames(c)<-features$V2
      return(cbind(a,b,c))
}

# prepare both the test and train data
testData <- mergeFiles("test")
trainData <- mergeFiles("train")

# combine the rows from both datasets for a final output
allData <- rbind(testData,trainData)

# Select only measurements that are means or standard deviations
finalData <- allData[,c(1,2,3,grep("mean\\()|std\\()",names(allData)))]

# melting data down and then averaging by variable, then outputting
library(reshape2)
tidyAll <- melt(finalData[,-2],id=1:2)
tidyFinal <- summarize(group_by(tidyAll,Subject,activityLabel,variable),mean(value))

# put in output format
tidyOutput <- dcast(tidyFinal, Subject + activityLabel ~ variable)
setwd('..') # going back up one level to save output
write.table(tidyOutput,"tidyData.txt",row.names=FALSE)






