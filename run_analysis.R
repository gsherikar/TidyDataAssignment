#
# Author: Girish Sherikar, Coursera Project Assignment - Getting and Cleaning Data 
# Load the required libraries
# It is assumed that the following packages have been installed

library(data.table)
library(dplyr)
library(tidyr)
library(plyr)
library(reshape2)

# Activity Labels and feature names are common to both sets of data (train and test), so read in once, cleanse them
# Read in the feature names from features.txt

featureColNames<-tbl_df(read.table("features.txt",sep=" ",col.names=c("ID","featureName")))

# Cleanup the special characters etc. from feature Names so they can be used as names for variables
featureColNames<-data.table(featureColNames %>% 
                                    mutate(featureName=sub("fBodyBody","fBody",featureName)) %>%
                                    mutate(featureName=sub("angle\\(t","angleTimeBody",featureName)) %>%
                                    mutate(featureName=sub("angle\\(","angle",featureName)) %>%       
                                    mutate(featureName=sub("-","_",featureName)) %>% 
                                    mutate(featureName=sub("\\(\\)","",featureName)) %>% 
                                    mutate(featureName=sub("-","_",featureName)) %>%
                                    mutate(featureName=sub(",","_",featureName))   %>% 
                                    mutate(featureName=sub("\\(","",featureName))   %>%
                                    mutate(featureName=sub("\\)","",featureName)) %>% 
                                    mutate(featureName=sub("\\)","",featureName)) %>%
                                    mutate(featureName=sub("^f","freq",featureName)) %>% 
                                    mutate(featureName=sub("^t","time",featureName)))

# Read in the activiy names from activity_labels.txt

activityLabelsData <-tbl_df(read.table("activity_labels.txt",sep=" ",col.names=c("Activity_ID","Activity")))

activityLabelsData <-tbl_df(read.table("activity_labels.txt",sep=" ",col.names=c("Activity_ID","Activity")))


# The following function processDataSet has been designed to perform the repetitive task of reading data sets from
# the 2 folders, train and test. It has 4 arguments
# X_dataFile = the measurement set file name (e.g. X_test.txt or X_train.txt)
# Y_dataFile = Labels associated with measurement set file name (e.g. Y_test.txt or Y_train.txt)
# subject_ID_DataFile = subject ID associated with each measurement (e.g. subject_test.txt or subject_train.txt)
# sourceFolder = the folder from which to read the files. For this assignments it would be test or train

# The final output of this function is data that is merged from the 3 files along with activity labels and feature names assgined
# as variable (or column) names

processDataSet <- function(X_dataFile, Y_dataFile, subject_ID_DataFile, sourceFolder) {
        
        # PRIMARY DATS SET # 1 preparation
        
        # Y_dataFile has activity ID associated to each of the measurement
        activityIDOfDataSet <-tbl_df(read.table(Y_dataFile,sep="",col.names=c("Activity_ID")))
        
        # We now have table with single column called Activity_ID
        
        # associate an activity lable to each of the activity ID associated with each measurement
        activityLabelsOfDataSet <- join(activityIDOfDataSet,activityLabelsData,by="Activity_ID")
        
        # We now have a table with 2 columns: Activity_ID and Activity
        
        # Here we add an extra column called idx which will be helpful in merging data. It, in effect assigns a row number
        # sequentially for each row in the data set. The same principle is used subsequently and consistently
        activityLabelsOfDataSet <- activityLabelsOfDataSet %>% mutate(idx=0)
        
        j <- ncol(activityLabelsOfDataSet)
        for (i in 1:nrow(activityLabelsOfDataSet)) {
                activityLabelsOfDataSet[i,j]=i
        } 
        # subject_ID_DataFile contains Subject ID associated with each measurement row in X_Datafile
        
        subjectIDOfDataSet <- tbl_df(read.table(subject_ID_DataFile,sep="",col.names=c("Subject_ID")))
        
        # We now have a table with a single column called Subject_ID
        
        # assign a row number for each row by adding an extra column called idx. This will be helpful in merging
        subjectIDOfDataSet <- subjectIDOfDataSet %>% mutate(idx=0)
        
        j <- ncol(subjectIDOfDataSet)
        for (i in 1:nrow(subjectIDOfDataSet)) {
                subjectIDOfDataSet[i,j]=i
        } 
        
        # merge Subject ID with Activity Labels using idx as column to merge
        
        subjectactivityLabelsOfDataSet <- merge(subjectIDOfDataSet,activityLabelsOfDataSet,by="idx")

        # PRIMARY DAT SET # 1 ReadY!
        # At this point we have idx, Subject_ID, Activity_ID and Activity as columns 
        # for each of the measurements
        
        # PRIMARY DATS SET # 2 preparation
        # now read in the actual measurements data
        
        measurementsData <- tbl_df(read.table(X_dataFile,header=F,col.names=c(featureColNames$featureName)))
        
        # like earlier assign a unique row ID for each row in new column called idx. helps in merging
        measurementsData <- measurementsData %>% mutate(idx=0)
        
        j<-ncol(measurementsData)
        
        for (i in 1:nrow(measurementsData)) {
                measurementsData[i,j]=i
        } 
        
        # PRIMARY DAT SET # 2 ReadY!
        # We now have a unique row number assigned to each of the measurements
        
        # PRIMARY DATA SETS #1 and 2 
        preparedDataSet <-merge(subjectactivityLabelsOfDataSet,measurementsData,by="idx")
        
        # Not required as such but for the heck of it I have introduced a column to identify the source
        # folder from this these files were read
        preparedDataSet <- preparedDataSet %>% mutate(source=sourceFolder)
        
        return(preparedDataSet)
        
}

# We are now ready for the action

# First we will process the files in test folder, so prepare the arguments for processDataSet function and call it
sourceFolder <- "test"
X_dataFile <- ".\\test\\X_test.txt"
Y_dataFile <- ".\\test\\Y_test.txt"
subject_ID_DataFile <- ".\\test\\subject_test.txt"

testDataSet <- processDataSet(X_dataFile, Y_dataFile, subject_ID_DataFile,sourceFolder) # function call 1

# Data from test folder is now ready and in prepared state

# Now, process the files in train folder, so prepare the arguments for processDataSet function and call it

sourceFolder <- "train"
X_dataFile <- ".\\train\\X_train.txt"
Y_dataFile <- ".\\train\\Y_train.txt"
subject_ID_DataFile <- ".\\train\\subject_train.txt"

trainDataSet <- processDataSet(X_dataFile, Y_dataFile, subject_ID_DataFile,sourceFolder) # function call 2

# bind the data prepared from test and train folders so we to merge data from both folders

combinedDataSet <- arrange(rbindlist(list(testDataSet,trainDataSet),use.names=TRUE,fill=TRUE),Activity)

# PREPARATION FOR FINAL RESULTS

# remove columns that do not have the word mean or std in them
# identify variables/columns that have the word "mean" in the label of the variable or column heading
varsHavingMean <- names(select(combinedDataSet,contains("mean")))

# identify variables/columns that have the word "std" in the label of the variable or column heading
varsHavingStd <- names(select(combinedDataSet,contains("std")))

# combine the list of variables (columns) having mean or standard deviation
varsToRetain<-c(varsHavingMean,varsHavingStd)  # combine the 2 vectors containing variable names prepared above

# Prepare  merged data set with only the columns to be retained
combinedDataSet <- select(combinedDataSet,Activity,Subject_ID,one_of(varsToRetain))

#Melt data to so we have a unique combination of Subject Id and Activity for each measurement as variable and it's value 
combinedDataMelt<-melt(combinedDataSet,id=c("Activity","Subject_ID"),measure.vars=varsToRetain)

# Cast data to compute average for each variable for a given subject and activity
finalResult <- dcast(combinedDataMelt,Activity+Subject_ID~variable,mean)

# Last but not the least, rename the measurement variables to be very clear that the measures represent average value
for (i in 3:length(colnames(finalResult))) {colnames(finalResult)[i] <- paste("AverageOf_",colnames(finalResult)[i])}

# Write the final output to text file for submission
write.table(finalResult,"finalresult.txt",row.names=FALSE)



