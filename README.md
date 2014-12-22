TidyDataAssignment
==================

This repo was created to submit the project assignment with Getting and Cleaning Data course from Coursera.

The approach to prepareing the data is as follows. 
1. Features.txt provides the column (variable names) for the measurements in each of test and train data set in the X_File
2. Activity Labels provide the description of the activity recorded
3. Y_ File set provides link of Activity to the measurements in X_File
4. Subject (train or test) provides the subject ID to be linked to measurements in X_File
2. For each of the test and train data set first join and  the rows between Activity Labels, Features, training labels and subject measurements data
2. Then rbind the 2 data sets to prepare a combined data set (from test and train)
3. remove the unwanted columns
4. Melt and DCast the data to compute the averages and collapse
5. Details in the R Script. marked by comments where data preparation takes place
6. 
