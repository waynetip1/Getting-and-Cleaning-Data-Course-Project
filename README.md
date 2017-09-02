## Getting and Cleaning Data Course Project
### Author: Wayne Tipton
### Date: September 1, 2017

## Project Description
This course project is about getting data and making the data tidy.

## The Data
The data is a collection of measurements from accelerometers from the
Samsung Galaxy S smartphone. 

## The Script and Explanations
### Downloading and Unzipping 
        library(dplyr)

        library(tidyr)

        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

        download.file(fileURL,destfile = "./data/dataset.zip") # relative file path

        unzip(zipfile ="./data/dataset.zip", exdir = "./data")

        pathdata <- file.path("./data/UCI HAR Dataset")

### Reading in the data
#### Reading training tables
        xtrain <- read.table(file.path(pathdata, "train", "X_train.txt"),header = F)

        ytrain <- read.table(file.path(pathdata, "train", "y_train.txt"),header = F)

        sub_train <-read.table(file.path(pathdata,"train","subject_train.txt"),header=F)

#### Reading the testing tables
        xtest <- read.table(file.path(pathdata, "test", "X_test.txt"),header = F)

        ytest <- read.table(file.path(pathdata, "test", "y_test.txt"),header = F)

        sub_test <- read.table(file.path(pathdata, "test", "subject_test.txt"),header=F)

#### Read features data
        features <- read.table(file.path(pathdata, "features.txt"),header = F)

#### Read activity labels
        act_labels <- read.table(file.path(pathdata, "activity_labels.txt"),header = F)

### Create the column names
#### The xtrain column names are from the features or variables table
        colnames(xtrain) <- features[,2]

#### The ytrain column names are the activity ids
        colnames(ytrain) <- "activityId"

#### The sub_train column names are the subject ids
        colnames(sub_train) <- "subjectId"

#### The xtest column names are from the features or variables table
        colnames(xtest)<- features[,2]

#### The ytest column names are the activity ids
        colnames(ytest)<- "activityId"

#### The sub_test column names are the subject ids
        colnames(sub_test) <- "subjectId"

#### The act_labels column names are the activity Ids matched with the activity name
        colnames(act_labels) <- c("activityId","activityName")

## 1. Merge the training and the test sets to create one data set.
### Column bind xtrain, ytrain, and sub_train
        m_train <- cbind(ytrain,sub_train,xtrain)

### Column bind xtest, ytest, and sub_test
        m_test <- cbind(ytest,sub_test,xtest)

### Row bind the m_train and m_test dataset to make the d_set dataset

        d_set <- rbind(m_train,m_test)

## 2. Extract only mean and standard deviation measurements from the dataset 

### Read all the current labels
    
        col_names <- colnames(d_set) 

### Create a logical vector by getting activityID, subjectID ,and variables with mean or std in the string

        mean_std_labels <- (grepl("activityId" , col_names) | 

                        grepl("subjectId" , col_names) | 
                            
                        grepl("mean.." , col_names) | 
                            
                        grepl("std.." , col_names))

### Subset data where logical vector is true, which are only the mean and standard deviation measurements

        d_set_mean_std_labels <- d_set[,mean_std_labels == T]
        

## 3. Uses descriptive activity names to name the activities in the data set
### Use merge to add the activity_labels (activity names and  activity Ids) to the subset data created in step 2

        d_set_activity_names <- merge(d_set_mean_std_labels, act_labels, 
                              by='activityId',all.x = T)

### Move activity ID name to first column for better readability

        d_set_activity_names <- d_set_activity_names %>% 
        
                select(activityId,activityName, everything()) 
        

## 4. Appropriately labels the data set with descriptive variable names.
### Clean up the variable names by using gsub
#### Create new vector the has the original variable names
        col_names_desc <- colnames(d_set_activity_names)
        
#### Remove the "()" in all the variables        
        
        col_names_desc <-gsub("[()]", "", col_names_desc)
#### Change the -X, -Y, -Z  to more descriptive _xAxis, _Yaxis, _Zaxis
        
        col_names_desc <-gsub("-X", "_Xaxis", col_names_desc)
        
        col_names_desc <-gsub("-Y", "_Yaxis", col_names_desc) 
        
        col_names_desc <-gsub("-Z", "_Zaxis", col_names_desc) 
#### Change the t and f prefix to time and fourier
        
        col_names_desc <-gsub("^t", "time_", col_names_desc) 
        
        col_names_desc <-gsub("^f", "fourier_", col_names_desc) 
#### Change the -mean, -std to _mean , _sdev for better readability
        
        col_names_desc <-gsub("-mean", "_mean", col_names_desc) 
        
        col_names_desc <-gsub("-std", "_sdev", col_names_desc) 
#### Change BodyBody to Body
        col_names_desc <-gsub("BodyBody", "Body", col_names_desc) 
        
### Assign new descriptive variable names to data set
colnames(d_set_activity_names) <- col_names_desc

## 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
### Group the dataset by activityName and subjectId first
        d_set_2 <- group_by(d_set_activity_names,activityName,subjectId)
### Summarize the grouped data set by calculating the mean for each column
        d_set_2 <- summarise_all(d_set_X,mean)

### Arrange the data set by subjectId and activityId
#### The data set is considered tidy because each variable is saved in its own column, and each observation is saved in its own row.
        d_set_2 <- d_set_X %>%
        select(subjectId,activityId,activityName,everything()) %>%
        arrange(subjectId,activityId)

### Write the tidy data into a text file
        write.table(d_set_2, "tidyDataSet.txt",row.name = F)





