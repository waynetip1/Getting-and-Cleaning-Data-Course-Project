library(dplyr)
library(tidyr)


fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL,destfile = "./data/dataset.zip") # relative file path

unzip(zipfile ="./data/dataset.zip", exdir = "./data")
list.files("./data/UCI HAR Dataset")

pathdata <- file.path("./data/UCI HAR Dataset")

files <- list.files(pathdata, recursive = TRUE)

files

# reading training tables
xtrain <- read.table(file.path(pathdata, "train", "X_train.txt"),header = F)
ytrain <- read.table(file.path(pathdata, "train", "y_train.txt"),header = F)
sub_train <- read.table(file.path(pathdata, "train", "subject_train.txt"),header = F)

# reading the testing tables
xtest <- read.table(file.path(pathdata, "test", "X_test.txt"),header = F)
ytest <- read.table(file.path(pathdata, "test", "y_test.txt"),header = F)
sub_test <- read.table(file.path(pathdata, "test", "subject_test.txt"),header = F)

# read features data
features <- read.table(file.path(pathdata, "features.txt"),header = F)

# read activity labels
act_labels <- read.table(file.path(pathdata, "activity_labels.txt"),header = F)

# create the column names
colnames(xtrain) <- features[,2]
colnames(ytrain) <- "activityId"
colnames(sub_train) <- "subjectId"
colnames(xtest)<- features[,2]
colnames(ytest)<- "activityId"
colnames(sub_test) <- "subjectId"
colnames(act_labels) <- c("activityId","activityName")

# 1. Merges the training and the test sets to create one data set.
m_train <- cbind(ytrain,sub_train,xtrain)
m_test <- cbind(ytest,sub_test,xtest)
d_set <- rbind(m_train,m_test)

# 2. Extracts only the measurements on the mean and standard deviation for 
#    each measurement.
col_names <- colnames(d_set) # read all the current labels
# creates a logical vector
mean_std_labels <- (grepl("activityId" , col_names) | 
                            grepl("subjectId" , col_names) | 
                            grepl("mean.." , col_names) | 
                            grepl("std.." , col_names))
# subset data where logical vector is true
d_set_mean_std_labels <- d_set[,mean_std_labels == T]


# 3. Uses descriptive activity names to name the activities in the data set
d_set_activity_names <- merge(d_set_mean_std_labels, act_labels, 
                              by='activityId',all.x = T)

d_set_activity_names <- d_set_activity_names %>% 
        select(activityId,activityName, everything()) # move activity ID name to first column

# 4. Appropriately labels the data set with descriptive variable names.
col_names_desc <- colnames(d_set_activity_names)
col_names_desc <-gsub("[()]", "", col_names_desc) # removes ()
col_names_desc <-gsub("-X", "_Xaxis", col_names_desc) # add _Xaxis
col_names_desc <-gsub("-Y", "_Yaxis", col_names_desc) # add _Yaxis
col_names_desc <-gsub("-Z", "_Zaxis", col_names_desc) # add _Zaxis
col_names_desc <-gsub("^t", "time_", col_names_desc) # add time
col_names_desc <-gsub("^f", "fourier_", col_names_desc) # add Fast_Fourier
col_names_desc <-gsub("-mean", "_mean", col_names_desc) # add "_" to mean
col_names_desc <-gsub("-std", "_sdev", col_names_desc) # changes -std to _sdev
col_names_desc <-gsub("BodyBody", "Body", col_names_desc) # removes Body Body
# assign new variable names to data set
colnames(d_set_activity_names) <- col_names_desc
# 5. From the data set in step 4, create a second, independent tidy data 
#    set with the average of each variable for each activity and each subject.
d_set_2 <- group_by(d_set_activity_names,activityName,subjectId)
d_set_2 <- summarise_all(d_set_2,mean)
d_set_2 <- d_set_2 %>%
        select(subjectId,activityId,activityName,everything()) %>%
        arrange(subjectId,activityId)


# write the file

write.table(d_set_2, "tidyDataSet.txt",row.name = F)
