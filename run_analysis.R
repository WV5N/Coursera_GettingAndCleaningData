#Module 3 - Getting and Cleaning Data Course Project

    
# 0. Download and extract the data. Not part of the assignment, but helpful to have here anyway.
    
# Check to see if the "data.table" package is installed, since this script relies on it.
if(!library(data.table, logical.return = TRUE)) {
    # It didn't exist. Install the package, then load it
    install.packages('data.table')
    library(data.table)
}

# Check to see if the "plyr" package is installed, since this script relies on it.
if(!library(plyr, logical.return = TRUE)) {
    # It didn't exist. Install the package, then load it
    install.packages('plyr')
    library(plyr)
}

# Check to see if the "dplyr" package is installed, since this script relies on it.
if(!library(dplyr, logical.return = TRUE)) {
    # It didn't exist. Install the package, then load it
    install.packages('dplyr')
    library(dplyr)
}



# Check to see if a subdirectory exists for this project. If not, create it.
if(!file.exists("./courseprojectdata")){
    dir.create("./courseprojectdata")
}

# Define the zip file name on the UCI server.
fileUrl1 = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

# Define the destination filename. Does not match the above filename from UCI.
destfilename = "ucidata.zip"

# Define the full path for the zip file.
destfilepathfull = file.path("./courseprojectdata", destfilename)

# Download the zip file from the UCI repository, and save it to the project subdirectory.
download.file(fileUrl1,destfile = destfilepathfull, method = "curl")



# Define the directory for where the top level extracted files should go.
outDir <- file.path("./courseprojectdata", "ucidata")

# Define the directory where the extracted project files will be automatically placed. The folder in the zipped file is already named "UCI HAR Dataset".
finalDirName <- file.path(outDir, "UCI HAR Dataset")

# Unzip the file, if it doesn't already exist.
if (!file.exists(finalDirName)) { 
    unzip(destfilepathfull, exdir=outDir)
}



# 1. Merges the training and the test sets to create one data set.

## Column labels for use later

# Reads the Feature Names into the variable 'featureNames'. There are 561 features.
colnames_origorder <- c("index","featurename")
featureNames <- read.table(file.path(finalDirName, "features.txt"), col.names = colnames_origorder)

# This shows the duplicated column names, which needs to be corrected.
namecount <- ddply(featureNames,.(featurename),nrow)
namecount[namecount$V1 > 1,]


# Adding a 3rd column with a combination of featurename and index, since there are 42 triplicate column names in this dataset. Duplicated columns names won't work with dplyr select for column names, which is needed later on.
# This extra information will be removed towards the end of this process, and the separater/delimiter will be used in that process.
colnames_neworder <- c("featurename", "index")
featureNames$newname <- do.call(paste, c(featureNames[colnames_neworder], sep="_"))


# Reads the Activities into the variable 'activityLabels'. There are 6 activity labels.
activityLabels <- read.table(file.path(finalDirName, "activity_labels.txt"), col.names = c("activitycodenum", "activityname"))



## Test Data

# Read the Test data into tables.
test_feature_data <- read.table(file.path(finalDirName, 'test', 'X_test.txt'))
test_activity_data <- read.table(file.path(finalDirName, 'test', 'y_test.txt'))
test_subject_data <- read.table(file.path(finalDirName, 'test', 'subject_test.txt'))

# Rename the columns in the test_feature_data table with the feature names from the 'newname' column in the featureNames table, and transform that column into 561 columns, so that they match up correctly.
colnames(test_feature_data) <- t(featureNames$newname)

# Add a source column so that I can keep track of where the data came from: test vs train. This step is not required for this project.
test_feature_data <- cbind("test", test_feature_data)
setnames(test_feature_data, '"test"', "datasource")

# Rename the single column in each of the tables for activity and subject.
colnames(test_activity_data) <- "activity"
colnames(test_subject_data) <- "subject"

# Merge (by columns) all 3 tables for Test. All tables have the same number of observations (10299).
merged_test_data <- cbind(test_feature_data, test_activity_data, test_subject_data)



## Train Data

# Read the Train data into tables.
train_feature_data <- read.table(file.path(finalDirName, 'train', 'X_train.txt'))
train_activity_data <- read.table(file.path(finalDirName, 'train', 'y_train.txt'))
train_subject_data <- read.table(file.path(finalDirName, 'train', 'subject_train.txt'))

# Rename the columns in the test_feature_data table with the feature names from the 'newname' column in the featureNames table, and transform that column into 561 columns, so that they match up correctly.
colnames(train_feature_data) <- t(featureNames$newname)

# Add a source column so that I can keep track of where the data came from: test vs train. This step is not required for this project.
train_feature_data <- cbind("train", train_feature_data)
setnames(train_feature_data, '"train"', "datasource")

# Rename the single column in each of the tables for activity and subject.
colnames(train_activity_data) <- "activity"
colnames(train_subject_data) <- "subject"

# Merge (by columns) all 3 tables for Train. All tables have the same number of observations (10299).
merged_train_data <- cbind(train_feature_data, train_activity_data, train_subject_data)



## Tidy Data table

# Merge (by Rows) the Test data and the Train data.
merged_all_data <- rbind(merged_test_data, merged_train_data)








# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

tidy_data <- merged_all_data %>% select(datasource, subject, activity, contains("mean"), contains("std"))







# 3. Uses descriptive activity names to name the activities in the data set

# Update the Activity field to be character type, to be able to hold the descriptive name. Originally numeric, which can't hold string data.
tidy_data$activity <- as.character(tidy_data$activity)

# Update the Activity field to be descriptive names, looking up the names in the activityLabels table.
tidy_data$activity <- activityLabels[tidy_data$activity, 2]






# 4. Appropriately labels the data set with descriptive variable names. 

# Here's how the column names look before renaming
names(tidy_data)


# Make all names lowercase.
# lowercase instructions for column names is per lecture: https://www.coursera.org/learn/data-cleaning/lecture/drpnT/editing-text-variables
names(tidy_data) <- tolower(names(tidy_data))

# Get rid of the field index number which was added towards the top of this script to help differentiate the column names.
names(tidy_data) <- sapply(strsplit(names(tidy_data), "_"), '[', 1)


# Rename the abbreviations in the field names with their longer equivalents
names(tidy_data) <- gsub("acc", "accelerometer", names(tidy_data))
names(tidy_data) <- gsub("gyro", "gyroscope", names(tidy_data))
names(tidy_data) <- gsub("bodybody", "body", names(tidy_data))
names(tidy_data) <- gsub("mag", "magnitude", names(tidy_data))
names(tidy_data) <- gsub("^t", "time", names(tidy_data))
names(tidy_data) <- gsub("\\(tbody", "-timebody", names(tidy_data))
names(tidy_data) <- gsub("^f", "frequency", names(tidy_data))

# Get rid of parentheses and commas
names(tidy_data) <- gsub("\\(\\)", "", names(tidy_data))
names(tidy_data) <- gsub("\\(", "-", names(tidy_data))
names(tidy_data) <- gsub("\\)", "", names(tidy_data))
names(tidy_data) <- gsub("\\,", "-", names(tidy_data))

# These need to be done last, since many had '()' at the end.
names(tidy_data) <- gsub("freq-", "frequency-", names(tidy_data))
names(tidy_data) <- gsub("freq$", "frequency", names(tidy_data))


# Here's how the column names look after renaming
names(tidy_data)







# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# Make the 'subject' column a factor instead of numeric, to be able to aggregate
#tidy_data$subject <- as.factor(tidy_data$subject)

# Drop the 'datasource' column, since it's not needed in the final output
tidy_data <- subset(tidy_data, select = -datasource)


# Aggregate the data into a new, final table.
final_tidy_data <- aggregate(. ~activity + subject, tidy_data, mean)

# Reorder the subject & activity columns
final_tidy_data <- final_tidy_data[order(final_tidy_data$subject,final_tidy_data$activity),]


# Define the final table filename
finaltabledestfilename = "tidy_data_set.txt"

# Define the full path for the zip file.
finaltabledestfilepathfull = file.path("./courseprojectdata", finaltabledestfilename)

# Write the file.
write.table(final_tidy_data, file=file.path(finaltabledestfilepathfull), row.names = FALSE, quote = FALSE)




# 6. Produce Readme and CodeBook

rmarkdown::render("CodeBook.rmd")
rmarkdown::render("README.rmd")

