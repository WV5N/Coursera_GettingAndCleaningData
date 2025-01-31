---
title: "README"
author: "Vern"
date: "1/19/2020"
output:
  html_document:
    keep_md: yes
---


# Peer-graded Assignment: Getting and Cleaning Data Course Project

This repository is the submission for the Getting and Cleaning Data Course Project. It has instructions on how to get and clean the Human Activity Recognition Using Smartphones Data Set, from UCI.

# Dataset

#### [Human Activity Recognition Using Smartphones Data Set](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

# Files

1. CodeBook.md: a code book that describes what I did to get and clean the data.
2. run_analysis.R: prepares the data, following the assignment instructions.
   * Merges the training and the test sets to create one data set.
   * Extracts only the measurements on the mean and standard deviation for each measurement.
   * Uses descriptive activity names to name the activities in the data set.
   * Appropriately labels the data set with descriptive variable names.
   * From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
3. tidy_data_set.txt: the final output data after everything above has been completed.
