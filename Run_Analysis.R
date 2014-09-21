#set the working directory |Directorio de trabajo###
#setwd("~/data/UCI HAR Dataset")
#####Data Manipulation########
#TRAINING DATA
#read in the training data into R
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

#========================================================================#

#TEST DATA
#read in the test data into R
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

#========================================================================#
#========================================================================#
#========================================================================#

#1. Merge the training and the test sets to create one data set.
#================================================================#
#merge the training and test X, y, subject data together

X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)

#concatenate the X, y, and subject data into a data frame
data <- data.frame(subject, y, X)
#===========================================================================#
#2. Extract only the measurements on the mean and standard deviation for
#each measurement.
#4. Appropriately label the data set with descriptive variable names.

#read in the features.txt into R that contains the 561 feature names
features <- read.table("./UCI HAR Dataset/features.txt")
class(features[,1]) #integer
class(features[,2]) #factor
features[,2] <- as.character(features[,2])
class(features[,2]) #character

#replace the '-', ',', '(', ')', characters in the features character
#vector with '_', "", "", and "" respectively to create valid R variable
#names
features[,2] <- gsub("-", "_", features[,2])
features[,2] <- gsub(",", "", features[,2])
features[,2] <- gsub(")", "", features[,2])
features[,2] <- gsub("\\(", "", features[,2])

#initialize a character vector to store the features involving mean and
#standard deviation data for subsetting, processing, and data frame
#naming later on
featureNames <- vector(mode="character")

#initialize a numeric vector to store the index of the features involving
#mean and standard deviation for data processing later on
featureColumns <- vector(mode="numeric")

#extract the names and column indexes of the measurements of means and
#standard deviations for creating a new data frame
i <- 0
for (name in features[,2]) {
        i <- i + 1
        print(i)
        meanMatch = grepl("mean", name)
        sdMatch = grepl("std", name)
        if (meanMatch || sdMatch) {
                featureNames <- append(featureNames, name)
                featureColumns <- append(featureColumns, i)
        }
}

#create a new data frame with subjectID, activity numbers and the columns
#pertaining to measurements of the mean and standard deviation
dataset <- cbind(subject, y, X[, featureColumns])

#give names to the columns in the dataset data frame
names(dataset) <- c("subjectID", "activity", featureNames)

#3. Uses descriptive activity names to name the activities in the data set

#read in the activity_labels.txt into R that contains the numeric labels
#for the 6 activities
activityFile <- read.table("./UCI HAR Dataset/activity_labels.txt")
activity <- activityFile
class(activity[,1]) #integer
class(activity[,2]) #factor
activity[,2] <- as.character(activity[,2])
class(activity[,2]) #character

#replace the integers 1, 2, 3, 4, 5, 6 in the y column of data with
#the corresponding activity label names
for (i in seq(activity[,1])) {
        y[,1] <- gsub(activity[i,1], activity[i,2], y[,1])   
}

#modify the dataset data frame with the updated y column containing
#name-based labels
dataset[,2] <- y

# 5. Creates a second, independent tidy data set with the average of each
#variable for each activity and each subject. 

#initialize the columns of the tidy data frame
subjectID <- rep(1:30, 1, each=6)
activityVector <- rep(activity[,2], 30)
measurement <- matrix(rep(0, (ncol(dataset)-2)*180), nrow=180)
tidy <- data.frame(subjectID, activityVector, measurement)

#name the columns of the tidy data frame
names(tidy) <- c("subjectID", "activity", featureNames)

#populate the tidy data set with the average of each variable for each
#activity and each subject. 
for (row in seq(subjectID)) {
        subject <- subjectID[row]
        #print(paste(row, subject, sep=": "))
        for (act in activity[,2]) {
                measurementColumns <- dataset[dataset$subjectID==subject & dataset$activity==act, 3:ncol(dataset)]
                currentMeasurementMean <- apply(measurementColumns, 2, mean)
                tidy[row,3:ncol(tidy)] <- currentMeasurementMean
        }
}
#==OUTPUT Writing++++# Exportando Archivos en CSV and TXT#####
#Save the tidy data frame containing the aggregate data in a .txt file
write.table(tidy, file='tidyDataTXT.txt')

#Additionally, save the tidy data frame containing the aggregate data in
#a .csv file
write.csv(tidy, file='tidyDataCSV.txt')

#Save the featureNames vetor containing the features names in our tidy
#data into a .txt file. Then, we can conveniently copy these names from
#the .txt file into our code book
write.table(featureNames, file='featureNames.txt')
####END#######
