#Course Project for Getting and Cleaning Data# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


library(plyr)

#Load in the data
x_train <- read.table("x_train.txt")
x_test <- read.table("x_test.txt")
y_train <- read.table("y_train.txt")
y_test <- read.table("y_test.txt")
features <- read.table("features.txt")
subject_test <- read.table("subject_test.txt")
subject_train <- read.table("subject_train.txt")
activity <- read.table("activity_labels.txt")

#merge the test and training sets
x_set <- rbind(x_train, x_test)
y_set <- rbind(y_train, y_test)
subject_set <- rbind(subject_test, subject_train)

#extract just columns with feature names "mean()" and "std()"
extracted <- grep("-(mean|std)\\(\\)",features[,2])

#subset the columns of the merged data with the new variable created above
x_set_limited <- x_set[,extracted]

#Use descriptive activity names to name the activities in the data set
names(x_set_limited) <- features[extracted,2]
y_set[,1] <- activity[y_set[,1],2]
names(y_set) <- "activity" #correcting column name

#Label data set with descriptive variable names
# Change t to Time, f to Frequency, mean() to Mean and std() to StdDev
# Remove extra dashes and BodyBody naming error from original feature names
names(subject_set) <- "subject"
names(x_set_limited) <- gsub("^t", "Time", names(x_set_limited))
names(x_set_limited) <- gsub("^f", "Frequency", names(x_set_limited))
names(x_set_limited) <- gsub("-mean\\(\\)", "Mean", names(x_set_limited))
names(x_set_limited) <- gsub("-std\\(\\)", "StdDev", names(x_set_limited))
names(x_set_limited) <- gsub("-", "", names(x_set_limited))
names(x_set_limited) <- gsub("BodyBody", "Body", names(x_set_limited))

#merge all three sets (x, y, and subject) together
complete_set <- cbind(x_set_limited, y_set, subject_set)

# Create a second, independent tidy data set with the average of each variable for each activity and each subject
# mean for all but activity and subject
tidy <- ddply(complete_set, .(subject,activity), function(whatever) colMeans(whatever[,1:66]))

#write final table
write.table(tidy, "averages_data.txt", row.names=FALSE)
