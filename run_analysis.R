# Source of data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# This R script does the following:
# 1. Merges the training and the test sets to create one data set.

temp_1 <- read.table("train/X_train.txt")
temp_2 <- read.table("test/X_test.txt")
X <- rbind(temp_1, temp_2)

temp_1 <- read.table("train/y_train.txt")
temp_2 <- read.table("test/y_test.txt")
Y <- rbind(temp_1, temp_2)

temp_1 <- read.table("train/subject_train.txt")
temp_2 <- read.table("test/subject_test.txt")
Subjects <- rbind(temp_1, temp_2)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
features <- read.table("features.txt")
extract_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, extract_features]
names(X) <- features[extract_features, 2]
names(X) <- gsub("\\(|\\)", "", tolower(names(X)))


# 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table("activity_labels.txt")
activity_labels[, 2] = gsub("_", "", tolower(activity_labels[, 2]))
Y[,1] = activity_labels[Y[,1], 2]
names(Y) <- "activity"


# 4. Appropriately labels the data set with descriptive activity names.

names(Subjects) <- "subject"
merged_data <- cbind(Subjects, Y, X)

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

unique_Subjects = unique(Subjects)[,1]
num_Subjects = length(unique(Subjects)[,1])
num_Activities = length(activity_labels[,1])
num_Cols = dim(merged_data)[2]
tidy_data = merged_data[1:(num_Subjects*num_Activities), ]

row_count = 1
for (s in 1:num_Subjects) {
	for (a in 1:num_Activities) {
		tidy_data[row_count, 1] = unique_Subjects[s]
		tidy_data[row_count, 2] = activity_labels[a, 2]
		temp <- merged_data[merged_data$subject==s & merged_data$activity==activity_labels[a, 2], ]
		tidy_data[row_count, 3:num_Cols] <- colMeans(temp[, 3:num_Cols])
		row_count = row_count+1
	}
}
write.table(tidy_data, file = "./tidy_data.txt", )
