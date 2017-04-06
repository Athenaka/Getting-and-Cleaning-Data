

runAnalysis <- function() {
  # Please make sure that the data set is located in your working directory
  
  # Load base files
  # Load activity labels
  activitylabels <- read.table("UCI HAR Dataset/activity_labels.txt")
  activitylabels[,2] <- as.character(activitylabels[,2])
  
  # Load Features
  features <- read.table("UCI HAR Dataset/features.txt")
  features[,2] <- as.character(features[,2])
  
  # Extract the mean and standard deviation data
  featuresneeded <- grep(".*mean.*|.*std.*", features[,2])
  featuresneeded.names <- features[featuresneeded,2]
  featuresneeded.names = gsub('-mean', 'Mean', featuresneeded.names)
  featuresneeded.names = gsub('-std', 'Std', featuresneeded.names)
  featuresneeded.names <- gsub('[-()]', '', featuresneeded.names)
  
  
  # Read the  datasets
  # Read the training data
  train <- read.table("UCI HAR Dataset/train/X_train.txt")[featuresneeded]
  trainActivities <- read.table("UCI HAR Dataset/train/Y_train.txt")
  trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
  train <- cbind(trainSubjects, trainActivities, train)
  
  # Read the test data
  test <- read.table("UCI HAR Dataset/test/X_test.txt")[featuresneeded]
  testActivities <- read.table("UCI HAR Dataset/test/Y_test.txt")
  testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
  test <- cbind(testSubjects, testActivities, test)
  
  # merge datasets 
  allData <- rbind(train, test)
  colnames(allData) <- c("subject", "activity", featuresneeded.names)
  
  # turn activities & subjects into factors
  allData$activity <- factor(allData$activity, levels = activitylabels[,1], labels = activitylabels[,2])
  allData$subject <- as.factor(allData$subject)
  
  allData.melted <- melt(allData, id = c("subject", "activity"))
  allData.mean <- dcast(allData.melted, subject + activity ~ variable, mean)
  
  # Writing the tidy output file
  write.table(allData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
}