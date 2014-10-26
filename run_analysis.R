#reading the data from files
features <- read.table("./features.txt", header = FALSE)
activityType <- read.table("./activity_labels.txt", header = FALSE)

#reading the data for test and setting the columns' names
subjectTest <- read.table("./test/subject_test.txt", header = FALSE)
xTest <- read.table("./test/X_test.txt", header = FALSE)
yTest <- read.table("./test/Y_test.txt", header = FALSE)

colnames(activityType) <- c("activityId", "activityType")

colnames(subjectTest) <- "subjectId"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"
testData <- cbind(yTest, subjectTest, xTest)

#reading the data for train and setting the columns' names
subjectTrain <- read.table("./train/subject_train.txt", header = FALSE)
xTrain <- read.table("./train/X_train.txt", header = FALSE)
yTrain <- read.table("./train/Y_train.txt", header = FALSE)

colnames(subjectTrain) <- "subjectId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"
trainData <- cbind(yTrain, subjectTrain, xTrain)

#combine the test and train data to one table
finalData <- rbind(trainData, testData)
colNames <- colnames(finalData)
extractVector <- (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("mean\\(\\)", colNames) | grepl("std\\(\\)", colNames))
finalData <- finalData[extractVector == TRUE]

finalData <- merge(finalData, activityType, by = "activityId", all.x = TRUE)

#resetting the columns' names
for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

colnames(finalData) = colNames;

#getting the tidtdata
tidyData <- aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean);

tidyData <- merge(tidyData, activityType, by = "activityId", all.x = TRUE)

#write the tidydata to a txt file
write.table(tidyData, "tidyData.txt", row.names = TRUE, sep = "\t")

