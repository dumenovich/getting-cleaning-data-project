# runAnalysis.r File Description:

# This script will perform the following steps on the UCI HAR Dataset downloaded from 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 


DirUCIName <- "UCI HAR Dataset"
FileDataName <- "getdata_dataset.zip"

# Download and unzip the dataset
if (!file.exists(DirUCIName)) { 
  if (!file.exists(FileDataName)){
    fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, FileDataName, method="curl")
  }  
  unzip(filename) 
}

# List of all features
features <- read.table(file.path(DirUCIName,'features.txt'),header=FALSE) 
# Activity name
activity <- read.table(file.path(DirUCIName,'activity_labels.txt'),header=FALSE)

# Read train data
# Subject train
subjectTrain <- read.table(file.path(DirUCIName,'train/subject_train.txt'),header=FALSE)
# Training set
xTrain <- read.table(file.path(DirUCIName,'train/x_train.txt'),header=FALSE)
# Training labels
yTrain <- read.table(file.path(DirUCIName,'train/y_train.txt'),header=FALSE)

# Read test data
# Subject train
subjectTest <- read.table(file.path(DirUCIName,'test/subject_test.txt'),header=FALSE)
# Test set
xTest <- read.table(file.path(DirUCIName,'test/x_test.txt'),header=FALSE)
# Test labels
yTest <- read.table(file.path(DirUCIName,'test/y_test.txt'),header=FALSE)


# Assigin column names to the data imported above
colnames(activity) <- c('id_activity','activity_type')
colnames(subjectTrain) <- colnames(subjectTest) <- 'id'
colnames(yTrain) <- colnames(yTest) <- 'id_activity'
colnames(xTrain) <- colnames(xTest) <- features[,2] 

# Merge ..
trainingData <- cbind(subjectTrain, yTrain, xTrain)
testData <- cbind(subjectTest, yTest, xTest)

# Combine .. 
finalData <- rbind(trainingData,testData)

ColumnNames <- colnames(finalData)
ColumnNames_bool <- (grepl("id_activity",ColumnNames) | grepl("id",ColumnNames) | grepl("-mean..",ColumnNames) & !grepl("-meanFreq..",ColumnNames) & !grepl("mean..-",ColumnNames) | grepl("-std..",ColumnNames) & !grepl("-std()..-",ColumnNames));

# Subset from finalData Mean and Std
finalData <- finalData[ColumnNames_bool==TRUE]
finalData <- merge(finalData,activity,by='id_activity',all.x=TRUE)

ColumnNames <- colnames(finalData)

# Cleaning up the variable names
for (i in 1:length(ColumnNames)) 
{
  ColumnNames[i] = gsub("\\()","",ColumnNames[i])
  ColumnNames[i] = gsub("-std$","StdDev",ColumnNames[i])
  ColumnNames[i] = gsub("-mean","Mean",ColumnNames[i])
  ColumnNames[i] = gsub("^(t)","time",ColumnNames[i])
  ColumnNames[i] = gsub("^(f)","freq",ColumnNames[i])
  ColumnNames[i] = gsub("([Gg]ravity)","Gravity",ColumnNames[i])
  ColumnNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",ColumnNames[i])
  ColumnNames[i] = gsub("[Gg]yro","Gyro",ColumnNames[i])
  ColumnNames[i] = gsub("AccMag","AccMagnitude",ColumnNames[i])
  ColumnNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",ColumnNames[i])
  ColumnNames[i] = gsub("JerkMag","JerkMagnitude",ColumnNames[i])
  ColumnNames[i] = gsub("GyroMag","GyroMagnitude",ColumnNames[i])
}

colnames(finalData) <- ColumnNames

# Create a second, independent tidy dataset with the average of each variable for each activity and each subject. 
tidyData <- finalData[,names(finalData) != 'activity_type']
tidyData <- aggregate(tidyData[,names(tidyData) != c('id_activity','id')],by=list(id_activity=tidyData$id_activity, id = tidyData$id),mean)
tidyData <- merge(tidyData, activity, by='id_activity',all.x=TRUE)

# Export the tidyData set 
write.table(tidyData, file.path(DirUCIName,'../tidyData.txt'),row.names=TRUE,sep='\t')

