#  List of Actions in run_analysis.R ................................................
# IMPORTANT: Before sourcing run_analysis.R load "plyr" "knit" libraries
# 1.  Applying directories and files
# 1.1 Loading raw data...............................................................
# 2.  Merging training and test sets to create one data set
# 2.1 Binding sensor data............................................................
# 2.2 Labeling columns...............................................................
# 3.  Extracting only the measurements on the mean 
##    and standard deviation for each measurement
# 4.  Using descriptive activity names to name the activities in the data set
# 5.  Appropriately labeling the data set with descriptive names
# 5.1 Removing parenthesis...........................................................
# 5.2 Applying syntactically valid names.............................................
# 5.3 Applying reader friendly names.................................................
# 6.  Creating a second a organized data set 
#     with the means of each variable for activities and subjects and writing to a
#     new file UCITidyData.txt
#
# Thanks to the useful information found in the links below:
#
# https://rstudio-pubs-static.s3.amazonaws.com/37290_8e5a126a3a044b95881ae8df530da583.html
# https://bitbucket.org/maurotrb/getting-cleaning-data-2014-project/src
# https://github.com/yihui/knitr/tree/master/vignettes
# 

#......................................................................................
# 1. Applying directories and files
#......................................................................................

uciHarDir <- "UCI\ HAR\ Dataset"

featureFile <- paste(uciHarDir, "/features.txt", sep = "")

activityLabelsFile <- paste(uciHarDir, "/activity_labels.txt", sep = "")

xTrainFile <- paste(uciHarDir, "/train/X_train.txt", sep = "")

yTrainFile <- paste(uciHarDir, "/train/y_train.txt", sep = "")

subjectTrainFile <- paste(uciHarDir, "/train/subject_train.txt", sep = "")

xTestFile  <- paste(uciHarDir, "/test/X_test.txt", sep = "")

yTestFile  <- paste(uciHarDir, "/test/y_test.txt", sep = "")

subjectTestFile <- paste(uciHarDir, "/test/subject_test.txt", sep = "")

# 1.1 Loading raw data.................................................................

features <- read.table(featureFile, colClasses = c("character"))

activityLabels <- read.table(activityLabelsFile, col.names = c("ActivityId", "Activity"))

xTrain <- read.table(xTrainFile)

yTrain <- read.table(yTrainFile)

subject_train <- read.table(subjectTrainFile)

xTest <- read.table(xTestFile)

yTest <- read.table(yTestFile)

subjectTest <- read.table(subjectTestFile)

#......................................................................................
# 2. Merges the training and the test sets to create one data set
#......................................................................................

# 2.1 Binding sensor data..............................................................

trainingSensorData <- cbind(cbind(xTrain, subject_train), yTrain)

testSensorData <- cbind(cbind(xTest, subjectTest), yTest)

sensorData <- rbind(trainingSensorData, testSensorData)

# 2.2 Labeling columns...............................................................

sensorLabels <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]

names(sensorData) <- sensorLabels

#....................................................................................
# 3. Extracting only the measurements on the mean 
## and standard deviation for each measurement
#....................................................................................

sensorDataMeanStd <- sensorData[,grepl("mean|std|Subject|ActivityId", names(sensorData))]

#....................................................................................
# 4. Using descriptive activity names to name the activities in the data set
#....................................................................................

sensorDataMeanStd <- join(sensorDataMeanStd, activityLabels, by = "ActivityId", match = "first")

sensorDataMeanStd <- sensorDataMeanStd[,-1]


#....................................................................................
# 5. Appropriately labeling the data set with descriptive names
#....................................................................................

# 5.1 Removing parenthesis.............................................................

names(sensorDataMeanStd) <- gsub('\\(|\\)',"",names(sensorDataMeanStd), perl = TRUE)

# 5.2 Applying syntactically valid names.................................................

names(sensorDataMeanStd) <- make.names(names(sensorDataMeanStd))

# 5.3 Applying reader friendly names.................................................

names(sensorDataMeanStd) <- gsub('Acc',"Acceleration",names(sensorDataMeanStd))

names(sensorDataMeanStd) <- gsub('GyroJerk',"AngularAcceleration",names(sensorDataMeanStd))

names(sensorDataMeanStd) <- gsub('Gyro',"AngularSpeed",names(sensorDataMeanStd))

names(sensorDataMeanStd) <- gsub('Mag',"Magnitude",names(sensorDataMeanStd))

names(sensorDataMeanStd) <- gsub('^t',"TimeDomain.",names(sensorDataMeanStd))

names(sensorDataMeanStd) <- gsub('^f',"FrequencyDomain.",names(sensorDataMeanStd))

names(sensorDataMeanStd) <- gsub('\\.mean',".Mean",names(sensorDataMeanStd))

names(sensorDataMeanStd) <- gsub('\\.std',".StandardDeviation",names(sensorDataMeanStd))

names(sensorDataMeanStd) <- gsub('Freq\\.',"Frequency.",names(sensorDataMeanStd))

names(sensorDataMeanStd) <- gsub('Freq$',"Frequency",names(sensorDataMeanStd))

#.....................................................................................
# 6. Creating a second a organized data set with the means
#  of each variable for activities and subjects and writing to a
#  new file UCITidyData.txt
#.....................................................................................

sensorAvgActSub = ddply(sensorDataMeanStd, c("Subject","Activity"), numcolwise(mean))

write.table(sensorAvgActSub, file = "UCITidyData.txt")

# End 