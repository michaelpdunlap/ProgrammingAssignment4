
##requisite libraries
library(tidyr)
library(dplyr)

##download & unzip files
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp <- tempfile()
download.file(fileURL, temp, method="curl")
unzip(temp)

##extract file into constituent components

##all activity labels
activityLabel <- read.csv("./UCI HAR Dataset/activity_labels.txt", sep = " ", header = FALSE,
                          col.names = c("Activity_ID","Activity_Desc"), stringsAsFactors = FALSE)
##features.txt = feature labels for all
featureLabel <- read.csv("./UCI HAR Dataset/features.txt", sep = " ", header = FALSE,
                         col.names = c("Feature_ID","Feature_Desc"), stringsAsFactors = FALSE)

##test files - these need joined
##subject_test.txt = subject ID for test
subjectTestID <- read.csv("./UCI HAR Dataset/test/subject_test.txt", sep = " ", header = FALSE, 
                          col.names = c("Subject_ID"))
subjectTestID$Subject_ID <- as.character(subjectTestID$Subject_ID)

##y_test.txt = activity label for test
activityTestID <- read.csv("./UCI HAR Dataset/test/y_test.txt", sep = " ", header = FALSE,
                           col.names = c("Activity_ID"))


##subject_train.txt = subject ID for train
subjectTrainID <- read.csv("./UCI HAR Dataset/train/subject_train.txt", sep = " ", header = FALSE, 
                           col.names = c("Subject_ID"))
subjectTrainID$Subject_ID <- as.character(subjectTrainID$Subject_ID)
##y_train.txt = activity label for train
activityTrainID <- read.csv("./UCI HAR Dataset/train/y_train.txt", sep = " ", header = FALSE,
                            col.names = c("Activity_ID"))


##function to merge activity name

##load and tidy up Test dataset
##x_test.txt = feature data for test
featureTest <- tbl_df(read.csv("./UCI HAR Dataset/test/x_test.txt", sep = "", header = FALSE,
                        ##col.names = featureLabel$Feature_Desc
                        ))
colnames(featureTest) <- featureLabel$Feature_Desc

#remove column duplicate names (not needed)
featureTest <- featureTest[,!duplicated(colnames(featureTest))]


featureTest <- featureTest %>%
  mutate(Activity_ID = activityTestID$Activity_ID ) %>%
  mutate(Subject_ID = subjectTestID$Subject_ID) %>%
  left_join(activityLabel, by = "Activity_ID") %>%
  # select columns we care about: subject, activity, mean, stdev
  select (Subject_ID, Activity_Desc, contains("mean"), contains("std"))

##load and tidy up Train dataset  
##x_train.txt = feature data for train
featureTrain <- tbl_df(read.csv("./UCI HAR Dataset/train/x_train.txt", sep = "", header = FALSE,
                         ##col.names = featureLabel$Feature_Desc
                         ))

colnames(featureTrain) <- featureLabel$Feature_Desc

#remove column duplicate names (not needed)
featureTrain <- featureTrain[,!duplicated(colnames(featureTrain))]

featureTrain <- featureTrain %>%
  ##rename(featureLabel$Feature_Desc) %>%
  mutate(Activity_ID = activityTrainID$Activity_ID ) %>%
  mutate(Subject_ID = subjectTrainID$Subject_ID) %>%
  left_join(activityLabel, by = "Activity_ID") %>%
  # select columns we care about: subject, activity, mean, stdev
  select (Subject_ID, Activity_Desc, contains("mean"), contains("std"))
  
##append rows - test + train data here
AllFeatureData <- bind_rows(featureTest, featureTrain, id = NULL)

##create second data set that has average of each variable for each activity
## for each subject
featureSummary <- AllFeatureData %>%
  group_by(Subject_ID, Activity_Desc) %>%
  summarize_if(is.numeric, mean)

##write datasets to file
write.csv(AllFeatureData, "AllFeatureData.csv")
write.csv(featureSummary, "FeatureSummary.csv")




