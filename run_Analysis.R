
# Coursera JHU Getting & Cleaning Data Project - Script

#-Downloads and unzip data collected from the accelerometers from the Samsung Galaxy S smartphone
if(!file.exists("data")) {  dir.create("data") }
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./wear.zip")
unzip("./wear.zip",exdir="./data")

#-Build R Data Sets for Train and Test
  #-Loads all the data files (activity, subject, data set) into R
  #-Build Data Along the Columns (activity, subject, data set) 
  #-Add Features Names to Each Data Set
  #-Merges Train and Test Data into One Combined Data Set

# TRAIN
# get activity labels
activity_labels <- read.table("./activity_labels.txt")

# get train labels AND match to activities in activity_labels file
train_labels <- read.table("./train/Y_train.txt")
install.packages("plyr")
library(plyr)
train_labels_v<-as.vector(t(train_labels))
train_labels_v_act<-mapvalues(train_labels_v, c("1","2","3","4","5","6")
                  , c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

# get train subjects
train_subject <- read.table("./train/subject_train.txt")

# get train data
train_data <- read.table("./train/X_train.txt")

# merge train columns
train_data<-cbind(train_labels_v_act,train_subject,train_data)

# TEST
# get test labels
test_labels <- read.table("./test/Y_test.txt")

# get train labels - match to activities in activity_labels file
test_labels_v<-as.vector(t(test_labels))
test_labels_v_act<-mapvalues(test_labels_v, c("1","2","3","4","5","6")
                , c("WALKING","WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING","LAYING"))

# get test subjects
test_subject <- read.table("./test/subject_test.txt")

# get test data
test_data <- read.table("./test/X_test.txt")

# merge test columns
test_data<-cbind(test_labels_v_act,test_subject,test_data)

# get features names and transpose so can add to merged train test data
features <- read.table("./features.txt")  
features_t <- t(features)

# get just features names and add extra names for activity and subject
features_names <- features_t[2,]
features_names2 <- c("activity","subject",features_names)

# give train_data and test_data real names with above activity/subject/Features vector
names(train_data) <- features_names2
names(test_data) <- features_names2

# merge train and test data
traintest_data<-rbind(train_data,test_data)

#-Find Features Names with Mean or STD in them
features_t_ms_2 <- features_t[2,grep('(mean|std)',features_t[2,])] 

#get columns for about feature names and when FALSE make NA and omit 
#1st two are activity and subject so hard coded 1 and 2
col <- names(traintest_data) %in% c(features_t_ms_2) 
x=1
for (i in 1:563){
  if(i==1) {x[i]=1}
  else if(i==2) {x[i]=i}
  else if(col[i]==TRUE) {x[i]=i}
  else{x[i]=NA}
  }
good<-complete.cases(x)  
y<-x[good] # columns with mean or std

#-Subset Data Set based on Mean and STD columns
traintest_data_ms<-traintest_data[,y]

#-Create new independent tidy data set with the average of each variable for each activity and each subject
install.packages("dplyr")
library(dplyr)
traintest_data_ms_avgPactsub<-ddply(traintest_data_ms
                          , c("traintest_data_ms$activity","traintest_data_ms$subject")
                          , numcolwise(mean)
)

#-Output new tidy set above in TXT format
?write.table
write.csv(traintest_data_ms_avgPactsub, file = "traintest_data_ms_avgPactsub.txt", row.names = FALSE)


