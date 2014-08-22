

######## INITIALIZATION########### ##################################################################
# Clear Global Environment variables
rm(list=ls())
# Set Working Directory as the location of the extracted Zip File
setwd("C:/Users/Sriram/Downloads/UCI HAR/UCI HAR Dataset")

######## STEP 1 - TEST DATASET #######################################################################
# Read Test Dataset which contains all the measurements 
Test_measurements = read.table("./test/X_test.txt",header=FALSE,sep="")

# Read Subject Dataset which contains the subject who recorded the measurements
Test_subject = read.table("./test/subject_test.txt",header=FALSE,sep="")
#Label the column appropriately
colnames(Test_subject) = "Subject_Code"

# Get Feature Names from Features File
Feature_names = read.table("./features.txt",header=FALSE,sep="")

# Use Feature Names as column names for the Test Dataset
Feature_names$V2 = gsub("\\(","",Feature_names$V2)
Feature_names$V2 = gsub("\\)","",Feature_names$V2)

colnames(Test_measurements) = Feature_names$V2

# Only keep columns with std and mean
Selected_columns = c(c(1:6),c(41:46),c(81:86),c(121:126),c(161:166),c(201:202),c(214:215),c(227:228),c(240:241),c(253:254),c(266:271),c(345:350),c(424:429),c(503:504),c(516:517),c(529:530),c(542:543))
Test_measurements = Test_measurements[,Selected_columns]

# Read Test Class Dataset which contains the activity code for the recorded information
Test_activity_codes = read.table("./test/Y_test.txt",header=FALSE,sep="")
#Label the column appropriately
colnames(Test_activity_codes) = "Activity_Code"

# Activity Label file contains the activity description associated with the activity code
Activity_labels = read.table("./activity_labels.txt",header=FALSE,sep="") 
# .. and label the newly read colummns appropriately
colnames(Activity_labels) = c("Activity_Code","Activity_Description")

# Use this file to expand the Test Activity Code Dataset to add Activity Description as well 
Test_activity = merge(Test_activity_codes, Activity_labels)
# Add a column for FileType (for use in merged file)
Test_activity$Type = "Test"

Test_measurements = cbind(Test_activity, Test_measurements)
Test_measurements = cbind(Test_subject, Test_measurements)

# remove unwanted dataframes from workspace
rm(Test_activity, Test_activity_codes, Test_subject)

######## STEP 2 - TRAINING DATASET ##################################################################
# Read Train Dataset which contains all the measurements
Train_measurements = read.table("./train/X_train.txt",header=FALSE,sep="")

# Read Subject Dataset which contains the subject who recorded the measurements
Train_subject = read.table("./train/subject_train.txt",header=FALSE,sep="")
#Label the column appropriately
colnames(Train_subject) = "Subject_Code"

# Add Feature Names as column names to the Train Dataset
# Note Feature names was already loaded during Test Dataset processing
colnames(Train_measurements) = Feature_names[,2]

# Only keep columns with std and mean
# selected columns were already specified for Test Dataset so leverage same set here as well
Train_measurements = Train_measurements[,Selected_columns]

# Read Train Class Dataset which contains the activity code for the recorded information
Train_activity_codes = read.table("./train/Y_Train.txt",header=FALSE,sep="")
# .. and label the newly read columns appropriately
colnames(Train_activity_codes) = "Activity_Code"

# Use Activity Label created before
Train_activity = merge(Train_activity_codes, Activity_labels)
# Add a column for FileType (for use in merged file)
Train_activity$Type = "Train"

Train_measurements = cbind(Train_activity, Train_measurements)
Train_measurements = cbind(Train_subject, Train_measurements)

######## STEP 3 - MERGE TEST AND TRAINING DATASET ###################################################
# Now Merge Train and Test Datasets
Test_plus_Train_measurements = rbind(Train_measurements,Test_measurements)
Test_plus_Train_measurements = Test_plus_Train_measurements[order(Test_plus_Train_measurements$Activity_Description),]

# remove unwanted dataframes from workspace
rm(Activity_labels, Feature_names)
rm(Train_activity, Train_activity_codes, Train_subject)
rm(Train_measurements,Test_measurements)

######## STEP 4 - CREATE TIDY DATASET ##################################################################
# Create Tidy Dataset
temp_DF = Test_plus_Train_measurements[,-c(2,4)]
melt_DF  =melt(temp_DF,id.vars=c("Subject_Code","Activity_Description"))
Tidy = dcast(melt_DF, Subject_Code + Activity_Description ~ variable, mean)
rm(temp_DF,melt_DF)

# Update Tidy column names
colnames_Tidy = as.data.frame(colnames(Tidy))
colnames(colnames_Tidy) = "V1"
colnames_Tidy = transform(colnames_Tidy,  V1 = paste("Average by Subject & Activity for UCI HAR Source Measurement - ",V1,sep=" "))
colnames_Tidy$V1 = gsub("tBody","Time Body", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("tGravity","Time Gravity", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("fBody", "Frequency Signals - Body", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("fBodyBody", "Frequency Signals - Body", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("fGravity","Frequency Signals - Gravity", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("Acc"," Acceleration ", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("Gyro"," Gyro", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("Mag"," Magnitude", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("tGravity","Time Gravity", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("-X","  X Axis", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("-Y","  Y Axis", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("-Z"," Z Axis", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("-mean"," mean ", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("-std"," Std Deviation ", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("Jerk"," Jerk Signals", colnames_Tidy$V1)
colnames_Tidy$V1 = gsub("tGravity","Time Gravity", colnames_Tidy$V1)


colnames_Tidy[1,1] = "Subject"
colnames_Tidy[2,1] = "Activity"
colnames(Tidy) = colnames_Tidy[,1]

rm(colnames_Tidy)

######## STEP 4 - CREATE TIDY TXT FILE ##################################################################
#Write tidy dataset into txt file
write.table(Tidy, file="./data/Cleaning_data_course_project_output_tidy_dataset.txt",row.names=FALSE)

