library(reshape2)

##############################
######### 1. READING #########
##############################

# 1.1 Features Reading
features <- read.table("./UCI HAR Dataset/features.txt")[,2]
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")

# 1.2 Activity Reading
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
Y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
Y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

# 1.3 Subject Reading
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

########################################
######### 2. MERGING AND NAMING #########
#########################################

# 2.1 Merging test\train (horizontal)
dataFeatures <- rbind(X_test, X_train)
dataActivity <- rbind(Y_test, Y_train)
dataSubject <- rbind(subject_test, subject_train)

# 2.2 Setting names

names(dataFeatures) <- features
names(dataActivity) <- "activity"
names(dataSubject) <- "subject"

# 2.3 Merging to the one data frame (vertical)

Data <- cbind(dataFeatures, cbind(dataSubject, dataActivity))

##########################################################
######### 3. Substract only mean and sd features #########
##########################################################

MSFeatures <- as.character(features[grepl("mean|std", features)])
Data <- subset(Data,select=c(MSFeatures, "subject", "activity" ))

##########################################################################
######### 4. Implement descriptive activity names instead numbers ########
##########################################################################

Data[,dim(Data)[2]] <- activity_labels[Data[,dim(Data)[2]]]

##########################################################
######### 5. Correcting names of Data Set (a bit) ########
##########################################################

names(Data) <- gsub("\\(|\\)", "", names(Data))
names(Data) <- gsub("BodyBody", "Body", names(Data))

##########################################################
######### 6. Creating independant tidy data set ##########
##########################################################

kj = length(names(Data))-2
Data_melt <- melt(Data,id=c("subject","activity"),measure.vars = names(Data)[1:kj])
Data_tidy <- dcast(Data_melt, subject + activity ~ variable, mean)
write.table(Data_tidy, file = "./tidy_data.txt")


