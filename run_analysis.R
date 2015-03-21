setwd("~/coursera/r/clearing data/project")
## Create dataset with activity_labels.txt
## <Common initial steps>
## This will be used later to achieve <Step 2>
allFeatures <- read.csv("UCI HAR Dataset/features.txt", sep="", header = FALSE)
allFeatures[,2] = gsub('-', '', allFeatures[,2])
allFeatures[,2] = gsub('[-()]', '', allFeatures[,2])
## This will be used later to achieve <Step 5>
activityDataframe <- read.csv("UCI HAR Dataset/activity_labels.txt", sep = "", header = FALSE)

## First, we read X_train and put it into the training data frame:
training <- read.csv("UCI HAR Dataset/train/X_train.txt", sep="", header = FALSE)
        ## Then we add two variables at the end:
        ## nº562: Y_train.txt, which will be the activity training labels       
training[, 562] <- read.csv("UCI HAR Dataset/train/Y_train.txt", sep="", header = FALSE)
        ## nº563: subject_train.txt, which will identify each subject
training[, 563] <- read.csv("UCI HAR Dataset/train/subject_train.txt", sep="", header = FALSE)

## Second, we read X_test and put into the testing data frame:
testing = read.csv("UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
        ## Then, again, we add two variables at the end:
        ## nº562: Y_test.txt, which will be the activity training labels
testing[, 562] = read.csv("UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE)
        ## nº563: subject_test.txt, which will identify each subject
testing[, 563] = read.csv("UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)


## <Step 1> Merging the training and the test sets to create one data set.
dataset <- rbind(training, testing)

## <Step 2> Extracting only the measurements on the mean and standard deviation for each measurement.
## First, we select the variables from the "features" data frame, that match means or stds,
## which includes the 562th and 563th variables (activity labels and subjects)
targetCols <- grep(".*Mean.*|.*mean.*|.*Std.*|.*std.*", allFeatures[ ,2])
## and then, this series of integers set the subselection of our initial Features frame:
targetFeatures <- allFeatures[targetCols, ]
targetCols <- c(targetCols, 562, 563)
## So, once we have this subset of variables, we can make the subsequent new dataset by:
dataset <- dataset[ ,targetCols]
##...which has the new two variables.

## <Step 3> Uses descriptive activity names to name the activities in the data set
## Now, we just have to replace the common "V1", "V2", ... , "V560", "V561", "V1.1", "V1.2"
## We will rename them as the should: taking their labels from the targetFeatures + activity and subject
colnames(dataset) <- c(targetFeatures$V2, "activityLabel", "subject")

## <Step 4> Appropriately labels the data set with descriptive variable names.
## Running a loop to replace each activity code (from 1 to 6) with their descriptions
Activity <- 1
for (i in activityDataframe$V2){
        dataset$activityLabel <- gsub(Activity, i, dataset$activityLabel)
        Activity <- Activity + 1
}

## Let's convert the last two variables to a factor data type.
dataset$activityLabel <- as.factor(dataset$activityLabel)
dataset$subject <- as.factor(dataset$subject)

## <Step 5> From the data set in step 4, creates a second, independent tidy data set with
## the average of each variable for each activity and each subject.
## First, we use the function aggregate
tidyDataset <- aggregate(dataset, by=list(activityLabel = dataset$activityLabel, subject = dataset$subject), mean)
## This new tidyDataset has activityLabel and subject as the first two variables
## There are now two (89 and 90) more variables than needed. We eleiminate them.
tidyDataset[ ,90] = NULL
tidyDataset[ ,89] = NULL
write.table(tidyDataset, "tidyDataset.txt", sep="\t")
## The result file has 180 observations for the selected "mean" and "std" (88) variables
## It is consistent with the fact that there are 30 subjects and 6 activities.