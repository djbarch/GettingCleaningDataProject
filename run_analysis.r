library(plyr)
library(reshape2)

## Objectives
## One -- one column per variable
## Two -- one row per observation
## Three -- table linking ids included

## Training and test set merging
root.dir <- "UCI HAR Dataset"

data.set <- list()

message("loading features.txt")
data.set$features <- read.table(paste(root.dir, "features.txt", sep="/"), col.names=c('id', 'name'), stringsAsFactors=FALSE)

message("loading activity_features.txt")
data.set$activity_labels <- read.table(paste(root.dir, "activity_labels.txt", sep="/"), col.names=c('id', 'Activity'))

message("loading test set")
data.set$test <- cbind(subject=read.table(paste(root.dir, "test", "subject_test.txt", sep="/"), col.names="Subject"),
      y=read.table(paste(root.dir, "test", "y_test.txt", sep="/"), col.names="Activity.ID"),
      x=read.table(paste(root.dir, "test", "x_test.txt", sep="/")))

message("loading train set")
data.set$train <- cbind(subject=read.table(paste(root.dir, "train", "subject_train.txt", sep="/"), col.names="Subject"),
      y=read.table(paste(root.dir, "train", "y_train.txt", sep="/"), col.names="Activity.ID"),
      x=read.table(paste(root.dir, "train", "X_train.txt", sep="/")))

rename.features <- function(col) {
      col <- gsub("tBody", "Time.Body", col)
      col <- gsub("tGravity", "Time.Gravity", col)
      col <- gsub("fBody", "FFT.Body", col)
      col <- gsub("fGravity", "FFT.Gravity", col)
      col <- gsub("\\-mean\\(\\)\\-", ".Mean.", col)
      col <- gsub("\\-std\\(\\)\\-", ".Std.", col)
      col <- gsub("\\-mean\\(\\)", ".Mean", col)
      col <- gsub("\\-std\\(\\)", ".Std", col)
    return(col)
}

## Mean and standard deviation per measurement
tidy <- rbind(data.set$test, data.set$train)[,c(1, 2, grep("mean\\(|std\\(", data.set$features$name) + 2)]

## Descriptive activity names
names(tidy) <- c("Subject", "Activity.ID", rename.features(data.set$features$name[grep("mean\\(|std\\(", data.set$features$name)]))

## Descriptive activity name labeling
tidy <- merge(tidy, data.set$activity_labels, by.x="Activity.ID", by.y="id")
tidy <- tidy[,!(names(tidy) %in% c("Activity.ID"))]

## Separate tidy data set with mean of each variable per activity per subject
tidy.mean <- ddply(melt(tidy, id.vars=c("Subject", "Activity")), .(Subject, Activity), summarise, MeanSamples=mean(value))

write.csv(tidy.mean, file = "tidy.mean.txt",row.names = FALSE)
write.csv(tidy, file = "tidy.txt",row.names = FALSE)

