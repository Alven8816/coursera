
#Merges the training and the test sets to create one data set
trains <- read.table(file = "C:\\Users\\HP\\Documents\\coursera\\UCI HAR Dataset\\train\\X_train.txt", header = FALSE)
test <- read.table(file = "C:\\Users\\HP\\Documents\\coursera\\UCI HAR Dataset\\test\\X_test.txt", header = FALSE)
X <- rbind(trains,test)

trains_y <- read.table(file = "C:\\Users\\HP\\Documents\\coursera\\UCI HAR Dataset\\train\\y_train.txt", header = FALSE)
test_y <- read.table(file = "C:\\Users\\HP\\Documents\\coursera\\UCI HAR Dataset\\test\\y_test.txt", header = FALSE)
Y <- rbind(trains_y,test_y)

trains_sub <- read.table(file = "C:\\Users\\HP\\Documents\\coursera\\UCI HAR Dataset\\train\\subject_train.txt", header = FALSE)
test_sub <- read.table(file = "C:\\Users\\HP\\Documents\\coursera\\UCI HAR Dataset\\test\\subject_test.txt", header = FALSE)
S <- rbind(trains_sub,test_sub)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
#读取下载后的变量名文件
names<- read.table(file = "C:\\Users\\HP\\Documents\\coursera\\UCI HAR Dataset\\features.txt")
#变量名全改为小写
names<- tolower(names$v2)
#修改变量名不符合部分，分离元数据，并运用可描述语句设置变量名
names<- strsplit(names,split = "-")
named <- function(x) {    rr <- paste(x[2], x[1], "-", x[3], sep = "")
chartr("()", "of", rr)
}
names<- sapply(names, named)
the_mean_and_standard_deviation <- grep("mean|std",names)
X <- X[,the_mean_and_standard_deviation]
names(X) <- names[the_mean_and_standard_deviation]

#Uses descriptive activity names to name the activities in the data set
activity_names <- read.table("C:\\Users\\HP\\Documents\\coursera\\UCI HAR Dataset\\activity_labels.txt")
activity_names[, 2] = gsub("_", "", tolower(as.character(activity_names[, 2])))
Y[,1] <- activity_names[Y[,1],2]
names(Y) <- "activity"

#Appropriately labels the data set with descriptive activity names.
names(S) <- "subject"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "C:\\Users\\HP\\Documents\\coursera\\merged_clean_data.txt")

# 5. Creates a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activity_names[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (s in 1:numSubjects) {
  for (a in 1:numActivities) {
    result[row, 1] = uniqueSubjects[s]
    result[row, 2] = activity_names[a, 2]
    tmp <- cleaned[cleaned$subject==s & cleaned$activity==activity_names[a, 2], ]
    result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
    row = row+1
  }
}
write.table(result, "C:\\Users\\HP\\Documents\\coursera\\data_set_with_the_averages.txt", row.name=FALSE)