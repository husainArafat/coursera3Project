coursera3script <- function(){
  
  ##install packages
  install.packages("reshape2")
  library(reshape2)
  
  
  ## Download and unzip data
  urlData <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url = urlData, destfile = "./Dataset.zip")
  unzip(zipfile = "./Dataset.zip")
  
  ## load into dataframes
  subjectData1 <- read.table(file = "./UCI HAR Dataset/test/subject_test.txt")
  subjectData2 <- read.table(file = "./UCI HAR Dataset/train/subject_train.txt")
  xData <- read.table(file = "./UCI HAR Dataset/test/X_test.txt")
  yData <- read.table(file = "./UCI HAR Dataset/test/Y_test.txt")
  
  ## combine train and test data
  
  subjectData <- rbind(subjectData1, subjectData2)
  xData <- rbind(xData, read.table(file = "./UCI HAR Dataset/train/X_train.txt"))
  yData <- rbind(yData, read.table(file = "./UCI HAR Dataset/train/Y_train.txt"))
  
  ## load in labels
  xlabels <- read.table(file = "./UCI HAR Dataset/features.txt")
  activityLabels <- read.table(file = "./UCI HAR Dataset/activity_labels.txt")
  colnames(activityLabels) <- c("V1", "activities")
  ## assign labels to columns
  colnames(xData) <- xlabels[,2]
  colnames(yData) <- "activityData"
  colnames(subjectData) <- "subjectID"
  
  ## consolidate data
  totalData <- cbind(xData,yData, subjectData)
  
  ## columns to extract
  columnsToEx <- c("activityData", "subjectID", "mean", "std")
  
  ## logic of columns to keep
  columnsToExNum <- (grepl(columnsToEx[1], colnames(totalData)) |
                       grepl(columnsToEx[2], colnames(totalData)) |
                       grepl(columnsToEx[3], colnames(totalData)) |
                       grepl(columnsToEx[4], colnames(totalData)) )
  
  ## filter data
  filteredData <- totalData[, columnsToExNum]
  
  ## merge in activity descriptions
  filteredData <- merge(filteredData, activityLabels, by.x = "activityData", by.y = "V1")
  
  
  filteredData.melted <- melt(filteredData, id = c("subjectID", "activities"))
  filteredData.mean <- dcast(filteredData.melted, subjectID + activities ~ variable, mean)
  write.table(filteredData.mean, "tidy.txt", row.names = FALSE, quote = FALSE)
  
}