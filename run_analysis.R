#path is the name of a directory in the working directory containing all the data files
#the function creates one object ("data frames tbl" from the dplyr package) from every file in the directory and with the same name minus the extension
#so that it can be easly analysed and compared with the other files
# call createObjects("all_data") from the folder 
readData <- function(){
      obj_names <- c("X_train", "X_test", "y_train", "y_test", "subject_train", "subject_test","features")
      full_list <- list.files(path = ".", full.names = TRUE, recursive = TRUE)
      for(i in 1:length(obj_names)) {
            path <- grep(paste("/", obj_names[i], ".txt",sep = ""), full_list, value = TRUE)
            assign(obj_names[i], data.frame(read.table(path)), envir = .GlobalEnv)
      }
}

mergeData <- function(){
      #as required by the assignment, we merge the train and test datasets
      DF1 <<- rbind(X_train, X_test) #QUESTION 1
      Y <<- rbind(y_train, y_test)
      colnames(Y) <<- c("Activities")
      subjects <<- rbind(subject_train, subject_test)
      colnames(subjects) <<- c("Subjects")
      
}
clearMemory <- function(){
      rm(X_test, X_train, y_test, y_train, subject_test, subject_train, envir = .GlobalEnv)#clean the memory for unused objects
}
#renameActivities take the name of the data frame as an input and a name in quotation mark as output - ex. renameActivities(DF, "DF2")
renameActivities <- function(data, output) {
      
      data$Activities <- as.character(data$Activities)
      activities_names <-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")
      for(i in 1:length(data$Activities)) {
            k <- as.integer(data$Activities[i])
            data$Activities[i] <- activities_names[k]
            
      }
      assign(output, data, envir = .GlobalEnv)
}



tidyDataset <- function(final_output = "tidyDataset") {
      
      readData()
      mergeData() #QUESTION 1 - 
      clearMemory()
      
      DF_mean_std <- DF1[, grep("mean|std", features$V2)] #QUESTION 2 - data frame with variables with mean and std only (from the main data frame, take only the rows resulting from grep on the feature file)
      DF2 <<- DF_mean_std
      DF_mean_std <- cbind(Y, subjects, DF_mean_std) #merge with the activities vector
      renameActivities(DF_mean_std, "DF3") #QUESTION 3 - descriptive activity names to name the activities in the data set
      #QUESTION 4
      features <- make.names(features$V2, unique = TRUE) #make the variable names legal(no brackets, no "-", etc.)
      DF4 <- DF3
      colnames(DF4)[3:ncol(DF4)] <- grep("mean|std", features, value = TRUE)#rename the columns with the names from the vector "features" which contain "mean" or "std"
      DF4 <<- DF4
      #QUESTION 5
      
      DF_group <- group_by(DF4, Subjects, Activities)
      assign(final_output, summarise_each(DF_group, funs(mean)), envir = .GlobalEnv)
      rm("features", "subjects", "Y", "DF1", "DF2", "DF3","DF4",envir = .GlobalEnv)
      
      
}



