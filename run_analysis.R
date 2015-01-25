#REQUIREMENTS
#The code run_analysis.R requires the package dplyr to run correctly. Dplyr is called from the library at the beginning of the script, if not present in the library, please install it before running it.
#The dataset Human Activity Recognition Using Smartphones Dataset V1.0 has to be downloaded and unzipped in the working directory.
#INSTRUCTIONS
#The main function to be run is tidyDataset(), which returns a tidy data set with the average of each variable for each activity and each subject. 
#The functions readData, mergeData, cleanMemory and renameActivities are called during the process by the main function
#The steps of the manipulation described in the README.md file are annotated next to the relevant code line


tidyDataset <- function(final_output = "tidyDataset") {
      library("dplyr")
      readData()
      mergeData() #STEP 1 Merge the train and test sets to create one dataset.
      clearMemory()
      
      DF2 <- DF1[, grep("mean|std", features$V2)] #STEP 2 - data frame with variables with mean and std only (from the main data frame, take only the rows resulting from grep on the feature file)
      DF2 <- cbind(Y, subjects, DF2) #merge with the activity vector
      renameActivities(DF2, "DF3") #STEP 3 - descriptive activity names to name the activities in the data set
      #STEP 4 - ppropriately label the data set with descriptive variable names. 
      features <- make.names(features$V2, unique = TRUE) #make the variable names legal(no brackets, no "-", etc.)
      colnames(DF3)[3:ncol(DF3)] <- grep("mean|std", features, value = TRUE)#rename the columns with the names from the vector "features" which only contains "mean" or "std"
      
      #STEP 5 - Return as an output an independent tidy data set with the average of each variable for each activity and each subject.   
      DF_group <- group_by(DF3, Subjects, Activities)#the dataframe is grouped by the activities and subject
      assign(final_output, summarise_each(DF_group, funs(mean)), envir = .GlobalEnv)#the output dataframe summarises for each pair subject/activity the means of each variable
      
      #remove temporary variables
      rm("features", "subjects", "Y", "DF1", "DF3",envir = .GlobalEnv)
      
      
}



readData <- function(){ #read the dataset and creates the objects for the manipulation from the relevant files
      obj_names <- c("X_train", "X_test", "y_train", "y_test", "subject_train", "subject_test","features") #name of the relevant files for the manipulation
      full_list <- list.files(path = ".", full.names = TRUE, recursive = TRUE) # list of all the files of the working directory
      for(i in 1:length(obj_names)) { #extract the relevant files and assign the to objects
            path <- grep(paste("/", obj_names[i], ".txt",sep = ""), full_list, value = TRUE)#extract from the complete list of files, only the files with the relevant name
            assign(obj_names[i], data.frame(read.table(path)), envir = .GlobalEnv)#assign to each relevant name an object 
      }
}

mergeData <- function(){ #The train and test sets are merged in one dataset
      DF1 <<- rbind(X_train, X_test) 
      Y <<- rbind(y_train, y_test) 
      colnames(Y) <<- c("Activities") 
      subjects <<- rbind(subject_train, subject_test)
      colnames(subjects) <<- c("Subjects")
      
}
clearMemory <- function(){#function called to remove from the memory some working variables when they become obsolete
      rm(X_test, X_train, y_test, y_train, subject_test, subject_train, envir = .GlobalEnv)
}



#renameActivities takes the name of the data frame as an input and a name in quotation mark as an output - ex. renameActivities(DF, "DF2")
renameActivities <- function(data, output) {
      
      data$Activities <- as.character(data$Activities)
      activities_names <-c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING")#Activitiy names from the file activity_labels.txt
      for(i in 1:length(data$Activities)) {#each line of the variable Activities is checked and replaced with the equivalent activity name
            k <- as.integer(data$Activities[i])
            data$Activities[i] <- activities_names[k]
            
      }
      assign(output, data, envir = .GlobalEnv)
}





tidyDataset()

