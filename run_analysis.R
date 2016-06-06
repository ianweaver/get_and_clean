

run_analysis <-function() {
  
  # Add the dplyr library as my come in handy and data.table for faster loading
  library(dplyr)
  library(data.table)
  
  
  
  
  #Load the X_test data
  
  x.test <- fread("./test/X_test.txt")
  
  
  #load the X_train data
  
  x.train <-fread("./train/X_train.txt")
  
  
  #Combine the two
  
  x<-bind_rows(x.test, x.train)
  
  
  #Get column labels
  
  features <- fread("features.txt")
  colnames<-unlist(select(features,2))
  
  
  #pull out mean and std from those columns
  colsselect<-grep("(mean|std)\\(\\)", colnames)
  x<-select(x,colsselect)
  
  #label columns, has to be done after select due to some erroneous names
  #remove brackets as well
  
  colnames<-colnames[colsselect]
  colnames<-gsub("[\\(\\)]","", colnames)
  setnames(x,names(x),colnames)
  
  
  #Load test activity ids
  y.test<-fread("./test/y_test.txt")
  
  #Load train activity ids
  y.train<-fread("./train/y_train.txt")
  
  #Build consolidatd
  
  y<-bind_rows(y.test,y.train)
  
  #Load activity labels
  activitylabels<-fread("./activity_labels.txt")
  
  
  #Build activities label list (will join on first column, v1)
  activities<-inner_join(y,activitylabels)
  
  activities<-select(activities,2)
  activities<-rename(activities, activity_label=V2)
  
  
  
  #Load test subjects
  subject.test<-fread("./test/subject_test.txt")
  
  #Load train subjects#
  subject.train<-fread("./train/subject_train.txt")
  
  #Build consolidated
  
  subject<-bind_rows(subject.test, subject.train)
  
  subject<-rename(subject, subject_id=V1)
  
  #Create full output data set
  output<-bind_cols(subject, activities, x)
  
  
  #Create summarised data set
  summ <- output %>% group_by(subject_id, activity_label) %>% summarise_each(funs(mean))
  
  write.table(summ,"summarised_output.txt",row.name = FALSE)
  
  
  "OK"
  
  
  
  
  
  
}