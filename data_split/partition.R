library("data.table")
library('dplyr')
library("ggplot2")

ds2 <- fread("data_source/clean_columns_everything.csv") #read in data

set.seed(1)
train_ind<-runif(length(unique(ds2$TRACTA)),0,1)<.8 #randomly pick 80% of census tracts for training set
test_ind<-!(train_ind) #remaining are test set

train_tracts<-unique(ds2$TRACTA)[train_ind]
test_tracts<-unique(ds2$TRACTA)[test_ind]

length(train_tracts)/length(unique(ds2$TRACTA)) #percentage of tracts in training

#save list of tracts in each split into a .txt file
write(train_tracts,"train_tracts.txt",ncol=1) 
write(test_tracts,"test_tracts.txt",ncol=1)

ds_train<-ds2[ds2$TRACTA %in% train_tracts] #get incidents from tracts in training set
ds_test<-ds2[ds2$TRACTA %in% test_tracts] #get incidents from tracts in test set

#save training and test data to csv
write.csv(ds_train,"train_complete.csv")
write.csv(ds_test,"test_complete.csv")