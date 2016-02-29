#############################
# < Zhang Yunyan >
# STAT W4240 
# Homework <3> , Problem <4>
# < March 4th >
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("~/Learning/stat w4240 data mining/homework3")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  
library(pixmap)
library(class)
#################
# Problem 4a
#################

views_4a = c('P00A+000E+00', 'P00A+005E+10', 'P00A+005E-10', 'P00A+010E+00' )

# load the data and save it as a matrix with the name face_matrix_4a

#----- START YOUR CODE BLOCK HERE -----#

# Code block from hw01.R solutions
# Includes a few modifications
#	- changed view_list
#	- refactored some variable names
#	- attached vector to matrix
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
name=vector()
subfile=vector()
viewnum=vector()
face_matrix_4a=vector()
for ( i in 1:length(dir_list_1) ){
  for( j in 1:length(views_4a) ){
  filename = sprintf("CroppedYale/%s/%s_%s.pgm", 
                     dir_list_1[i] , dir_list_1[i] , views_4a[j])
  # record the subject number
  name<-rbind(name,filename)
  subfile<-rbind(subfile,i)
  viewnum<-rbind(viewnum,j)
  # load the views
  face = read.pnm(file = filename)
  # convert each row to a vector
  chan = as.vector(getChannels(face))
  # store the collection as a matrix where each row is a photo
  face_matrix_4a = rbind( face_matrix_4a , chan )
  chan=vector()
  }
}

# End code block
###############################
#----- END YOUR CODE BLOCK HERE -----#

# Get the size of the matrix for use later
fm_4a_size = dim(face_matrix_4a)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4a = floor(fm_4a_size[1]*4/5) # Number of training obs
ntest_4a = fm_4a_size[1]-ntrain_4a # Number of testing obs
set.seed(1) # Set pseudo-random numbers so everyone gets the same output
ind_train_4a = sample(1:fm_4a_size[1],ntrain_4a) # Training indices
ind_test_4a = c(1:fm_4a_size[1])[-ind_train_4a] # Testing indices

#----- START YOUR CODE BLOCK HERE -----#
# the first 5 files in the training set
rownames(name)<-c(1:152)
name[ind_train_4a[1:5],]
#sub_no[ind_train_4a[1:5],]
#view_no[ind_train_4a[1:5],]
# the first 5 files in the testing set
name[ind_test_4a[1:5],]
#sub_no[ind_test_4a[1:5],]
#view_no[ind_test_4a[1:5],]
#apply(dat[,1:2],2,dist())

#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4b
#################

#----- START YOUR CODE BLOCK HERE -----#
# load training set
face_matrix_train<-face_matrix_4a[ind_train_4a,]
# compute mean face
means<-as.matrix(colMeans(face_matrix_train)) 
iden<-matrix(c(rep(1,length(ind_train_4a))),nrow = length(ind_train_4a),ncol = 1)
# subtract off mean face
face_matrix_train_cent<-face_matrix_train-iden%*%t(means) 
# run prcomp()
pc_train<-prcomp(face_matrix_train_cent)
# load testing data
face_matrix_test<-face_matrix_4a[ind_test_4a,]
# subtract off mean face
iden2<-matrix(c(rep(1,length(ind_test_4a))),nrow = length(ind_test_4a),ncol = 1)
face_matrix_test_cent<-face_matrix_test-iden2%*%t(means)
# project testing data onto the first 25 loadings

face_matrix_train_scores<-pc_train$x[,1:25]
face_matrix_test_scores<-face_matrix_test_cent%*%as.matrix(pc_train$rotation[,1:25])
train_class<-subfile[ind_train_4a]
test_class<-subfile[ind_test_4a]
test_est_class<-knn1(face_matrix_train_scores,face_matrix_test_scores,train_class)
sum(test_est_class!=test_class)
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4c
#################

# Use different lighting conditions

views_4c = c('P00A-035E+15', 'P00A-050E+00', 'P00A+035E+15', 'P00A+050E+00')

# load your data and save the images as face_matrix_4c

#----- START YOUR CODE BLOCK HERE -----#
namec=vector()
subfilec=vector()
viewnumc=vector()
face_matrix_4c=vector()
for ( i in 1:length(dir_list_1) ){
  for( j in 1:length(views_4c) ){
    filename = sprintf("CroppedYale/%s/%s_%s.pgm", 
                       dir_list_1[i] , dir_list_1[i] , views_4c[j])
    # record the subject number
    namec<-rbind(namec,filename)
    subfilec<-rbind(subfilec,i)
    viewnumc<-rbind(viewnumc,j)
    # load the views
    face = read.pnm(file = filename)
    # convert each row to a vector
    chan = as.vector(getChannels(face))
    # store the collection as a matrix where each row is a photo
    face_matrix_4c = rbind( face_matrix_4c , chan )
    chan=vector()
  }
}
rownames(namec)<-c(1:152)
#----- END YOUR CODE BLOCK HERE -----#

fm_4c_size = dim(face_matrix_4c)
# Use 4/5 of the data for training, 1/5 for testing
ntrain_4c = floor(fm_4c_size[1]*4/5)
ntest_4c = fm_4c_size[1]-ntrain_4c
set.seed(2) # Set pseudo-random numbers
# You are resetting so that if you have used a random number in between the last use of sample(), you will still get the same output
ind_train_4c = sample(1:fm_4c_size[1],ntrain_4c)
ind_test_4c = c(1:fm_4c_size[1])[-ind_train_4c]

#----- START YOUR CODE BLOCK HERE -----#
# load training set
face_matrix_trainc<-face_matrix_4c[ind_train_4c,]
train_classc<-subfile[ind_train_4c]
test_classc<-subfile[ind_test_4c]
# compute mean face
meansc<-as.matrix(colMeans(face_matrix_trainc)) 
idenc<-matrix(c(rep(1,length(ind_train_4c))),nrow = length(ind_train_4c),ncol = 1)
# subtract off mean face
face_matrix_trainc_cent<-face_matrix_trainc-iden%*%t(meansc) 
# run prcomp()
pc_trainc<-prcomp(face_matrix_trainc_cent)
face_matrix_trainc_scores<-pc_trainc$x[,1:25]
# load testing data
face_matrix_testc<-face_matrix_4c[ind_test_4c,]
# subtract off mean face
iden2c<-matrix(c(rep(1,length(ind_test_4c))),nrow = length(ind_test_4c),ncol = 1)
face_matrix_testc_cent<-face_matrix_testc-iden2c%*%t(meansc)
# project testing data onto the first 25 loadings
face_matrix_testc_scores<-face_matrix_testc_cent%*%as.matrix(pc_trainc$rotation[,1:25])
test_est_classc<-knn1(face_matrix_trainc_scores,face_matrix_testc_scores,train_classc)
sum(test_est_classc==test_classc)
sum(test_est_classc!=test_classc)

ori<-namec[ind_test_4c[which(test_est_classc!=test_classc)]]
mis<-ori
for(i in 1:27)
{
  mis[i]<-sprintf("CroppedYale/%s/%s_%s", 
          dir_list_1[test_est_classc[which(test_est_classc!=test_classc)[i]]], 
          dir_list_1[test_est_classc[which(test_est_classc!=test_classc)[i]]],strsplit(ori[i],"_")[[1]][2])
}
par(mfrow=c(1,2))
for(i in 1:27)
{
  plot(read.pnm(ori[i]),main="original")
  plot(read.pnm(mis[i]),main="misclassified")
  dev.copy(device=png,sprintf("photo%d.png",i))
  dev.off()
}

#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 4d
#################

#----- START YOUR CODE BLOCK HERE -----#
misnum<-vector()
for(i in 10:19)
{
  set.seed(i) # Set pseudo-random numbers
  # You are resetting so that if you have used a random number in between the last use of sample(), you will still get the same output
  ind_train_4d = sample(1:fm_4c_size[1],ntrain_4c)
  ind_test_4d = c(1:fm_4c_size[1])[-ind_train_4d]
  face_matrix_traind<-face_matrix_4c[ind_train_4d,]
  train_classd<-subfilec[ind_train_4d]
  test_classd<-subfilec[ind_test_4d]
  # compute mean face
  meansd<-as.matrix(colMeans(face_matrix_traind)) 
  idend<-matrix(c(rep(1,length(ind_train_4d))),nrow = length(ind_train_4d),ncol = 1)
  # subtract off mean face
  face_matrix_traind_cent<-face_matrix_traind-iden%*%t(meansd) 
  # run prcomp()
  pc_traind<-prcomp(face_matrix_traind_cent)
  face_matrix_traind_scores<-pc_traind$x[,1:25]
  # load testing data
  face_matrix_testd<-face_matrix_4c[ind_test_4d,]
  # subtract off mean face
  iden2d<-matrix(c(rep(1,length(ind_test_4d))),nrow = length(ind_test_4d),ncol = 1)
  face_matrix_testd_cent<-face_matrix_testd-iden2c%*%t(meansd)
  # project testing data onto the first 25 loadings
  face_matrix_testd_scores<-face_matrix_testd_cent%*%as.matrix(pc_traind$rotation[,1:25])
  test_est_classd<-knn1(face_matrix_traind_scores,face_matrix_testd_scores,train_classd)
  misnum<-append(misnum,sum(test_est_classd!=test_classd))
}
misnum
#----- END YOUR CODE BLOCK HERE -----#

#################
# End of Script
#################


