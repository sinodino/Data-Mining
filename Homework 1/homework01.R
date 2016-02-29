#############################
# Homework <1> 
# < Feb 5th >
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
setwd("~/Learning/stat w4240 data mining/homework1")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  From the
# command line, type install.packages("pixmap")
library(pixmap)

#################
# Problem 1a
#################

# paste or type in the given code here
face_01 = read.pnm(file = "CroppedYale/yaleB01/yaleB01_P00A-005E+10.pgm")

# now plot the data
plot(face_01)
# give it a nice title
title('hw01_01a: the first face')
# save the result
filename = 'hw01_01a.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

# extract the class and size

#----- START YOUR CODE BLOCK HERE -----#
face_01
#class: "pixmapGrey"
#size: 192x168 
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1b
#################

# make face_01 into a matrix with the given command
face_01_matrix = getChannels(face_01)

# load a second face
face_02 = read.pnm(file = "CroppedYale/yaleB02/yaleB02_P00A-005E+10.pgm")
face_02_matrix = getChannels(face_02)

# combine two faces into a single data matrix and make that a pixmap
faces_matrix = cbind( face_01_matrix , face_02_matrix )
faces = pixmapGrey( faces_matrix )

# plot to verify
plot(faces)

# find min and max values 

#----- START YOUR CODE BLOCK HERE -----#
max(faces_matrix)
#1
min(faces_matrix)
#0.007843137
grey(1)
#FFFFFF
grey(0)
#000000

#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1c
#################

# get directory structure
dir_list_1 = dir(path="~/Learning/stat w4240 data mining/homework1/CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="~/Learning/stat w4240 data mining/homework1/CroppedYale/",all.files=FALSE,recursive=TRUE)

# find lengths

#----- START YOUR CODE BLOCK HERE -----#
length(dir_list_1)
#38
head(dir_list_1)
#"yaleB01" "yaleB02" "yaleB03" "yaleB04" "yaleB05" "yaleB06"
length(dir_list_2)
#2547
head(dir_list_2)
# [1] "yaleB01/DEADJOE"                 
# [2] "yaleB01/WS_FTP.LOG"              
# [3] "yaleB01/yaleB01_P00_Ambient.pgm" 
# [4] "yaleB01/yaleB01_P00.info"        
# [5] "yaleB01/yaleB01_P00A-005E-10.pgm"
# [6] "yaleB01/yaleB01_P00A-005E+10.pgm"
#----- END YOUR CODE BLOCK HERE -----#

#################
# Problem 1d
#################

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
pic_list = c( 09 , 12 , 22 )
view_list = c(  'P00A-005E+10' , 'P00A-005E-10' , 'P00A-010E+00')

# preallocate an empty list
pic_data = vector("list",length(pic_list)*length(view_list))
# initialize an empty matrix of faces data
faces_matrix = vector()

#----- START YOUR CODE BLOCK HERE -----#

for(i in 1:length(pic_list)){
  temp_face_vector=vector()
  for(j in 1:length(view_list)){
    filename = sprintf("CroppedYale/%s/%s_%s.pgm",
                       dir_list_1[pic_list[i]] , dir_list_1[pic_list[i]] , view_list[j])
    temp_face=read.pnm(file = filename)
    temp_face_matrix=getChannels(temp_face)
    temp_face_vector = cbind( temp_face_vector , temp_face_matrix )
  }
  faces_matrix=rbind(faces_matrix,temp_face_vector)
}
pic_data = faces_matrix
#----- END YOUR CODE BLOCK HERE -----#

# now faces_matrix has been built properly.  plot and save it.
faces = pixmapGrey(faces_matrix)
plot(faces)
# give it a nice title
title('hw01_01d: 3x3 grid of faces')
# save the result
filename = 'hw01_01d.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# Problem 2a
#################

# Your code here
view_list1= c(  'P00A+000E+00' , 'P00A+005E+10' , 'P00A+005E-10','P00A+010E+00')
faces_matrix1 = vector()
temp_face_matrix1=vector()
for(i in 1:length(dir_list_1)){
  temp_face_row1=vector()
  for(j in 1:length(view_list1)){
    filename1 = sprintf("CroppedYale/%s/%s_%s.pgm",
                        dir_list_1[i] , dir_list_1[i] , view_list1[j])
    temp_face1=read.pnm(file = filename1)
    temp_face_matrix1=getChannels(temp_face1)
    temp_face_matrix1=as.vector(temp_face_matrix1)
    temp_face_row1 = rbind( temp_face_row1 , temp_face_matrix1 )
  }
  faces_matrix1=rbind(faces_matrix1,temp_face_row1)
}
dim(faces_matrix1)
# 152 32256
#################
# Problem 2b
#################

# Your code here
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)
valid_name=vector()
valid_name=rbind(valid_name,dir_list_2[which(grepl(".pgm$",dir_list_2))])
length(valid_name)
#2452
valid_name[10]
#"yaleB01/yaleB01_P00A-025E+00.pgm"

#################
# Problem 2c
#################
# Your code here
threshold=0.05
temp_black_name=vector()
min_pixel_vector=vector()
max_pixel_vector=vector()
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)
for(i in 1:length(valid_name)){
  filename2 = sprintf("CroppedYale/%s",valid_name[i])
  temp_face2=read.pnm(file = filename2)
  temp_face_matrix2=getChannels(temp_face2)
  if(max(temp_face_matrix2)<=threshold){
    temp_black_name=rbind(temp_black_name,valid_name[i])
    min_pixel=min(temp_face_matrix2)
    min_pixel_vector=rbind(min_pixel_vector,min_pixel)
    max_pixel=max(temp_face_matrix2)
    max_pixel_vector=rbind(max_pixel_vector,max_pixel)
  }
}
temp_black_name

# [1] "yaleB01/yaleB01_P00_Ambient.pgm" 
# [2] "yaleB02/yaleB02_P00_Ambient.pgm" 
# [3] "yaleB02/yaleB02_P00A+095E+00.pgm"
# [4] "yaleB04/yaleB04_P00_Ambient.pgm" 
# [5] "yaleB05/yaleB05_P00_Ambient.pgm" 
# [6] "yaleB06/yaleB06_P00_Ambient.pgm" 
# [7] "yaleB07/yaleB07_P00_Ambient.pgm" 
# [8] "yaleB08/yaleB08_P00_Ambient.pgm" 
# [9] "yaleB34/yaleB34_P00A+095E+00.pgm"
# [10] "yaleB35/yaleB35_P00A+095E+00.pgm"
# [11] "yaleB36/yaleB36_P00A+095E+00.pgm"
# [12] "yaleB37/yaleB37_P00A+095E+00.pgm"
# [13] "yaleB38/yaleB38_P00A+095E+00.pgm"
# [14] "yaleB39/yaleB39_P00A+095E+00.pgm"
min(min_pixel_vector)
# 0
grey(min(min_pixel_vector))
# "#000000"
max(max_pixel_vector)
# 0.04705882
grey(max(max_pixel_vector))
# "#0A0A0A"

#################
# Problem 2d
#################
# Your code here

proportion_vector=vector()
threshold=0
dir_list_1 = dir(path="CroppedYale/",all.files=FALSE)
dir_list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)
for(j in 1:21){
  bad=0
  black_name_column=vector()
  for(i in 1:length(valid_name)){
    filename2 = sprintf("CroppedYale/%s",valid_name[i])
    temp_face2=read.pnm(file = filename2)
    temp_face_matrix2=getChannels(temp_face2)
    if(max(temp_face_matrix2)<=threshold){
      bad=bad+1
      black_name_column=rbind(black_name_column,filename2)
    }
  }
  print(black_name_column)
  threshold=threshold+0.05
  proportion_vector=rbind(proportion_vector,bad/length(valid_name))
}
threshold_value<-seq(0,1,0.05)
proportion=proportion_vector
plot(threshold_value,proportion,main="hw01_02d proportion of “no flash” photos","l")
# save the result
filename3 = 'hw01_02d.png'
dev.copy(device=png, file=filename3, height=600, width=800)
dev.off()
# [1] 0.000000000 0.005709625 0.007340946 0.008564437 0.009787928
# [6] 0.010603589 0.011011419 0.011827080 0.012642741 0.013050571
# [11] 0.013050571 0.013866232 0.015497553 0.017944535 0.021615008
# [16] 0.035481240 0.050163132 0.084828711 0.129282219 0.200652529
# [21] 1.000000000
#################
# End of Script
#################