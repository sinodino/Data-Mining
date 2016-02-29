#############################
# Homework <HW 2> , Problem <1,2>
# < Feb 19th >
#
# The following code loads the eigenfaces data and
# performs a set of simple loading and plotting functions
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
#setwd("~/Documents/academic/teaching/STAT_W4240_2014_SPRG/hw/hw01")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  From the
# command line, type install.packages("pixmap")
library(pixmap)

#################
# # Problem 1
#################
# a
setwd("~/Learning/stat w4240 data mining/homework2/")
p1 <- read.csv("hw02_q1_p1.csv")
p1<-data.matrix(p1)
colMeans(p1)  # compute the column means
rowMeans(p1)  # compute the row means

# b
colm<-as.matrix(colMeans(p1))
one<-matrix(c(rep(1,100)),nrow = 100,ncol = 1)
p1_cent<-p1-one%*%t(colm) # center data
cov(p1_cent)     # get the covariance of centered data

# c
# compute for eigenvalues and eigenvectors
p1eigen<-eigen(cov(p1_cent),symmetric=T,only.values = F) 
p1eigen$values    #eigenvalues
p1eigen$vectors   #eigenvectors

# d
t(p1eigen$vectors)     #loadings
t(p1eigen$vectors)%*%t(p1_cent) #scores

# e
# Plot the proportion of variance captured against 
# the number of components included.
plot(seq(1:5),cumsum(p1eigen$values)/sum(p1eigen$values),
     xlab="component",ylab="variance captured (%)",
     main="variance captured against components number",type="b")

# f
p12 <- read.csv("hw02_q1_p2.csv")  # load 5 new observations
p12<-data.matrix(p12)
colm2<-as.matrix(colMeans(p12))
one2<-matrix(c(rep(1,5)),nrow = 5,ncol = 1) 
p12_cent<-p12-one2%*%t(colm)   # center new data by subtracting means from p1

p12scores<-t(as.matrix(p1eigen$vectors))%*%t(p12_cent) # compute scores
p12scores

# g
# use only the first two scores to represent the observations 
# from the previous part.

p12proj<-t(p12scores[1:2,])%*%t(as.matrix(p1eigen$vectors)[,1:2])+one2%*%t(colm) # coordinates of the projections in the original space
p12proj
sqrt(rowSums((p12-p12proj)^2)) # Euclidean distance from the original data points

# h
p12-p12proj #errors

#################
# # Problem 2
#################

# the list of pictures (note the absence of 14 means that 31 corresponds to yaleB32)
#############################
# Code block from hw01.R solutions
# Includes a few modifications
#	- changed view_list
#	- refactored some variable names
#	- attached vector to matrix
pic_list = 1:38
view_list = c(  'P00A+000E+00', 'P00A+005E+10' , 'P00A+005E-10' , 'P00A+010E+00')
list_1 = dir(path="CroppedYale/",all.files=FALSE)
list_2 = dir(path="CroppedYale/",all.files=FALSE,recursive=TRUE)
s=vector()
for(i in 1:38){
  for(j in 1:4){
    filename = sprintf("CroppedYale/%s/%s_%s.pgm",
                       list_1[i] , list_1[i] , view_list[j])
    face=read.pnm(file=filename)
    chan=getChannels(face)
    vec=as.vector(chan)
    s=rbind(s,vec)
    vec=vector()
  }
}
# End code block
###############################

#################
# # Problem 2a
#################
means<-as.matrix(colMeans(s)) # compute mean face
iden<-matrix(c(rep(1,152)),nrow = 152,ncol = 1)
s_cent<-s-iden%*%t(means) 
meanf<-matrix(means,nrow = 192,ncol = 168)
meanface<-pixmapGrey(meanf)
plot(meanface)  # display the mean face
title('hw02_02a: mean face') 
filename = 'hw02_02a.png'  # save the photo
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# # Problem 2b
#################
pc<-prcomp(s_cent)  # PCA on image matrix
# Plot the number of components on the x-axis against 
# the proportion of the variance explained on the y-axis.
plot(seq(1:152),cumsum((pc$sdev)^2)/sum((pc$sdev)^2),
     xlab="component",ylab="variance captured (%)",
     main="variance captured against components number",type="b")
title('hw02_02b: variance captured')
filename = 'hw02_02b.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()
#################
# # Problem 2c
#################

#############################
# Code block from hw01.R solutions
# Includes a few modifications
#	- added if statements
#	- refactored some variable names
#	- attached vector to matrix
faces3=vector()
faces9=vector()
for(h in 1:9){
  chan2=pc$rotation[,h]
  faces=matrix(chan2,nrow = 192,ncol = 168)
  chan2=vector()
  faces3=cbind(faces3,faces)
  faces=vector()
  if(h%%3==0){
    faces9=rbind(faces9,faces3)
    faces3=vector()
  }
}
# Display the first 9 eigenfaces in a 3-by-3 grid.
plot(pixmapGrey(faces9))
title('hw02_02c: eigenfaces')
filename = 'hw02_02c.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()
# End code block
###############################

#################
# # Problem 2d
#################
# Use the eigenfaces to reconstruct yaleB05 P00A+010E+00.pgm
filename5 = "CroppedYale/yaleB05/yaleB05_P00A+010E+00.pgm"
face5=read.pnm(file=filename5)
plot(face5)
store1=as.character(store)
which(store1=="CroppedYale/yaleB05/yaleB05_P00A+010E+00.pgm")

#############################
# Code block from hw01.R solutions
# Includes a few modifications
#	- added if statements
#	- refactored some variable names
#	- attached vector to matrix
faces25=vector()
faces5=meanf # start with mean face
chan2<-as.numeric(c(rep(0,32256)))
# add in one eigen face at a time until reach 24 eigen faces
for(h in 1:24){
  chan2=chan2+pc$x[20,h]*pc$rotation[,h]
  
  faces=matrix(chan2,nrow = 192,ncol = 168)
  facesm=faces+meanf
  
  faces5=cbind(faces5,facesm)
  
  if(h%%5==4){
    faces25=rbind(faces25,faces5) # save it in 5x5 grid
    faces5=vector()
  }
}

plot(pixmapGrey(faces25))
title('hw02_02d: one face per time')
filename = 'hw02_02d.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()
# End code block
###############################

#############################
# Code block from hw01.R solutions
# Includes a few modifications
#	- added if statements
#	- refactored some variable names
#	- attached vector to matrix
faces25=vector()
faces5=meanf
chan2<-as.numeric(c(rep(0,32256)))
# add in 5 eigen faces at a time until reach 120 eigen faces
for(h in 0:23){
  chan2=chan2+pc$x[20,5*h+1]*pc$rotation[,5*h+1]+pc$x[20,5*h+2]*pc$rotation[,5*h+2]+pc$x[20,5*h+3]*pc$rotation[,5*h+3]+pc$x[20,5*h+4]*pc$rotation[,5*h+4]+pc$x[20,5*(h+1)]*pc$rotation[,5*(h+1)]
  
  faces=matrix(chan2,nrow = 192,ncol = 168)
  facesm=faces+meanf
  faces5=cbind(faces5,facesm)
  
  if(h%%5==3){
    faces25=rbind(faces25,faces5) # save the result in 5x5 grid
    faces5=vector()
  }
}

plot(pixmapGrey(faces25))
title('hw02_02d: five faces per time')
filename = 'hw02_02d2.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()
# End code block
###############################

#################
# # Problem 2e
#################
# Remove the pictures of subject 01 from your image matrix 
# (there should be four pictures of him)
which(grepl("yaleB01",store1))
s1<-s[-c(1:4),]
dim(s1)
# recenter data
means1<-as.matrix(colMeans(s1))
iden1<-matrix(c(rep(1,148)),nrow = 148,ncol = 1)
# subtract off the mean face
s1_cent<-s1-iden1%*%t(means1)
meanf1<-matrix(means1,nrow = 192,ncol = 168)
# Rerun prcomp() to get new principal components.
pc1<-prcomp(s1_cent)
head(store1)
s01<-s[4,]
s01_cent<-s01-means1
reconstruct<-means1
for(i in 1:148){
  reconstruct<-reconstruct+pc1$rotation[,i]%*%s01_cent*pc1$rotation[,i]
}
reconstructf<-matrix(reconstruct,nrow=192,ncol=168)
plot(pixmapGrey(reconstructf))
title('hw02_02e: reconstruct face')
filename = 'hw02_02e.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

face01=read.pnm("CroppedYale/yaleB01/yaleB01_P00A+010E+00.pgm")
plot(face01)

#################
# # Problem 2f
#################
# load the image "balloons.pgm"
ball=read.pnm("balloons.pgm")
ball
plot(ball)
balloon = getChannels(ball)
# crop it to has the same size as the other photos
balloons=balloon[1:192,1:168]
plot(pixmapGrey(balloons))
balloons_cent=as.numeric(balloons)-means
recon=as.numeric(c(rep(0,32256)))
# Use the existing components reconstruct the image
for(i in 1:152){
  recon<-recon+pc$rotation[,i]%*%balloons_cent*pc$rotation[,i]
}
recon<-recon+means
recon<-matrix(recon,nrow=192,ncol=168)
plot(pixmapGrey(recon))
title('hw02_02f: reconstruct picture')
filename = 'hw02_02f.png'
dev.copy(device=png, file=filename, height=600, width=800)
dev.off()

#################
# # End of Script
#################