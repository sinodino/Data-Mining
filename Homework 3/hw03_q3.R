#############################
# < Zhang Yunyan >
# STAT W4240 
# Homework <3> , Problem <3>
# < March 4th >
#
# The following code makes a knn regression function
# and plots the results
#############################

#################
# Setup
#################

# make sure R is in the proper working directory
# note that this will be a different path for every machine
#setwd("~/Documents/academic/teaching/STAT_W4240_2016_SPRG/hw/hw03")

# first include the relevant libraries
# note that a loading error might mean that you have to
# install the package into your R distribution.  

#################
# Problem 3a
#################

# write a function for knn

knn_regression = function(x_train, y_train, x_test, y_test, k_vec){
	
  # Coerce all inputs into matrices
	# (This allows us to use dim() even when input is a vector)
	x_train = as.matrix(x_train)
	x_test = as.matrix(x_test)
	
	# Compute dimensions and store values
	dim_train = dim(x_train)
	dim_test = dim(x_test)
	d = dim_train[2]
	n_test = dim_test[1]
	n_train = dim_train[1]
	k_len = length(k_vec)
	
	# Pre-allocate outputs for speed
	test_mse = rep(0,k_len)
	train_mse = rep(0,k_len)
	
	#----- START YOUR CODE BLOCK HERE -----#
	#samp<-sqrt((dat[2819,2]-dat[1,2])^2+(dat[8433,1]-dat[10002,1])^2)
	#sampmin<-sqrt((dat[1,1]-dat[6311,1])^2+(dat[6311,2]-dat[6311,2])^2)
	#dis<-apply(x_train,1,dist)
	#diss<-as.matrix(dist(x_train,upper = TRUE))
	dis<-as.matrix(dist(rbind(x_train,x_test),upper=TRUE))
	diss<-dis[c(1:10000),c(1:10000)]
	diss2<-dis[-c(10001:15000),-c(1:10000)]
	orde<-apply(diss,1,order)
	orde2<-apply(diss2,2,order)
	trainy_est<-vector()
	for (i in 1:k_len)
	{
	  sum_mse=0
	  for(j in 1:n_train)
	  {
	    trainy_est[j]<-mean(y_train[orde[1:k_vec[i],j]])
	    sum_mse<-sum_mse+(y_train[j]-trainy_est[j])^2
	  }
    train_mse[i]<-sum_mse/n_train
    y_testest<-vector()
    sum_testmse<-0
    li<-vector()
    for(j in 1:n_test)
    {
      #dis_test<-apply(rbind(x_test[j,],x_train),2,dist)[1:n_test,]
      #distest<-sum(sqrt(dis_test^2))
      #for(p in 1:n_train)
      # {
      #    dis_test[p]<-sqrt(sum((x_test[j,]-x_train[p,])^2))
      # }
      #ord<-order(dis_test)
      y_testest[j]<-mean(y_train[orde2[1:k_vec[i],j]])
      sum_testmse<-sum_testmse+(y_test[j]-y_testest[j])^2
      li<-rbind(li,sum_testmse)
    }
    test_mse[i]<-sum_testmse/n_test
	}

	#----- END YOUR CODE BLOCK HERE -----#
	
	# Store the outputs in a list and return
	output_list = list()
	output_list[["train_mse"]] = train_mse
	output_list[["test_mse"]] = test_mse
	return(output_list)
	
}

# Load the data and plot the results
#----- START YOUR CODE BLOCK HERE -----#
dat <- read.csv("hw03_q3.csv",header=T)
x_train<-dat[1:10000,2]
y_train<-dat[1:10000,1]
x_test<-dat[10001:15000,2]
y_test<-dat[10001:15000,1]

k_vec<-c(1,5,10,50,100,500,1000,5000,10000)
ptm<-proc.time()
res<-knn_regression(x_train,y_train,x_test,y_test,k_vec)
proc.time()-ptm
matplot(k_vec,cbind(res[["train_mse"]],res[["test_mse"]]),log="x",type="l",col=1:2,xlab="k",ylab="MSE")
legend("topleft", legend = c("train MSE","test MSE"), col=1:2, lty=1:2,pch=1)
plot(k_vec,test_mse,log="x",type = "l",lty=1,main='MSE')
lines(k_vec,train_mse,log="x",type="l",lty=2)
plot(k_vec,res[["train_mse"]],log="x",type = "b",color='red',main='MSE')
lines(k_vec,res[["test_mse"]],log="x",type="b",color='green')
#----- END YOUR CODE BLOCK HERE -----#


#################
# Problem 3c
#################

#----- START YOUR CODE BLOCK HERE -----#
k_vec<-c(1,5,10,50,100,500,1000,5000,9999)
min_mse=vector()
min_k=vector()
for(i in 2:101)
{
  x_train<-dat[1:10000,c(2:i)]
  #y_train<-dat[1:10000,1]
  x_test<-dat[10001:15000,c(2:i)]
  #y_test<-dat[10001:15000,1]
  ptm<-proc.time()
  res<-knn_regression(x_train,y_train,x_test,y_test,k_vec)
  proc.time()-ptm
  min_mse[i-1]=min(res[["test_mse"]])
  min_k[i-1]=k_vec[which.min(res[["test_mse"]])]
}
plot(seq(1:100),min_mse,main="Min Test MSE",xlab="number of covariates",ylab="Min Mse",type="l")
plot(seq(1:100),min_k,main="Min k value",xlab="number of covariates",ylab="Min k",type="b")


#----- END YOUR CODE BLOCK HERE -----#

#################
# End of Script
#################


