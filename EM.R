a <- read.table('gasch.dat')
require(mclust)

##### Data Clean #####
n = length(a[,1])		# Row    
m = 173		  	# Col    
b <- matrix(rep(0,n*m),n,m)
c <- matrix(rep(1,n*m),n,m)
means <- rep(0,m)

for(j in 1:m){	
	for(i in 1:n){	
		if(a[i,j] < 98){
			b[i,j] <- a[i,j]	
			c[i,j] <- 0	
		}
	}
	N <- n-sum(c[,j])
	means[j] <- sum(b[,j])/N
	for(i in 1:n){	
		if(a[i,j] > 98){
			b[i,j] <- means[j]	
		}
	}
}

############ Sampling ###################
######## Re-ordering based on Variance
sigma2 <- matrix(rep(0,n),n,1)

for(i in 1:n){
	sigma2[i] <- var(b[i,])
}

B <- matrix(rep(0,n*(m+1)),n,(m+1))
B[1:n,1:m] <-b
B[1:n,m+1] <-sigma2
data <- B[order(B[,174], decreasing=TRUE),]

###### Experimental Samples

######### 500's ########

data1 <- data[1:500,1:173]
data2 <- data[501:1000,1:173]
data3 <- data[1001:1500,1:173]
data4 <- data[1501:2000,1:173]
data5 <- data[2001:2500,1:173]
data6 <- data[2501:3000,1:173]
data7 <- data[3001:3500,1:173]
data8 <- data[3501:4000,1:173]
data9 <- data[4001:4500,1:173]
data10 <- data[4501:5000,1:173]
data11 <- data[5001:5500,1:173]
data12 <- data[5501:6000,1:173]

######### Different sizes ########

data1T <- data[1:400,1:173]
data2T <- data[1:740,1:173]
data3T <- data[1:750,1:173]
data4T <- data[3001:4000,1:173]
data5T <- data[4001:5000,1:173]
data6T <- data[5001:6000,1:173]

############ Mclust ###################

####### Results

mclustModelNames

m1 <- Mclust(data=data1T, G=1:30, modelNames = mclust.options("emModelNames"))
m2 <- Mclust(data=data, G=1:20, modelNames = mclust.options("emModelNames"))
m5 <- Mclust(data=data5, G=1:20, modelNames = mclust.options("emModelNames"))

m1$parameters$pro
m1$parameters$mean[174,]
m1$parameters$variance
m1$loglik
m1$parameters$variance$scale
m1$parameters$variance$shape

m1$BIC
m1$bic

m1$uncertainty
m1$classification

m1$call
m1$z[1,]
m1$df

BIC <- 2*m1$loglik - m1$df*log(500)
BIC

mB1 <- mclustBIC(data=data12, G=1:10, modelNames = mclust.options("emModelNames"))
plot.mclustBIC(mB1)


####### Rand Sample

mRS <- Mclust(data=dataRS, G=1:20, modelNames = mclust.options("emModelNames"))

mRS$parameters$pro
mRS$parameters$mean
mRS$parameters$variance

mRS$parameters$variance$scale
mRS$parameters$variance$shape

mRS$BIC
mRS$bic

mRSB <- mclustBIC(data=dataRS, G=1:20, modelNames = mclust.options("emModelNames"))
plot.mclustBIC(mRSB)

######## Visualisation ###############

V <- rep(0,1,173)
for(i in 1:173){
	V[i] <- var(dataHV[,i])
}
V

m$z

m1 <- Mclust(data=data1T, G=1:20, modelNames = mclust.options("emModelNames"))
m2 <- Mclust(data=data2T, G=1:20, modelNames = mclust.options("emModelNames"))
m3 <- Mclust(data=data3T, G=1:20, modelNames = mclust.options("emModelNames"))
m4 <- Mclust(data=data4T, G=1:20, modelNames = mclust.options("emModelNames"))
m5 <- Mclust(data=data5T, G=1:20, modelNames = mclust.options("emModelNames"))
m6 <- Mclust(data=data6T, G=1:20, modelNames = mclust.options("emModelNames"))

mclust2Dplot(data1T[,c(13,173)], parameters = m1$parameters, z = m1$z,)
mclust2Dplot(data2[,c(13,100)], parameters = m2$parameters, z = m2$z,)
mclust2Dplot(data5[,c(13,100)], parameters = m5$parameters, z = m5$z,)
mclust2Dplot(dataRS[,c(21,122)], parameters = mRS$parameters, z = mRS$z,)

