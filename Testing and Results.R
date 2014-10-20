require(mclust)

a <- read.table('gasch.dat')

############ Data Clean ############
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

############ Re-ordering based on Variance ############
sigma2 <- matrix(rep(0,n),n,1)

for(i in 1:n){
	sigma2[i] <- var(b[i,])
}

B <- matrix(rep(0,n*(m+1)),n,(m+1))
B[1:n,1:m] <-b
B[1:n,m+1] <-sigma2
data <- B[order(B[,174], decreasing=TRUE),]


###### Greatest Variance #######
data1 <- data[1:200,1:173]
data2 <- data[1:250,1:173]
data3 <- data[1:300,1:173]
data4 <- data[1:350,1:173]
data5 <- data[1:400,1:173]
data6 <- data[1:450,1:173]
data7 <- data[1:500,1:173]
data8 <- data[1:550,1:173]
data9 <- data[1:600,1:173]
data10 <- data[1:650,1:173]
data11 <- data[1:700,1:173]
data12 <- data[1:750,1:173]
data13 <- data[1:800,1:173]
data14 <- data[1:850,1:173]
data15 <- data[1:900,1:173]
data16 <- data[1:950,1:173]
data17 <- data[1:1000,1:173]

m1 <- Mclust(data=data1, G=1:20, modelNames = mclust.options("emModelNames"))
m2 <- Mclust(data=data2, G=1:20, modelNames = mclust.options("emModelNames"))
m3 <- Mclust(data=data3, G=1:20, modelNames = mclust.options("emModelNames"))
m4 <- Mclust(data=data4, G=1:20, modelNames = mclust.options("emModelNames"))
m5 <- Mclust(data=data5, G=1:20, modelNames = mclust.options("emModelNames"))
m6 <- Mclust(data=data6, G=1:20, modelNames = mclust.options("emModelNames"))
m7 <- Mclust(data=data7, G=1:20, modelNames = mclust.options("emModelNames"))
m8 <- Mclust(data=data8, G=1:20, modelNames = mclust.options("emModelNames"))
m9 <- Mclust(data=data9, G=1:20, modelNames = mclust.options("emModelNames"))
m10 <- Mclust(data=data10, G=1:20, modelNames = mclust.options("emModelNames"))
m11 <- Mclust(data=data11, G=1:20, modelNames = mclust.options("emModelNames"))
m12 <- Mclust(data=data12, G=1:20, modelNames = mclust.options("emModelNames"))
m13 <- Mclust(data=data13, G=1:20, modelNames = mclust.options("emModelNames"))
m14 <- Mclust(data=data14, G=1:20, modelNames = mclust.options("emModelNames"))
m15 <- Mclust(data=data15, G=1:20, modelNames = mclust.options("emModelNames"))
m16 <- Mclust(data=data16, G=1:20, modelNames = mclust.options("emModelNames"))
m17 <- Mclust(data=data17, G=1:20, modelNames = mclust.options("emModelNames"))


G <- matrix(1:(10*17), 17,10)
for(i in 1:10){
	G[1,i] <- which.max(m1$BIC[,i])
	G[2,i] <- which.max(m2$BIC[,i])
	G[3,i] <- which.max(m3$BIC[,i])
	G[4,i] <- which.max(m4$BIC[,i])
	G[5,i] <- which.max(m5$BIC[,i])
	G[6,i] <- which.max(m6$BIC[,i])
	G[7,i] <- which.max(m7$BIC[,i])
	G[8,i] <- which.max(m8$BIC[,i])
	G[9,i] <- which.max(m9$BIC[,i])
	G[10,i] <- which.max(m10$BIC[,i])
	G[11,i] <- which.max(m11$BIC[,i])
	G[12,i] <- which.max(m12$BIC[,i])
	G[13,i] <- which.max(m13$BIC[,i])
	G[14,i] <- which.max(m14$BIC[,i])
	G[15,i] <- which.max(m15$BIC[,i])
	G[16,i] <- which.max(m16$BIC[,i])
	G[17,i] <- which.max(m17$BIC[,i])
}

colour <- rainbow(10,start=0)
plot(c(1:17),G[,1], xlim=c(1,17), ylim=c(0,20), col=colour[1], ylab="Number of Clusters", xlab="Sample Number", lwd=2)
lines(c(1:17),G[,1], xlim=c(1,17), ylim=c(0,20), col=colour[1], ylab="", xlab="", lwd=2)
par(new=T)

for(i in 2:10){
	plot(c(1:17),G[,i], xlim=c(1,17), ylim=c(0,20), col=colour[i], ylab="", xlab="", lwd=2)
	lines(c(1:17),G[,i], xlim=c(1,17), ylim=c(0,20), col=colour[i], ylab="", xlab="", lwd=2)
	par(new=T)
}

G.best <- c(m1$G, m2$G, m3$G, m4$G, m5$G, m6$G, m7$G, m8$G, m9$G, m10$G, m11$G, m12$G, m13$G, m14$G, m15$G, m16$G, m17$G) 
plot(c(1:17),G.best, xlim=c(1,17), ylim=c(0,20), pch=16,  ylab="", xlab="")
legend(9, 6, c("EII","VII","EEI","VEI","EVI","VVI","EEE","EEV","VEV","VVV", "Optimal"), pch=16, ncol=3, col=c(colour[1:10], "black"))

B <- c(m1$bic, m2$bic, m3$bic, m4$bic, m5$bic, m6$bic, m7$bic, m8$bic, m9$bic, m10$bic, 
	m11$bic, m12$bic, m13$bic, m14$bic, m15$bic, m16$bic, m17$bic)
plot(B, ylab="BIC", xlab="Sample Number")
lines(B, ylab="BIC", xlab="Sample Number")

J <- matrix(1:(10*17), 17,10)
for(i in 1:10){
	J[1,i]  <- max(m1$BIC[,i], na.rm = TRUE)
	J[2,i]  <- max(m2$BIC[,i], na.rm = TRUE)
	J[3,i]  <- max(m3$BIC[,i], na.rm = TRUE)
	J[4,i]  <- max(m4$BIC[,i], na.rm = TRUE)
	J[5,i]  <- max(m5$BIC[,i], na.rm = TRUE)
	J[6,i]  <- max(m6$BIC[,i], na.rm = TRUE)
	J[7,i]  <- max(m7$BIC[,i], na.rm = TRUE)
	J[8,i]  <- max(m8$BIC[,i], na.rm = TRUE)
	J[9,i]  <- max(m9$BIC[,i], na.rm = TRUE)
	J[10,i] <- max(m10$BIC[,i], na.rm = TRUE)
	J[11,i] <- max(m11$BIC[,i], na.rm = TRUE)
	J[12,i] <- max(m12$BIC[,i], na.rm = TRUE)
	J[13,i] <- max(m13$BIC[,i], na.rm = TRUE)
	J[14,i] <- max(m14$BIC[,i], na.rm = TRUE)
	J[15,i] <- max(m15$BIC[,i], na.rm = TRUE)
	J[16,i] <- max(m16$BIC[,i], na.rm = TRUE)
	J[17,i] <- max(m17$BIC[,i], na.rm = TRUE)
}

colour <- rainbow(10,start=0)
plot(c(1:17),J[,1], xlim=c(1,17), ylim=c(-400000,-80000), col=colour[1], ylab="BIC", xlab="Sample Number", lwd=2)
lines(c(1:17),J[,1], xlim=c(1,17), ylim=c(-400000,-80000), col=colour[1], ylab="", xlab="", lwd=2)
par(new=T)

for(i in 2:10){
	plot(c(1:17),J[,i], xlim=c(1,17), ylim=c(-400000,-80000), col=colour[i], ylab="", xlab="", lwd=2)
	lines(c(1:17),J[,i], xlim=c(1,17), ylim=c(-400000,-80000), col=colour[i], ylab="", xlab="", lwd=2)
	par(new=T)
}
legend(1, -300000, c("EII","VII","EEI","VEI","EVI","VVI","EEE","EEV","VEV","VVV"), pch=16, ncol=3, col=c(colour[1:10]))

###### Smallest Variance #######

dataLV1 <- data[5952:6152,1:173]
dataLV2 <- data[5902:6152,1:173]
dataLV3 <- data[5852:6152,1:173]
dataLV4 <- data[5802:6152,1:173]
dataLV5 <- data[5752:6152,1:173]
dataLV6 <- data[5702:6152,1:173]
dataLV7 <- data[5652:6152,1:173]
dataLV8 <- data[5602:6152,1:173]
dataLV9 <- data[5552:6152,1:173]
dataLV10 <- data[5502:6152,1:173]
dataLV11 <- data[5452:6152,1:173]
dataLV12 <- data[5402:6152,1:173]
dataLV13 <- data[5352:6152,1:173]
dataLV14 <- data[5302:6152,1:173]
dataLV15 <- data[5252:6152,1:173]
dataLV16 <- data[5202:6152,1:173]
dataLV17 <- data[5152:6152,1:173]

mLV1 <- Mclust(data=dataLV1, G=1:20, modelNames = mclust.options("emModelNames"))
mLV2 <- Mclust(data=dataLV2, G=1:20, modelNames = mclust.options("emModelNames"))
mLV3 <- Mclust(data=dataLV3, G=1:20, modelNames = mclust.options("emModelNames"))
mLV4 <- Mclust(data=dataLV4, G=1:20, modelNames = mclust.options("emModelNames"))
mLV5 <- Mclust(data=dataLV5, G=1:20, modelNames = mclust.options("emModelNames"))
mLV6 <- Mclust(data=dataLV6, G=1:20, modelNames = mclust.options("emModelNames"))
mLV7 <- Mclust(data=dataLV7, G=1:20, modelNames = mclust.options("emModelNames"))
mLV8 <- Mclust(data=dataLV8, G=1:20, modelNames = mclust.options("emModelNames"))
mLV9 <- Mclust(data=dataLV9, G=1:20, modelNames = mclust.options("emModelNames"))
mLV10 <- Mclust(data=dataLV10, G=1:20, modelNames = mclust.options("emModelNames"))
mLV11 <- Mclust(data=dataLV11, G=1:20, modelNames = mclust.options("emModelNames"))
mLV12 <- Mclust(data=dataLV12, G=1:20, modelNames = mclust.options("emModelNames"))
mLV13 <- Mclust(data=dataLV13, G=1:20, modelNames = mclust.options("emModelNames"))
mLV14 <- Mclust(data=dataLV14, G=1:20, modelNames = mclust.options("emModelNames"))
mLV15 <- Mclust(data=dataLV15, G=1:20, modelNames = mclust.options("emModelNames"))
mLV16 <- Mclust(data=dataLV16, G=1:20, modelNames = mclust.options("emModelNames"))
mLV17 <- Mclust(data=dataLV17, G=1:20, modelNames = mclust.options("emModelNames"))


G <- matrix(1:(10*17), 17,10)
for(i in 1:10){
	G[1,i] <- which.max(mLV1$BIC[,i])
	G[2,i] <- which.max(mLV2$BIC[,i])
	G[3,i] <- which.max(mLV3$BIC[,i])
	G[4,i] <- which.max(mLV4$BIC[,i])
	G[5,i] <- which.max(mLV5$BIC[,i])
	G[6,i] <- which.max(mLV6$BIC[,i])
	G[7,i] <- which.max(mLV7$BIC[,i])
	G[8,i] <- which.max(mLV8$BIC[,i])
	G[9,i] <- which.max(mLV9$BIC[,i])
	G[10,i] <- which.max(mLV10$BIC[,i])
	G[11,i] <- which.max(mLV11$BIC[,i])
	G[12,i] <- which.max(mLV12$BIC[,i])
	G[13,i] <- which.max(mLV13$BIC[,i])
	G[14,i] <- which.max(mLV14$BIC[,i])
	G[15,i] <- which.max(mLV15$BIC[,i])
	G[16,i] <- which.max(mLV16$BIC[,i])
	G[17,i] <- which.max(mLV17$BIC[,i])
}

colour <- rainbow(10,start=0)
plot(c(1:17),G[,1], xlim=c(1,17), ylim=c(0,15), col=colour[1], ylab="Number of Clusters", xlab="Sample Number",lwd=2)
lines(c(1:17),G[,1], xlim=c(1,17), ylim=c(0,15), col=colour[1], ylab="", xlab="",lwd=2)
par(new=T)

for(i in 2:10){
	plot(c(1:17),G[,i], xlim=c(1,17), ylim=c(0,15), col=colour[i], ylab="", xlab="",lwd=2)
	lines(c(1:17),G[,i], xlim=c(1,17), ylim=c(0,15), col=colour[i], ylab="", xlab="",lwd=2)
	par(new=T)
}

G.best <- c(mLV1$G, mLV2$G, mLV3$G, mLV4$G, mLV5$G, mLV6$G, mLV7$G, mLV8$G, mLV9$G, mLV10$G, mLV11$G, mLV12$G, mLV13$G, mLV14$G, mLV15$G, mLV16$G, mLV17$G) 
plot(c(1:17),G.best, xlim=c(1,17), ylim=c(0,15), pch=16,  ylab="", xlab="",lwd=2)
legend(1, 15, c("EII","VII","EEI","VEI","EVI","VVI","EEE","EEV","VEV","VVV", "Optimal"), pch=16, ncol=3, col=c(colour[1:10], "black"))

B <- c(m1$bic, m2$bic, m3$bic, m4$bic, m5$bic, m6$bic, m7$bic, m8$bic, m9$bic, m10$bic, 
	m11$bic, m12$bic, m13$bic, m14$bic, m15$bic, m16$bic, m17$bic)
plot(B, ylab="BIC", xlab="Sample Number")
lines(B, ylab="BIC", xlab="Sample Number")

J <- matrix(1:(10*17), 17,10)
for(i in 1:10){
	J[1,i]  <- max(mLV1$BIC[,i], na.rm = TRUE)
	J[2,i]  <- max(mLV2$BIC[,i], na.rm = TRUE)
	J[3,i]  <- max(mLV3$BIC[,i], na.rm = TRUE)
	J[4,i]  <- max(mLV4$BIC[,i], na.rm = TRUE)
	J[5,i]  <- max(mLV5$BIC[,i], na.rm = TRUE)
	J[6,i]  <- max(mLV6$BIC[,i], na.rm = TRUE)
	J[7,i]  <- max(mLV7$BIC[,i], na.rm = TRUE)
	J[8,i]  <- max(mLV8$BIC[,i], na.rm = TRUE)
	J[9,i]  <- max(mLV9$BIC[,i], na.rm = TRUE)
	J[10,i] <- max(mLV10$BIC[,i], na.rm = TRUE)
	J[11,i] <- max(mLV11$BIC[,i], na.rm = TRUE)
	J[12,i] <- max(mLV12$BIC[,i], na.rm = TRUE)
	J[13,i] <- max(mLV13$BIC[,i], na.rm = TRUE)
	J[14,i] <- max(mLV14$BIC[,i], na.rm = TRUE)
	J[15,i] <- max(mLV15$BIC[,i], na.rm = TRUE)
	J[16,i] <- max(mLV16$BIC[,i], na.rm = TRUE)
	J[17,i] <- max(mLV17$BIC[,i], na.rm = TRUE)
}

colour <- rainbow(10,start=0)
plot(c(1:17),J[,1], xlim=c(1,17), ylim=c(-160000,-10000), col=colour[1], ylab="BIC", xlab="Sample Number", lwd=2)
lines(c(1:17),J[,1], xlim=c(1,17), ylim=c(-160000,-10000), col=colour[1], ylab="", xlab="", lwd=2)
par(new=T)

for(i in 2:10){
	plot(c(1:17),J[,i], xlim=c(1,17), ylim=c(-160000,-10000), col=colour[i], ylab="", xlab="", lwd=2)
	lines(c(1:17),J[,i], xlim=c(1,17), ylim=c(-160000,-10000), col=colour[i], ylab="", xlab="", lwd=2)
	par(new=T)
}
legend(1, -100000, c("EII","VII","EEI","VEI","EVI","VVI","EEE","EEV","VEV","VVV"), pch=16, ncol=3, col=c(colour[1:10]))


########### Random Sample ###############

## 200
data1.1 <- data[sample(c(1:6152),200),1:173]
data1.2 <- data[sample(c(1:6152),200),1:173]
data1.3 <- data[sample(c(1:6152),200),1:173]
data1.4 <- data[sample(c(1:6152),200),1:173]
data1.5 <- data[sample(c(1:6152),200),1:173]

m1.1 <- Mclust(data=data1.1, G=1:20, modelNames = mclust.options("emModelNames"))
m1.2 <- Mclust(data=data1.2, G=1:20, modelNames = mclust.options("emModelNames"))
m1.3 <- Mclust(data=data1.3, G=1:20, modelNames = mclust.options("emModelNames"))
m1.4 <- Mclust(data=data1.4, G=1:20, modelNames = mclust.options("emModelNames"))
m1.5 <- Mclust(data=data1.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 250
data2.1 <- data[sample(c(1:6152),250),1:173]
data2.2 <- data[sample(c(1:6152),250),1:173]
data2.3 <- data[sample(c(1:6152),250),1:173]
data2.4 <- data[sample(c(1:6152),250),1:173]
data2.5 <- data[sample(c(1:6152),250),1:173]

m2.1 <- Mclust(data=data2.1, G=1:20, modelNames = mclust.options("emModelNames"))
m2.2 <- Mclust(data=data2.2, G=1:20, modelNames = mclust.options("emModelNames"))
m2.3 <- Mclust(data=data2.3, G=1:20, modelNames = mclust.options("emModelNames"))
m2.4 <- Mclust(data=data2.4, G=1:20, modelNames = mclust.options("emModelNames"))
m2.5 <- Mclust(data=data2.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 300
data3.1 <- data[sample(c(1:6152),300),1:173]
data3.2 <- data[sample(c(1:6152),300),1:173]
data3.3 <- data[sample(c(1:6152),300),1:173]
data3.4 <- data[sample(c(1:6152),300),1:173]
data3.5 <- data[sample(c(1:6152),300),1:173]

m3.1 <- Mclust(data=data3.1, G=1:20, modelNames = mclust.options("emModelNames"))
m3.2 <- Mclust(data=data3.2, G=1:20, modelNames = mclust.options("emModelNames"))
m3.3 <- Mclust(data=data3.3, G=1:20, modelNames = mclust.options("emModelNames"))
m3.4 <- Mclust(data=data3.4, G=1:20, modelNames = mclust.options("emModelNames"))
m3.5 <- Mclust(data=data3.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 350
data4.1 <- data[sample(c(1:6152),350),1:173]
data4.2 <- data[sample(c(1:6152),350),1:173]
data4.3 <- data[sample(c(1:6152),350),1:173]
data4.4 <- data[sample(c(1:6152),350),1:173]
data4.5 <- data[sample(c(1:6152),350),1:173]

m4.1 <- Mclust(data=data4.1, G=1:20, modelNames = mclust.options("emModelNames"))
m4.2 <- Mclust(data=data4.2, G=1:20, modelNames = mclust.options("emModelNames"))
m4.3 <- Mclust(data=data4.3, G=1:20, modelNames = mclust.options("emModelNames"))
m4.4 <- Mclust(data=data4.4, G=1:20, modelNames = mclust.options("emModelNames"))
m4.5 <- Mclust(data=data4.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 400
data5.1 <- data[sample(c(1:6152),400),1:173]
data5.2 <- data[sample(c(1:6152),400),1:173]
data5.3 <- data[sample(c(1:6152),400),1:173]
data5.4 <- data[sample(c(1:6152),400),1:173]
data5.5 <- data[sample(c(1:6152),400),1:173]

m5.1 <- Mclust(data=data5.1, G=1:20, modelNames = mclust.options("emModelNames"))
m5.2 <- Mclust(data=data5.2, G=1:20, modelNames = mclust.options("emModelNames"))
m5.3 <- Mclust(data=data5.3, G=1:20, modelNames = mclust.options("emModelNames"))
m5.4 <- Mclust(data=data5.4, G=1:20, modelNames = mclust.options("emModelNames"))
m5.5 <- Mclust(data=data5.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 450
data6.1 <- data[sample(c(1:6152),450),1:173]
data6.2 <- data[sample(c(1:6152),450),1:173]
data6.3 <- data[sample(c(1:6152),450),1:173]
data6.4 <- data[sample(c(1:6152),450),1:173]
data6.5 <- data[sample(c(1:6152),450),1:173]

m6.1 <- Mclust(data=data6.1, G=1:20, modelNames = mclust.options("emModelNames"))
m6.2 <- Mclust(data=data6.2, G=1:20, modelNames = mclust.options("emModelNames"))
m6.3 <- Mclust(data=data6.3, G=1:20, modelNames = mclust.options("emModelNames"))
m6.4 <- Mclust(data=data6.4, G=1:20, modelNames = mclust.options("emModelNames"))
m6.5 <- Mclust(data=data6.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 500
data7.1 <- data[sample(c(1:6152),500),1:173]
data7.2 <- data[sample(c(1:6152),500),1:173]
data7.3 <- data[sample(c(1:6152),500),1:173]
data7.4 <- data[sample(c(1:6152),500),1:173]
data7.5 <- data[sample(c(1:6152),500),1:173]

m7.1 <- Mclust(data=data7.1, G=1:20, modelNames = mclust.options("emModelNames"))
m7.2 <- Mclust(data=data7.2, G=1:20, modelNames = mclust.options("emModelNames"))
m7.3 <- Mclust(data=data7.3, G=1:20, modelNames = mclust.options("emModelNames"))
m7.4 <- Mclust(data=data7.4, G=1:20, modelNames = mclust.options("emModelNames"))
m7.5 <- Mclust(data=data7.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 550
data8.1 <- data[sample(c(1:6152),550),1:173]
data8.2 <- data[sample(c(1:6152),550),1:173]
data8.3 <- data[sample(c(1:6152),550),1:173]
data8.4 <- data[sample(c(1:6152),550),1:173]
data8.5 <- data[sample(c(1:6152),550),1:173]

m8.1 <- Mclust(data=data8.1, G=1:20, modelNames = mclust.options("emModelNames"))
m8.2 <- Mclust(data=data8.2, G=1:20, modelNames = mclust.options("emModelNames"))
m8.3 <- Mclust(data=data8.3, G=1:20, modelNames = mclust.options("emModelNames"))
m8.4 <- Mclust(data=data8.4, G=1:20, modelNames = mclust.options("emModelNames"))
m8.5 <- Mclust(data=data8.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 600
data9.1 <- data[sample(c(1:6152),600),1:173]
data9.2 <- data[sample(c(1:6152),600),1:173]
data9.3 <- data[sample(c(1:6152),600),1:173]
data9.4 <- data[sample(c(1:6152),600),1:173]
data9.5 <- data[sample(c(1:6152),600),1:173]

m9.1 <- Mclust(data=data9.1, G=1:20, modelNames = mclust.options("emModelNames"))
m9.2 <- Mclust(data=data9.2, G=1:20, modelNames = mclust.options("emModelNames"))
m9.3 <- Mclust(data=data9.3, G=1:20, modelNames = mclust.options("emModelNames"))
m9.4 <- Mclust(data=data9.4, G=1:20, modelNames = mclust.options("emModelNames"))
m9.5 <- Mclust(data=data9.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 650
data10.1 <- data[sample(c(1:6152),650),1:173]
data10.2 <- data[sample(c(1:6152),650),1:173]
data10.3 <- data[sample(c(1:6152),650),1:173]
data10.4 <- data[sample(c(1:6152),650),1:173]
data10.5 <- data[sample(c(1:6152),650),1:173]

m10.1 <- Mclust(data=data10.1, G=1:20, modelNames = mclust.options("emModelNames"))
m10.2 <- Mclust(data=data10.2, G=1:20, modelNames = mclust.options("emModelNames"))
m10.3 <- Mclust(data=data10.3, G=1:20, modelNames = mclust.options("emModelNames"))
m10.4 <- Mclust(data=data10.4, G=1:20, modelNames = mclust.options("emModelNames"))
m10.5 <- Mclust(data=data10.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 700
data11.1 <- data[sample(c(1:6152),700),1:173]
data11.2 <- data[sample(c(1:6152),700),1:173]
data11.3 <- data[sample(c(1:6152),700),1:173]
data11.4 <- data[sample(c(1:6152),700),1:173]
data11.5 <- data[sample(c(1:6152),700),1:173]

m11.1 <- Mclust(data=data11.1, G=1:20, modelNames = mclust.options("emModelNames"))
m11.2 <- Mclust(data=data11.2, G=1:20, modelNames = mclust.options("emModelNames"))
m11.3 <- Mclust(data=data11.3, G=1:20, modelNames = mclust.options("emModelNames"))
m11.4 <- Mclust(data=data11.4, G=1:20, modelNames = mclust.options("emModelNames"))
m11.5 <- Mclust(data=data11.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 750
data12.1 <- data[sample(c(1:6152),750),1:173]
data12.2 <- data[sample(c(1:6152),750),1:173]
data12.3 <- data[sample(c(1:6152),750),1:173]
data12.4 <- data[sample(c(1:6152),750),1:173]
data12.5 <- data[sample(c(1:6152),750),1:173]

m12.1 <- Mclust(data=data12.1, G=1:20, modelNames = mclust.options("emModelNames"))
m12.2 <- Mclust(data=data12.2, G=1:20, modelNames = mclust.options("emModelNames"))
m12.3 <- Mclust(data=data12.3, G=1:20, modelNames = mclust.options("emModelNames"))
m12.4 <- Mclust(data=data12.4, G=1:20, modelNames = mclust.options("emModelNames"))
m12.5 <- Mclust(data=data12.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 800
data13.1 <- data[sample(c(1:6152),800),1:173]
data13.2 <- data[sample(c(1:6152),800),1:173]
data13.3 <- data[sample(c(1:6152),800),1:173]
data13.4 <- data[sample(c(1:6152),800),1:173]
data13.5 <- data[sample(c(1:6152),800),1:173]

m13.1 <- Mclust(data=data13.1, G=1:20, modelNames = mclust.options("emModelNames"))
m13.2 <- Mclust(data=data13.2, G=1:20, modelNames = mclust.options("emModelNames"))
m13.3 <- Mclust(data=data13.3, G=1:20, modelNames = mclust.options("emModelNames"))
m13.4 <- Mclust(data=data13.4, G=1:20, modelNames = mclust.options("emModelNames"))
m13.5 <- Mclust(data=data13.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 850
data14.1 <- data[sample(c(1:6152),850),1:173]
data14.2 <- data[sample(c(1:6152),850),1:173]
data14.3 <- data[sample(c(1:6152),850),1:173]
data14.4 <- data[sample(c(1:6152),850),1:173]
data14.5 <- data[sample(c(1:6152),850),1:173]

m14.1 <- Mclust(data=data14.1, G=1:20, modelNames = mclust.options("emModelNames"))
m14.2 <- Mclust(data=data14.2, G=1:20, modelNames = mclust.options("emModelNames"))
m14.3 <- Mclust(data=data14.3, G=1:20, modelNames = mclust.options("emModelNames"))
m14.4 <- Mclust(data=data14.4, G=1:20, modelNames = mclust.options("emModelNames"))
m14.5 <- Mclust(data=data14.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 900
data15.1 <- data[sample(c(1:6152),900),1:173]
data15.2 <- data[sample(c(1:6152),900),1:173]
data15.3 <- data[sample(c(1:6152),900),1:173]
data15.4 <- data[sample(c(1:6152),900),1:173]
data15.5 <- data[sample(c(1:6152),900),1:173]

m15.1 <- Mclust(data=data15.1, G=1:20, modelNames = mclust.options("emModelNames"))
m15.2 <- Mclust(data=data15.2, G=1:20, modelNames = mclust.options("emModelNames"))
m15.3 <- Mclust(data=data15.3, G=1:20, modelNames = mclust.options("emModelNames"))
m15.4 <- Mclust(data=data15.4, G=1:20, modelNames = mclust.options("emModelNames"))
m15.5 <- Mclust(data=data15.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 950
data16.1 <- data[sample(c(1:6152),950),1:173]
data16.2 <- data[sample(c(1:6152),950),1:173]
data16.3 <- data[sample(c(1:6152),950),1:173]
data16.4 <- data[sample(c(1:6152),950),1:173]
data16.5 <- data[sample(c(1:6152),950),1:173]

m16.1 <- Mclust(data=data16.1, G=1:20, modelNames = mclust.options("emModelNames"))
m16.2 <- Mclust(data=data16.2, G=1:20, modelNames = mclust.options("emModelNames"))
m16.3 <- Mclust(data=data16.3, G=1:20, modelNames = mclust.options("emModelNames"))
m16.4 <- Mclust(data=data16.4, G=1:20, modelNames = mclust.options("emModelNames"))
m16.5 <- Mclust(data=data16.5, G=1:20, modelNames = mclust.options("emModelNames"))

## 1000
data17.1 <- data[sample(c(1:6152),1000),1:173]
data17.2 <- data[sample(c(1:6152),1000),1:173]
data17.3 <- data[sample(c(1:6152),1000),1:173]
data17.4 <- data[sample(c(1:6152),1000),1:173]
data17.5 <- data[sample(c(1:6152),1000),1:173]

m17.1 <- Mclust(data=data17.1, G=1:20, modelNames = mclust.options("emModelNames"))
m17.2 <- Mclust(data=data17.2, G=1:20, modelNames = mclust.options("emModelNames"))
m17.3 <- Mclust(data=data17.3, G=1:20, modelNames = mclust.options("emModelNames"))
m17.4 <- Mclust(data=data17.4, G=1:20, modelNames = mclust.options("emModelNames"))
m17.5 <- Mclust(data=data17.5, G=1:20, modelNames = mclust.options("emModelNames"))


B <- c(     ((m1.1$bic +  m1.2$bic  + m1.3$bic  + m1.4$bic  + m1.5$bic)/5),
		((m2.1$bic +  m2.2$bic  + m2.3$bic  + m2.4$bic  + m2.5$bic)/5),
		((m3.1$bic +  m3.2$bic  + m3.3$bic  + m3.4$bic  + m3.5$bic)/5),
		((m4.1$bic +  m4.2$bic  + m4.3$bic  + m4.4$bic  + m4.5$bic)/5),
		((m5.1$bic +  m5.2$bic  + m5.3$bic  + m5.4$bic  + m5.5$bic)/5),
		((m6.1$bic +  m6.2$bic  + m6.3$bic  + m6.4$bic  + m6.5$bic)/5),
		((m7.1$bic +  m7.2$bic  + m7.3$bic  + m7.4$bic  + m7.5$bic)/5),
		((m8.1$bic +  m8.2$bic  + m8.3$bic  + m8.4$bic  + m8.5$bic)/5),
		((m9.1$bic +  m9.2$bic  + m9.3$bic  + m9.4$bic  + m9.5$bic)/5),
		((m10.1$bic + m10.2$bic + m10.3$bic + m10.4$bic + m10.5$bic)/5),
		((m11.1$bic + m11.2$bic + m11.3$bic + m11.4$bic + m11.5$bic)/5),
		((m12.1$bic + m12.2$bic + m12.3$bic + m12.4$bic + m12.5$bic)/5),
		((m13.1$bic + m13.2$bic + m13.3$bic + m13.4$bic + m13.5$bic)/5),
		((m14.1$bic + m14.2$bic + m14.3$bic + m14.4$bic + m14.5$bic)/5),
		((m15.1$bic + m15.2$bic + m15.3$bic + m15.4$bic + m15.5$bic)/5),
		((m16.1$bic + m16.2$bic + m16.3$bic + m16.4$bic + m16.5$bic)/5),
		((m17.1$bic + m17.2$bic + m17.3$bic + m17.4$bic + m17.5$bic)/5) )

G <- matrix(1:(10*17), 17,10)
for(i in 1:10) {
	G[1,i]  <- (which.max(m1.1$BIC[,i]) + 
			which.max(m1.2$BIC[,i]) + 
			which.max(m1.3$BIC[,i]) + 
			which.max(m1.4$BIC[,i]) + 
			which.max(m1.5$BIC[,i]) ) /5
	G[2,i]  <- (which.max(m2.1$BIC[,i]) + 
			which.max(m2.2$BIC[,i]) + 
			which.max(m2.3$BIC[,i]) + 
			which.max(m2.4$BIC[,i]) + 
			which.max(m2.5$BIC[,i]) ) /5
	G[3,i]  <- (which.max(m3.1$BIC[,i]) + 
			which.max(m3.2$BIC[,i]) + 
			which.max(m3.3$BIC[,i]) + 
			which.max(m3.4$BIC[,i]) + 
			which.max(m3.5$BIC[,i]) ) /5
	G[4,i]  <- (which.max(m4.1$BIC[,i]) + 
			which.max(m4.2$BIC[,i]) + 
			which.max(m4.3$BIC[,i]) + 
			which.max(m4.4$BIC[,i]) + 
			which.max(m4.5$BIC[,i]) ) /5
	G[5,i]  <- (which.max(m5.1$BIC[,i]) + 
			which.max(m5.2$BIC[,i]) + 
			which.max(m5.3$BIC[,i]) + 
			which.max(m5.4$BIC[,i]) + 
			which.max(m5.5$BIC[,i]) ) /5
	G[6,i]  <- (which.max(m6.1$BIC[,i]) + 
			which.max(m6.2$BIC[,i]) + 
			which.max(m6.3$BIC[,i]) + 
			which.max(m6.4$BIC[,i]) + 
			which.max(m6.5$BIC[,i]) ) /5
	G[7,i]  <- (which.max(m7.1$BIC[,i]) + 
			which.max(m7.2$BIC[,i]) + 
			which.max(m7.3$BIC[,i]) + 
			which.max(m7.4$BIC[,i]) + 
			which.max(m7.5$BIC[,i]) ) /5
	G[8,i]  <- (which.max(m8.1$BIC[,i]) + 
			which.max(m8.2$BIC[,i]) + 
			which.max(m8.3$BIC[,i]) + 
			which.max(m8.4$BIC[,i]) + 
			which.max(m8.5$BIC[,i]) ) /5
	G[9,i]  <- (which.max(m9.1$BIC[,i]) + 
			which.max(m9.2$BIC[,i]) + 
			which.max(m9.3$BIC[,i]) + 
			which.max(m9.4$BIC[,i]) + 
			which.max(m9.5$BIC[,i]) ) /5
	G[10,i] <- (which.max(m10.1$BIC[,i]) + 
			which.max(m10.2$BIC[,i]) + 
			which.max(m10.3$BIC[,i]) + 
			which.max(m10.4$BIC[,i]) + 
			which.max(m10.5$BIC[,i]) ) /5
	G[11,i] <- (which.max(m11.1$BIC[,i]) + 
			which.max(m11.2$BIC[,i]) + 
			which.max(m11.3$BIC[,i]) + 
			which.max(m11.4$BIC[,i]) + 
			which.max(m11.5$BIC[,i]) ) /5
	G[12,i] <- (which.max(m12.1$BIC[,i]) + 
			which.max(m12.2$BIC[,i]) + 
			which.max(m12.3$BIC[,i]) + 
			which.max(m12.4$BIC[,i]) + 
			which.max(m12.5$BIC[,i]) ) /5
	G[13,i] <- (which.max(m13.1$BIC[,i]) + 
			which.max(m13.2$BIC[,i]) + 
			which.max(m13.3$BIC[,i]) + 
			which.max(m13.4$BIC[,i]) + 
			which.max(m13.5$BIC[,i]) ) /5
	G[14,i] <- (which.max(m14.1$BIC[,i]) + 
			which.max(m14.2$BIC[,i]) + 
			which.max(m14.3$BIC[,i]) + 
			which.max(m14.4$BIC[,i]) + 
			which.max(m14.5$BIC[,i]) ) /5
	G[15,i] <- (which.max(m15.1$BIC[,i]) + 
			which.max(m15.2$BIC[,i]) + 
			which.max(m15.3$BIC[,i]) + 
			which.max(m15.4$BIC[,i]) + 
			which.max(m15.5$BIC[,i]) ) /5
	G[16,i] <- (which.max(m16.1$BIC[,i]) + 
			which.max(m16.2$BIC[,i]) + 
			which.max(m16.3$BIC[,i]) + 
			which.max(m16.4$BIC[,i]) + 
			which.max(m16.5$BIC[,i]) ) /5
	G[17,i] <- (which.max(m17.1$BIC[,i]) + 
			which.max(m17.2$BIC[,i]) + 
			which.max(m17.3$BIC[,i]) + 
			which.max(m17.4$BIC[,i]) + 
			which.max(m17.5$BIC[,i]) ) /5
}

G.best <- c(((m1.1$G +  m1.2$G  + m1.3$G  + m1.4$G  + m1.5$G)/5),
		((m2.1$G +  m2.2$G  + m2.3$G  + m2.4$G  + m2.5$G)/5),
		((m3.1$G +  m3.2$G  + m3.3$G  + m3.4$G  + m3.5$G)/5),
		((m4.1$G +  m4.2$G  + m4.3$G  + m4.4$G  + m4.5$G)/5),
		((m5.1$G +  m5.2$G  + m5.3$G  + m5.4$G  + m5.5$G)/5),
		((m6.1$G +  m6.2$G  + m6.3$G  + m6.4$G  + m6.5$G)/5),
		((m7.1$G +  m7.2$G  + m7.3$G  + m7.4$G  + m7.5$G)/5),
		((m8.1$G +  m8.2$G  + m8.3$G  + m8.4$G  + m8.5$G)/5),
		((m9.1$G +  m9.2$G  + m9.3$G  + m9.4$G  + m9.5$G)/5),
		((m10.1$G + m10.2$G + m10.3$G + m10.4$G + m10.5$G)/5),
		((m11.1$G + m11.2$G + m11.3$G + m11.4$G + m11.5$G)/5),
		((m12.1$G + m12.2$G + m12.3$G + m12.4$G + m12.5$G)/5),
		((m13.1$G + m13.2$G + m13.3$G + m13.4$G + m13.5$G)/5),
		((m14.1$G + m14.2$G + m14.3$G + m14.4$G + m14.5$G)/5),
		((m15.1$G + m15.2$G + m15.3$G + m15.4$G + m15.5$G)/5),
		((m16.1$G + m16.2$G + m16.3$G + m16.4$G + m16.5$G)/5),
		((m17.1$G + m17.2$G + m17.3$G + m17.4$G + m17.5$G)/5) )

colour <- rainbow(10,start=0)
plot(c(1:17),G[,1], xlim=c(1,17), ylim=c(0,20), col=colour[1], ylab="Number of Clusters", xlab="Sample Number", lwd=2)
lines(c(1:17),G[,1], xlim=c(1,17), ylim=c(0,20), col=colour[1], ylab="", xlab="", lwd=2)
par(new=T)

for(i in 2:10){
	plot(c(1:17),G[,i], xlim=c(1,17), ylim=c(0,20), col=colour[i], ylab="", xlab="", lwd=2)
	lines(c(1:17),G[,i], xlim=c(1,17), ylim=c(0,20), col=colour[i], ylab="", xlab="", lwd=2)
	par(new=T)
}

plot(c(1:17),G.best, xlim=c(1,17), ylim=c(0,20), pch=16,  ylab="Number of Clusters", xlab="Sample Number")
legend(1, 20, c("EII","VII","EEI","VEI","EVI","VVI","EEE","EEV","VEV","VVV", "Optimal"), pch=16, ncol=3, col=c(colour[1:10], "black"))


J <- matrix(1:(10*17), 17 ,10)
for(i in 1:10){
	J[1,i]  <- (max(m1.1$BIC[,i], na.rm = TRUE) + 
			max(m1.2$BIC[,i], na.rm = TRUE) + 
			max(m1.3$BIC[,i], na.rm = TRUE) + 
			max(m1.4$BIC[,i], na.rm = TRUE) + 
			max(m1.5$BIC[,i], na.rm = TRUE) ) /5
	J[2,i]  <- (max(m2.1$BIC[,i], na.rm = TRUE) + 
			max(m2.2$BIC[,i], na.rm = TRUE) + 
			max(m2.3$BIC[,i], na.rm = TRUE) + 
			max(m2.4$BIC[,i], na.rm = TRUE) + 
			max(m2.5$BIC[,i], na.rm = TRUE) ) /5
	J[3,i]  <- (max(m3.1$BIC[,i], na.rm = TRUE) + 
			max(m3.2$BIC[,i], na.rm = TRUE) + 
			max(m3.3$BIC[,i], na.rm = TRUE) + 
			max(m3.4$BIC[,i], na.rm = TRUE) + 
			max(m3.5$BIC[,i], na.rm = TRUE) ) /5
	J[4,i]  <- (max(m4.1$BIC[,i], na.rm = TRUE) + 
			max(m4.2$BIC[,i], na.rm = TRUE) + 
			max(m4.3$BIC[,i], na.rm = TRUE) + 
			max(m4.4$BIC[,i], na.rm = TRUE) + 
			max(m4.5$BIC[,i], na.rm = TRUE) ) /5
	J[5,i]  <- (max(m5.1$BIC[,i], na.rm = TRUE) + 
			max(m5.2$BIC[,i], na.rm = TRUE) + 
			max(m5.3$BIC[,i], na.rm = TRUE) + 
			max(m5.4$BIC[,i], na.rm = TRUE) + 
			max(m5.5$BIC[,i], na.rm = TRUE) ) /5
	J[6,i]  <- (max(m6.1$BIC[,i], na.rm = TRUE) + 
			max(m6.2$BIC[,i], na.rm = TRUE) + 
			max(m6.3$BIC[,i], na.rm = TRUE) + 
			max(m6.4$BIC[,i], na.rm = TRUE) + 
			max(m6.5$BIC[,i], na.rm = TRUE) ) /5
	J[7,i]  <- (max(m7.1$BIC[,i], na.rm = TRUE) + 
			max(m7.2$BIC[,i], na.rm = TRUE) + 
			max(m7.3$BIC[,i], na.rm = TRUE) + 
			max(m7.4$BIC[,i], na.rm = TRUE) + 
			max(m7.5$BIC[,i], na.rm = TRUE) ) /5
	J[8,i]  <- (max(m8.1$BIC[,i], na.rm = TRUE) + 
			max(m8.2$BIC[,i], na.rm = TRUE) + 
			max(m8.3$BIC[,i], na.rm = TRUE) + 
			max(m8.4$BIC[,i], na.rm = TRUE) + 
			max(m8.5$BIC[,i], na.rm = TRUE) ) /5
	J[9,i]  <- (max(m9.1$BIC[,i], na.rm = TRUE) + 
			max(m9.2$BIC[,i], na.rm = TRUE) + 
			max(m9.3$BIC[,i], na.rm = TRUE) + 
			max(m9.4$BIC[,i], na.rm = TRUE) + 
			max(m9.5$BIC[,i], na.rm = TRUE) ) /5
	J[10,i] <- (max(m10.1$BIC[,i], na.rm = TRUE) + 
			max(m10.2$BIC[,i], na.rm = TRUE) + 
			max(m10.3$BIC[,i], na.rm = TRUE) + 
			max(m10.4$BIC[,i], na.rm = TRUE) + 
			max(m10.5$BIC[,i], na.rm = TRUE) ) /5
	J[11,i] <- (max(m11.1$BIC[,i], na.rm = TRUE) + 
			max(m11.2$BIC[,i], na.rm = TRUE) + 
			max(m11.3$BIC[,i], na.rm = TRUE) + 
			max(m11.4$BIC[,i], na.rm = TRUE) + 
			max(m11.5$BIC[,i], na.rm = TRUE) ) /5
	J[12,i] <- (max(m12.1$BIC[,i], na.rm = TRUE) + 
			max(m12.2$BIC[,i], na.rm = TRUE) + 
			max(m12.3$BIC[,i], na.rm = TRUE) + 
			max(m12.4$BIC[,i], na.rm = TRUE) + 
			max(m12.5$BIC[,i], na.rm = TRUE) ) /5
	J[13,i] <- (max(m13.1$BIC[,i], na.rm = TRUE) + 
			max(m13.2$BIC[,i], na.rm = TRUE) + 
			max(m13.3$BIC[,i], na.rm = TRUE) + 
			max(m13.4$BIC[,i], na.rm = TRUE) + 
			max(m13.5$BIC[,i], na.rm = TRUE) ) /5
	J[14,i] <- (max(m14.1$BIC[,i], na.rm = TRUE) + 
			max(m14.2$BIC[,i], na.rm = TRUE) + 
			max(m14.3$BIC[,i], na.rm = TRUE) + 
			max(m14.4$BIC[,i], na.rm = TRUE) + 
			max(m14.5$BIC[,i], na.rm = TRUE) ) /5
	J[15,i] <- (max(m15.1$BIC[,i], na.rm = TRUE) + 
			max(m15.2$BIC[,i], na.rm = TRUE) + 
			max(m15.3$BIC[,i], na.rm = TRUE) + 
			max(m15.4$BIC[,i], na.rm = TRUE) + 
			max(m15.5$BIC[,i], na.rm = TRUE) ) /5
	J[16,i] <- (max(m16.1$BIC[,i], na.rm = TRUE) + 
			max(m16.2$BIC[,i], na.rm = TRUE) + 
			max(m16.3$BIC[,i], na.rm = TRUE) + 
			max(m16.4$BIC[,i], na.rm = TRUE) + 
			max(m16.5$BIC[,i], na.rm = TRUE) ) /5
	J[17,i] <- (max(m17.1$BIC[,i], na.rm = TRUE) + 
			max(m17.2$BIC[,i], na.rm = TRUE) + 
			max(m17.3$BIC[,i], na.rm = TRUE) + 
			max(m17.4$BIC[,i], na.rm = TRUE) + 
			max(m17.5$BIC[,i], na.rm = TRUE) ) /5
}

colour <- rainbow(10,start=0)
plot(c(1:17),J[,1], xlim=c(1,17), ylim=c(-320000,-50000), col=colour[1], ylab="BIC", xlab="Sample Number", lwd=2)
lines(c(1:17),J[,1], xlim=c(1,17), ylim=c(-320000,-50000), col=colour[1], ylab="", xlab="", lwd=2)
par(new=T)

for(i in 2:10){
	plot(c(1:17),J[,i], xlim=c(1,17), ylim=c(-320000,-50000), col=colour[i], ylab="", xlab="", lwd=2)
	lines(c(1:17),J[,i], xlim=c(1,17), ylim=c(-320000,-50000), col=colour[i], ylab="", xlab="", lwd=2)
	par(new=T)
}
legend(1, -250000, c("EII","VII","EEI","VEI","EVI","VVI","EEE","EEV","VEV","VVV"), pch=16, ncol=3, col=c(colour[1:10]))



K <- matrix(1:(5*17), 17 ,5)

K[1,1]  <-  m1.1$bic; K[1,2]  <-  m1.2$bic; K[1,3]  <-  m1.3$bic; K[1,4]  <-  m1.4$bic; K[1,5]  <-  m1.5$bic 
K[2,1]  <-  m2.1$bic; K[2,2]  <-  m2.2$bic; K[2,3]  <-  m2.3$bic; K[2,4]  <-  m2.4$bic; K[2,5]  <-  m2.5$bic 
K[3,1]  <-  m3.1$bic; K[3,2]  <-  m3.2$bic; K[3,3]  <-  m3.3$bic; K[3,4]  <-  m3.4$bic; K[3,5]  <-  m3.5$bic 
K[4,1]  <-  m4.1$bic; K[4,2]  <-  m4.2$bic; K[4,3]  <-  m4.3$bic; K[4,4]  <-  m4.4$bic; K[4,5]  <-  m4.5$bic 
K[5,1]  <-  m5.1$bic; K[5,2]  <-  m5.2$bic; K[5,3]  <-  m5.3$bic; K[5,4]  <-  m5.4$bic; K[5,5]  <-  m5.5$bic 
K[6,1]  <-  m6.1$bic; K[6,2]  <-  m6.2$bic; K[6,3]  <-  m6.3$bic; K[6,4]  <-  m6.4$bic; K[6,5]  <-  m6.5$bic 
K[7,1]  <-  m7.1$bic; K[7,2]  <-  m7.2$bic; K[7,3]  <-  m7.3$bic; K[7,4]  <-  m7.4$bic; K[7,5]  <-  m7.5$bic 
K[8,1]  <-  m8.1$bic; K[8,2]  <-  m8.2$bic; K[8,3]  <-  m8.3$bic; K[8,4]  <-  m8.4$bic; K[8,5]  <-  m8.5$bic 
K[9,1]  <-  m9.1$bic; K[9,2]  <-  m9.2$bic; K[9,3]  <-  m9.3$bic; K[9,4]  <-  m9.4$bic; K[9,5]  <-  m9.5$bic 
K[10,1]  <-  m10.1$bic; K[10,2]  <-  m10.2$bic; K[10,3]  <-  m10.3$bic; K[10,4]  <-  m10.4$bic; K[10,5]  <-  m10.5$bic 
K[11,1]  <-  m11.1$bic; K[11,2]  <-  m11.2$bic; K[11,3]  <-  m11.3$bic; K[11,4]  <-  m11.4$bic; K[11,5]  <-  m11.5$bic 
K[12,1]  <-  m12.1$bic; K[12,2]  <-  m12.2$bic; K[12,3]  <-  m12.3$bic; K[12,4]  <-  m12.4$bic; K[12,5]  <-  m12.5$bic 
K[13,1]  <-  m13.1$bic; K[13,2]  <-  m13.2$bic; K[13,3]  <-  m13.3$bic; K[13,4]  <-  m13.4$bic; K[13,5]  <-  m13.5$bic 
K[14,1]  <-  m14.1$bic; K[14,2]  <-  m14.2$bic; K[14,3]  <-  m14.3$bic; K[14,4]  <-  m14.4$bic; K[14,5]  <-  m14.5$bic 
K[15,1]  <-  m15.1$bic; K[15,2]  <-  m15.2$bic; K[15,3]  <-  m15.3$bic; K[15,4]  <-  m15.4$bic; K[15,5]  <-  m15.5$bic 
K[16,1]  <-  m16.1$bic; K[16,2]  <-  m16.2$bic; K[16,3]  <-  m16.3$bic; K[16,4]  <-  m16.4$bic; K[16,5]  <-  m16.5$bic
K[17,1]  <-  m17.1$bic; K[17,2]  <-  m17.2$bic; K[17,3]  <-  m17.3$bic; K[17,4]  <-  m17.4$bic; K[17,5]  <-  m17.5$bic 

vei <- c(K[1,1:5], K[2,1:3], K[2,5], K[3,1], K[3,4], K[4,1:4], K[5,1], 
		K[5,3:5], K[7, 1:5], K[8, 1], K[9, 3])
veiSamp <- c( rep(1,5), rep(2,4), rep(3,2), rep(4,4), rep(5,4), 
		rep(7,5), rep(8,1), rep(9,1))
vvi <- c(K[2, 4], K[3, 2:3], K[3, 5], K[4, 5], K[5, 2], K[6, 1:5], K[8, 2], K[8, 5])
vviSamp <- c(rep(2,1), rep(3,3), rep(4,1), rep(5,1), rep(6,5), rep(8,2))
eei <- c(K[8, 4]) 
eeiSamp <- 8
xxx <- c(K[8, 3], K[9, 1:2], K[9, 4:5], K[10, 1:5], K[11, 1:5],  K[12, 1:5],
		 K[13, 1:5],  K[14, 1:5], K[15, 1:5], K[16, 1:5], K[17, 1:5])
xxxSamp <- c(8, rep(9, 4), rep(10, 5), rep(11, 5), rep(12, 5), rep(13, 5), 
			rep(14, 5), rep(15, 5), rep(16, 5), rep(17, 5))

colour <- rainbow(10,start=0)
plot(veiSamp,vei, xlim=c(1,17), ylim=c(-250000,-50000), pch=16, col=colour[4], ylab="BIC", xlab="Sample Number")
par(new=T)
plot(vviSamp,vvi, xlim=c(1,17), ylim=c(-250000,-50000), pch=16, col=colour[6], ylab="BIC", xlab="Sample Number")
par(new=T)
plot(eeiSamp,eei, xlim=c(1,17), ylim=c(-250000,-50000), pch=16, col=colour[3], ylab="BIC", xlab="Sample Number")
par(new=T)
plot(xxxSamp,xxx, xlim=c(1,17), ylim=c(-250000,-50000), pch=16, col=colour[10], ylab="BIC", xlab="Sample Number")
legend(1, -200000, c("EEI","VEI","VVI","XXX"), pch=16, ncol=2, col=c(colour[3], colour[4], colour[6], colour[10]))





######## Entire data #########

m <- Mclust(data=data[,1:173], G=1:20, modelNames = mclust.options("emModelNames"))

######### 200's ########
data200.1  <- data[1:200,1:173]
data200.2  <- data[201:400,1:173]
data200.3  <- data[401:600,1:173]
data200.4  <- data[601:800,1:173]
data200.5  <- data[801:1000,1:173]
data200.6  <- data[1001:1200,1:173]
data200.7  <- data[1201:1400,1:173]
data200.8  <- data[1401:1600,1:173]
data200.9  <- data[1601:1800,1:173]
data200.10 <- data[1801:2000,1:173]
data200.11 <- data[2001:2200,1:173]
data200.12 <- data[2201:2400,1:173]
data200.13 <- data[2401:2600,1:173]
data200.14 <- data[2601:2800,1:173]
data200.15 <- data[2801:3000,1:173]
data200.16 <- data[3001:3200,1:173]
data200.17 <- data[3201:3400,1:173]
data200.18 <- data[3401:3600,1:173]
data200.19 <- data[3601:3800,1:173]
data200.20 <- data[3801:4000,1:173]
data200.21 <- data[4001:4200,1:173]
data200.22 <- data[4201:4400,1:173]
data200.23 <- data[4401:4600,1:173]
data200.24 <- data[4601:4800,1:173]
data200.25 <- data[4801:5000,1:173]
data200.26 <- data[5001:5200,1:173]
data200.27 <- data[5201:5400,1:173]
data200.28 <- data[5401:5600,1:173]
data200.29 <- data[5601:5800,1:173]
data200.30 <- data[5801:6000,1:173]

m200.1  <- Mclust(data=data200.1, G=1:20, modelNames = mclust.options("emModelNames"))
m200.2  <- Mclust(data=data200.2, G=1:20, modelNames = mclust.options("emModelNames"))
m200.3  <- Mclust(data=data200.3, G=1:20, modelNames = mclust.options("emModelNames"))
m200.4  <- Mclust(data=data200.4, G=1:20, modelNames = mclust.options("emModelNames"))
m200.5  <- Mclust(data=data200.5, G=1:20, modelNames = mclust.options("emModelNames"))
m200.6  <- Mclust(data=data200.6, G=1:20, modelNames = mclust.options("emModelNames"))
m200.7  <- Mclust(data=data200.7, G=1:20, modelNames = mclust.options("emModelNames"))
m200.8  <- Mclust(data=data200.8, G=1:20, modelNames = mclust.options("emModelNames"))
m200.9  <- Mclust(data=data200.9, G=1:20, modelNames = mclust.options("emModelNames"))
m200.10 <- Mclust(data=data200.10, G=1:20, modelNames = mclust.options("emModelNames"))
m200.11 <- Mclust(data=data200.11, G=1:20, modelNames = mclust.options("emModelNames"))
m200.12 <- Mclust(data=data200.12, G=1:20, modelNames = mclust.options("emModelNames"))
m200.13 <- Mclust(data=data200.13, G=1:20, modelNames = mclust.options("emModelNames"))
m200.14 <- Mclust(data=data200.14, G=1:20, modelNames = mclust.options("emModelNames"))
m200.15 <- Mclust(data=data200.15, G=1:20, modelNames = mclust.options("emModelNames"))
m200.16 <- Mclust(data=data200.16, G=1:20, modelNames = mclust.options("emModelNames"))
m200.17 <- Mclust(data=data200.17, G=1:20, modelNames = mclust.options("emModelNames"))
m200.18 <- Mclust(data=data200.18, G=1:20, modelNames = mclust.options("emModelNames"))
m200.19 <- Mclust(data=data200.19, G=1:20, modelNames = mclust.options("emModelNames"))
m200.20 <- Mclust(data=data200.20, G=1:20, modelNames = mclust.options("emModelNames"))
m200.21 <- Mclust(data=data200.21, G=1:20, modelNames = mclust.options("emModelNames"))
m200.22 <- Mclust(data=data200.22, G=1:20, modelNames = mclust.options("emModelNames"))
m200.23 <- Mclust(data=data200.23, G=1:20, modelNames = mclust.options("emModelNames"))
m200.24 <- Mclust(data=data200.24, G=1:20, modelNames = mclust.options("emModelNames"))
m200.25 <- Mclust(data=data200.25, G=1:20, modelNames = mclust.options("emModelNames"))
m200.26 <- Mclust(data=data200.26, G=1:20, modelNames = mclust.options("emModelNames"))
m200.27 <- Mclust(data=data200.27, G=1:20, modelNames = mclust.options("emModelNames"))
m200.28 <- Mclust(data=data200.28, G=1:20, modelNames = mclust.options("emModelNames"))
m200.29 <- Mclust(data=data200.29, G=1:20, modelNames = mclust.options("emModelNames"))
m200.30 <- Mclust(data=data200.30, G=1:20, modelNames = mclust.options("emModelNames"))

######### 250's ########
data250.1  <- data[1:250,1:173]
data250.2  <- data[251:500,1:173]
data250.3  <- data[501:750,1:173]
data250.4  <- data[751:1000,1:173]
data250.5  <- data[1001:1250,1:173]
data250.6  <- data[1251:1500,1:173]
data250.7  <- data[1501:1750,1:173]
data250.8  <- data[1751:2000,1:173]
data250.9  <- data[2001:2250,1:173]
data250.10 <- data[2251:2500,1:173]
data250.11 <- data[2501:2750,1:173]
data250.12 <- data[2751:3000,1:173]
data250.13 <- data[3001:3250,1:173]
data250.14 <- data[3251:3500,1:173]
data250.15 <- data[3501:3750,1:173]
data250.16 <- data[3751:4000,1:173]
data250.17 <- data[4001:4250,1:173]
data250.18 <- data[4251:4500,1:173]
data250.19 <- data[4501:4750,1:173]
data250.20 <- data[4751:5000,1:173]
data250.21 <- data[5001:5250,1:173]
data250.22 <- data[5251:5500,1:173]
data250.23 <- data[5501:5750,1:173]
data250.24 <- data[5751:6000,1:173]

m250.1  <- Mclust(data=data250.1, G=1:20, modelNames = mclust.options("emModelNames"))
m250.2  <- Mclust(data=data250.2, G=1:20, modelNames = mclust.options("emModelNames"))
m250.3  <- Mclust(data=data250.3, G=1:20, modelNames = mclust.options("emModelNames"))
m250.4  <- Mclust(data=data250.4, G=1:20, modelNames = mclust.options("emModelNames"))
m250.5  <- Mclust(data=data250.5, G=1:20, modelNames = mclust.options("emModelNames"))
m250.6  <- Mclust(data=data250.6, G=1:20, modelNames = mclust.options("emModelNames"))
m250.7  <- Mclust(data=data250.7, G=1:20, modelNames = mclust.options("emModelNames"))
m250.8  <- Mclust(data=data250.8, G=1:20, modelNames = mclust.options("emModelNames"))
m250.9  <- Mclust(data=data250.9, G=1:20, modelNames = mclust.options("emModelNames"))
m250.10 <- Mclust(data=data250.10, G=1:20, modelNames = mclust.options("emModelNames"))
m250.11 <- Mclust(data=data250.11, G=1:20, modelNames = mclust.options("emModelNames"))
m250.12 <- Mclust(data=data250.12, G=1:20, modelNames = mclust.options("emModelNames"))
m250.13 <- Mclust(data=data250.13, G=1:20, modelNames = mclust.options("emModelNames"))
m250.14 <- Mclust(data=data250.14, G=1:20, modelNames = mclust.options("emModelNames"))
m250.15 <- Mclust(data=data250.15, G=1:20, modelNames = mclust.options("emModelNames"))
m250.16 <- Mclust(data=data250.16, G=1:20, modelNames = mclust.options("emModelNames"))
m250.17 <- Mclust(data=data250.17, G=1:20, modelNames = mclust.options("emModelNames"))
m250.18 <- Mclust(data=data250.18, G=1:20, modelNames = mclust.options("emModelNames"))
m250.19 <- Mclust(data=data250.19, G=1:20, modelNames = mclust.options("emModelNames"))
m250.20 <- Mclust(data=data250.20, G=1:20, modelNames = mclust.options("emModelNames"))
m250.21 <- Mclust(data=data250.21, G=1:20, modelNames = mclust.options("emModelNames"))
m250.22 <- Mclust(data=data250.22, G=1:20, modelNames = mclust.options("emModelNames"))
m250.23 <- Mclust(data=data250.23, G=1:20, modelNames = mclust.options("emModelNames"))
m250.24 <- Mclust(data=data250.24, G=1:20, modelNames = mclust.options("emModelNames"))


######### 500's ########

data500.1  <- data[1:500,1:173]
data500.2  <- data[501:1000,1:173]
data500.3  <- data[1001:1500,1:173]
data500.4  <- data[1501:2000,1:173]
data500.5  <- data[2001:2500,1:173]
data500.6  <- data[2501:3000,1:173]
data500.7  <- data[3001:3500,1:173]
data500.8  <- data[3501:4000,1:173]
data500.9  <- data[4001:4500,1:173]
data500.10 <- data[4501:5000,1:173]
data500.11 <- data[5001:5500,1:173]
data500.12 <- data[5501:6000,1:173]

m500.1  <- Mclust(data=data500.1, G=1:20, modelNames = mclust.options("emModelNames"))
m500.2  <- Mclust(data=data500.2, G=1:20, modelNames = mclust.options("emModelNames"))
m500.3  <- Mclust(data=data500.3, G=1:20, modelNames = mclust.options("emModelNames"))
m500.4  <- Mclust(data=data500.4, G=1:20, modelNames = mclust.options("emModelNames"))
m500.5  <- Mclust(data=data500.5, G=1:20, modelNames = mclust.options("emModelNames"))
m500.6  <- Mclust(data=data500.6, G=1:20, modelNames = mclust.options("emModelNames"))
m500.7  <- Mclust(data=data500.7, G=1:20, modelNames = mclust.options("emModelNames"))
m500.8  <- Mclust(data=data500.8, G=1:20, modelNames = mclust.options("emModelNames"))
m500.9  <- Mclust(data=data500.9, G=1:20, modelNames = mclust.options("emModelNames"))
m500.10 <- Mclust(data=data500.10, G=1:20, modelNames = mclust.options("emModelNames"))
m500.11 <- Mclust(data=data500.11, G=1:20, modelNames = mclust.options("emModelNames"))
m500.12 <- Mclust(data=data500.12, G=1:20, modelNames = mclust.options("emModelNames"))

### systematic analysis. bic & component no.s
require(shape)


bic250 <- c(m250.1$bic,  m250.2$bic,  m250.3$bic,  m250.4$bic,  m250.5$bic, 
		m250.6$bic,  m250.7$bic,  m250.8$bic,  m250.9$bic,  m250.10$bic, 
		m250.11$bic, m250.12$bic, m250.13$bic, m250.14$bic, m250.15$bic, 
		m250.16$bic, m250.17$bic, m250.18$bic, m250.19$bic, m250.20$bic, 
		m250.21$bic, m250.22$bic, m250.23$bic, m250.24$bic)
bic500 <- c(m500.1$bic,  m500.2$bic,  m500.3$bic,  m500.4$bic,  m500.5$bic, 
		m500.6$bic,  m500.7$bic,  m500.8$bic,  m500.9$bic,  m500.10$bic, 
		m500.11$bic, m500.12$bic)

plot(bic250, xlim =c(1,24) , ylim=c(-180000, -30000), col="red", lwd=2, xlab="Sample", ylab="BIC")
lines(bic250, col="red", lwd=2)
par(new=T)
plot( c(seq(1,23,2)), bic500, xlim =c(1,24), ylim=c(-180000, -30000), col="blue" , lwd=2, xlab="Sample", ylab="BIC")
lines( c(seq(1,23,2)), bic500, col="blue", lwd=2)
legend(17, -160000, c("Sample Size: 250","Sample Size: 500"), pch=16, ncol=1, col=c("red", "blue"))

G250 <- c(m250.1$G,  m250.2$G,  m250.3$G,  m250.4$G,  m250.5$G, 
		m250.6$G,  m250.7$G,  m250.8$G,  m250.9$G,  m250.10$G, 
		m250.11$G, m250.12$G, m250.13$G, m250.14$G, m250.15$G, 
		m250.16$G, m250.17$G, m250.18$G, m250.19$G, m250.20$G, 
		m250.21$G, m250.22$G, m250.23$G, m250.24$G)
G500 <- c(m500.1$G,  m500.2$G,  m500.3$G,  m500.4$G,  m500.5$G, 
		m500.6$G,  m500.7$G,  m500.8$G,  m500.9$G,  m500.10$G, 
		m500.11$G, m500.12$G)

plot(G250, xlim =c(1,24) , ylim=c(0, 15), col="red", lwd=2, xlab="Sample", ylab="BIC")
lines(G250, col="red", lwd=2)
par(new=T)
plot( c(seq(1,23,2)), G500, xlim =c(1,24), ylim=c(0, 15), col="blue" , lwd=2, xlab="Sample", ylab="BIC")
lines( c(seq(1,23,2)), G500, col="blue", lwd=2)
par(new=T)

Gblend <- c((m250.1$G + m250.2$G)/2,  (m250.3$G + m250.4$G)/2,  (m250.5$G + 
		m250.6$G)/2,  (m250.7$G + m250.8$G)/2,  (m250.9$G + m250.10$G)/2, 
		(m250.11$G + m250.12$G)/2, (m250.13$G + m250.14$G)/2, (m250.15$G + 
		m250.16$G)/2, (m250.17$G + m250.18$G)/2, (m250.19$G + m250.20$G)/2, 
		(m250.21$G + m250.22$G)/2, (m250.23$G + m250.24$G)/2)

plot( c(seq(1,23,2)), round(1.5*Gblend), xlim =c(1,24), ylim=c(0, 15), col="green" , lwd=2, xlab="", ylab="")
lines( c(seq(1,23,2)), round(1.5*Gblend), col="green", lwd=2)
legend(17, 15, c("Sample Size: 250","Sample Size: 500"), pch=16, ncol=1, col=c("red", "blue"))




mclust2Dplot(data250.1[,c(13,100)], parameters = m250.1$parameters, z = m250.1$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m250.1$G){
plotellipse(mid = c(m250.1$parameters$mean[13,i], m250.1$parameters$mean[100,i]),
			 rx = sqrt(m250.1$parameters$variance$shape[13]*m250.1$parameters$variance$scale[i]), 
			 ry = sqrt(m250.1$parameters$variance$shape[100]*m250.1$parameters$variance$scale[i]))
}

mclust2Dplot(data250.2[,c(13,100)], parameters = m250.2$parameters, z = m250.2$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m250.2$G){
plotellipse(mid = c(m250.2$parameters$mean[13,i], m250.2$parameters$mean[100,i]),
			 rx = sqrt(m250.2$parameters$variance$shape[13]*m250.2$parameters$variance$scale[i]), 
			 ry = sqrt(m250.2$parameters$variance$shape[100]*m250.2$parameters$variance$scale[i]),
			 lcol = mclust.options("classPlotColors")[i])
}

mclust2Dplot(data500.1[,c(13,100)], parameters = m500.1$parameters, z = m500.1$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m500.1$G){
plotellipse(mid = c(m500.1$parameters$mean[13,i], m500.1$parameters$mean[100,i]),
			 rx = sqrt(m500.1$parameters$variance$shape[13]*m500.1$parameters$variance$scale[i]), 
			 ry = sqrt(m500.1$parameters$variance$shape[100]*m500.1$parameters$variance$scale[i]))
}

mclust2Dplot(data250.3[,c(13,100)], parameters = m250.3$parameters, z = m250.3$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m250.3$G){
plotellipse(mid = c(m250.3$parameters$mean[13,i], m250.3$parameters$mean[100,i]),
			 rx = 2*sqrt(m250.3$parameters$variance$shape[13]*m250.3$parameters$variance$scale[i]), 
			 ry = 2*sqrt(m250.3$parameters$variance$shape[100]*m250.3$parameters$variance$scale[i]),
			 lcol = mclust.options("classPlotColors")[i])
}

mclust2Dplot(data250.4[,c(13,100)], parameters = m250.4$parameters, z = m250.4$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m250.4$G){
plotellipse(mid = c(m250.4$parameters$mean[13,i], m250.4$parameters$mean[100,i]),
			 rx = 2*sqrt(m250.4$parameters$variance$shape[13]*m250.4$parameters$variance$scale[i]), 
			 ry = 2*sqrt(m250.4$parameters$variance$shape[100]*m250.4$parameters$variance$scale[i]),
			 lcol = mclust.options("classPlotColors")[i])
}

mclust2Dplot(data250.5[,c(13,100)], parameters = m250.5$parameters, z = m250.5$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m250.5$G){
plotellipse(mid = c(m250.5$parameters$mean[13,i], m250.5$parameters$mean[100,i]),
			 rx = 2*sqrt(m250.5$parameters$variance$shape[13]*m250.5$parameters$variance$scale[i]), 
			 ry = 2*sqrt(m250.5$parameters$variance$shape[100]*m250.5$parameters$variance$scale[i]),
			 lcol = mclust.options("classPlotColors")[i])
}

mclust2Dplot(data250.6[,c(13,100)], parameters = m250.6$parameters, z = m250.6$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m250.6$G){
plotellipse(mid = c(m250.6$parameters$mean[13,i], m250.6$parameters$mean[100,i]),
			 rx = 2*sqrt(m250.6$parameters$variance$shape[13]*m250.6$parameters$variance$scale[i]), 
			 ry = 2*sqrt(m250.6$parameters$variance$shape[100]*m250.6$parameters$variance$scale[i]),
			 lcol = mclust.options("classPlotColors")[i])
}

mclust2Dplot(data250.8[,c(13,100)], parameters = m250.8$parameters, z = m250.8$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m250.8$G){
plotellipse(mid = c(m250.8$parameters$mean[13,i], m250.8$parameters$mean[100,i]),
			 rx = 2*sqrt(m250.8$parameters$variance$shape[13]*m250.8$parameters$variance$scale), 
			 ry = 2*sqrt(m250.8$parameters$variance$shape[100]*m250.8$parameters$variance$scale),
			 lcol = mclust.options("classPlotColors")[i])
}

### means and vars ###
which.min(m250.1$parameters$pro)
m250.1$parameters$pro
m250.1$parameters$mean[13,]
2*sqrt(m250.1$parameters$variance$scale*m250.1$parameters$variance$shape[13])

which.min(m250.2$parameters$pro)
m250.2$parameters$pro
m250.2$parameters$mean[13,]
2*sqrt(m250.2$parameters$variance$scale*m250.2$parameters$variance$shape[13])

m500.1$parameters$pro
m500.1$parameters$mean[13,]
2*sqrt(m500.1$parameters$variance$scale*m500.1$parameters$variance$shape[13])


#### small sample blend knowing G####
############# 500.1
totalpro <- c(m250.1$parameters$pro, m250.2$parameters$pro)
totalmean.13 <- c(m250.1$parameters$mean[13,], m250.2$parameters$mean[13,])
totalvar.13 <- c((m250.1$parameters$variance$scale*m250.1$parameters$variance$shape[13]),
			(m250.2$parameters$variance$scale*m250.2$parameters$variance$shape[13]))
totalmean.100 <- c(m250.1$parameters$mean[100,], m250.2$parameters$mean[100,])
totalvar.100 <- c((m250.1$parameters$variance$scale*m250.1$parameters$variance$shape[100]),
			(m250.2$parameters$variance$scale*m250.2$parameters$variance$shape[100]))

targetpro <- rep(0, m500.1$G)


for( i in 1:m500.1$G){
	targetpro[i] <- which.max(totalpro)
	totalpro[targetpro[i]] <- 0
}
tm13 <- totalmean.13[targetpro]
tm100 <- totalmean.100[targetpro]
tv13 <- totalvar.13[targetpro]
tv100 <- totalvar.100[targetpro]


mclust2Dplot(data500.1[,c(13,100)], parameters = m500.1$parameters, z = m500.1$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m500.1$G){
plotellipse(mid = c(tm13[i], tm100[i]),
			 rx = sqrt(tv13[i]), 
			 ry = sqrt(tv100[i]))
}
############### 500.2

totalpro <- c(m250.3$parameters$pro, m250.4$parameters$pro)
totalmean.13 <- c(m250.3$parameters$mean[13,], m250.4$parameters$mean[13,])
totalvar.13 <- c((m250.3$parameters$variance$scale*m250.3$parameters$variance$shape[13]),
			(m250.4$parameters$variance$scale*m250.4$parameters$variance$shape[13]))
totalmean.100 <- c(m250.3$parameters$mean[100,], m250.4$parameters$mean[100,])
totalvar.100 <- c((m250.3$parameters$variance$scale*m250.3$parameters$variance$shape[100]),
			(m250.4$parameters$variance$scale*m250.4$parameters$variance$shape[100]))

targetpro <- rep(0, m500.2$G)


for( i in 1:m500.2$G){
	targetpro[i] <- which.max(totalpro)
	totalpro[targetpro[i]] <- 0
}
tm13 <- totalmean.13[targetpro]
tm100 <- totalmean.100[targetpro]
tv13 <- totalvar.13[targetpro]
tv100 <- totalvar.100[targetpro]


mclust2Dplot(data500.2[,c(13,100)], parameters = m500.2$parameters, z = m500.2$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m500.2$G){
plotellipse(mid = c(tm13[i], tm100[i]),
			 rx = sqrt(tv13[i]), 
			 ry = sqrt(tv100[i]))
}

mclust2Dplot(data500.2[,c(13,100)], parameters = m500.2$parameters, z = m500.2$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m500.2$G){
plotellipse(mid = c(m500.2$parameters$mean[13,i], m500.2$parameters$mean[100,i]),
			 rx = sqrt(m500.2$parameters$variance$shape[13]*m500.2$parameters$variance$scale[i]), 
			 ry = sqrt(m500.2$parameters$variance$shape[100]*m500.2$parameters$variance$scale[i]))
}

############### 500.4

totalpro <- c(m250.7$parameters$pro, m250.8$parameters$pro)
totalmean.13 <- c(m250.7$parameters$mean[13,], m250.8$parameters$mean[13,])
totalvar.13 <- c((m250.7$parameters$variance$scale*m250.7$parameters$variance$shape[13]),
			(m250.8$parameters$variance$scale*m250.8$parameters$variance$shape[13]))
totalmean.100 <- c(m250.7$parameters$mean[100,], m250.8$parameters$mean[100,])
totalvar.100 <- c((m250.7$parameters$variance$scale*m250.7$parameters$variance$shape[100]),
			(m250.8$parameters$variance$scale*m250.8$parameters$variance$shape[100]))

targetpro <- rep(0, m500.4$G)
tv13 <- rep(0, m500.4$G)
tv100 <- rep(0, m500.4$G)


for( i in 1:m500.4$G){
	targetpro[i] <- which.max(totalpro)
	totalpro[targetpro[i]] <- 0
	if(targetpro[i] > m500.4$G){
		tv13[i] <- m250.8$parameters$variance$scale*m250.8$parameters$variance$shape[13]
		tv100[i] <- m250.8$parameters$variance$scale*m250.8$parameters$variance$shape[100]
	} else {
		tv13[i] <- m250.7$parameters$variance$scale*m250.7$parameters$variance$shape[13]
		tv100[i] <- m250.7$parameters$variance$scale*m250.7$parameters$variance$shape[100]
	} 
}

tm13 <- totalmean.13[targetpro]
tm100 <- totalmean.100[targetpro]


mclust2Dplot(data500.4[,c(13,100)], parameters = m500.4$parameters, z = m500.4$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m500.4$G){
plotellipse(mid = c(tm13[i], tm100[i]),
			 rx = sqrt(tv13[i]), 
			 ry = sqrt(tv100[i]))
}

mclust2Dplot(data500.4[,c(13,100)], parameters = m500.4$parameters, z = m500.4$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m500.4$G){
plotellipse(mid = c(m500.4$parameters$mean[13,i], m500.4$parameters$mean[100,i]),
			 rx = sqrt(m500.4$parameters$variance$shape[13]*m500.4$parameters$variance$scale), 
			 ry = sqrt(m500.4$parameters$variance$shape[100]*m500.4$parameters$variance$scale))
}


#### small sample blend unknown G####



SampleBlend <- function( m.s1, m.s2, data.L1, m.L1, condition1, condition2, param1, param2){
	if(length(m.s1$parameters$variance$scale*m.s1$parameters$variance$shape[condition1]) == 1){
		var13.s1 <- rep(m.s1$parameters$variance$scale*m.s1$parameters$variance$shape[condition1], 
					length(m.s1$parameters$pro))
		var100.s1 <- rep(m.s1$parameters$variance$scale*m.s1$parameters$variance$shape[condition2], 
					length(m.s1$parameters$pro))

		var13.s2 <- rep(m.s2$parameters$variance$scale*m.s2$parameters$variance$shape[condition1], 
					length(m.s2$parameters$pro))
		var100.s2 <- rep(m.s2$parameters$variance$scale*m.s2$parameters$variance$shape[condition2], 
					length(m.s2$parameters$pro))
	} else {
		var13.s1 <- m.s1$parameters$variance$scale*m.s1$parameters$variance$shape[condition1]
		var100.s1 <- m.s1$parameters$variance$scale*m.s1$parameters$variance$shape[condition2]

		var13.s2 <- m.s2$parameters$variance$scale*m.s2$parameters$variance$shape[condition1]
		var100.s2 <- m.s2$parameters$variance$scale*m.s2$parameters$variance$shape[condition2]
	}

	target.s1  <- rep(0, 15)
	target.s2  <- rep(0, 15)

	for(i in 1:m.s1$G){
		if( m.s1$parameters$pro[i] > (1/(length(m.s1$parameters$pro)+param1))){
				target.s1[i] <- i
		} else if( var13.s1[i] < param2*mean(var13.s1)){
				target.s1[i] <- i
		} else if( var100.s1[i] < param2*mean(var100.s1)){
				target.s1[i] <- i
		}
	}

	for(i in 1:m.s2$G){
		if( m.s2$parameters$pro[i] > (1/(length(m.s2$parameters$pro)+param1))){
				target.s2[i] <- i
		} else if( var13.s2[i] < param2*mean(var13.s2)){
				target.s2[i] <- i
		} else if( var100.s2[i] < param2*mean(var100.s2)){
				target.s2[i] <- i
		}
	}

	tpro  <- c(m.s1$parameters$pro[target.s1], m.s2$parameters$pro[target.s2])

	tm13  <- c(m.s1$parameters$mean[condition1,target.s1], m.s2$parameters$mean[condition1,target.s2])
	tm100 <- c(m.s1$parameters$mean[condition2,target.s1], m.s2$parameters$mean[condition2,target.s2])	

	tv13  <- c(var13.s1[target.s1], var13.s2[target.s2])
	tv100 <- c(var100.s1[target.s1], var100.s2[target.s2])

	mclust2Dplot(data.L1[,c(condition1,condition2)], parameters = m.L1$parameters, z = m.L1$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
	for(i in 1:length(tm13)){
	plotellipse(mid = c(tm13[i], tm100[i]),
				 rx = sqrt(tv13[i]), 
				 ry = sqrt(tv100[i]))
	}
	list("tpro" = tpro, "tm13" = tm13, "tm100" = tm100, "tv13" = tv13, "tv100" = tv100)
}



SampleBlend( m250.1, m250.2, data500.1, m500.1, 13, 100, 1, 1)
SampleBlend( m250.3, m250.4, data500.2, m500.2, 13, 100, 1, 1)
SampleBlend( m250.5, m250.6, data500.3, m500.3, 13, 100, 1, 1)
SampleBlend( m250.7, m250.8, data500.4, m500.4, 13, 100, 1, 1)

m250.1$parameters$variance$scale*m250.1$parameters$variance$shape[13]
m250.1$parameters$variance$scale*m250.1$parameters$variance$shape[100]

mclust2Dplot(data500.2[,c(13,100)], parameters = m500.2$parameters, z = m500.2$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m500.2$G){
plotellipse(mid = c(m500.2$parameters$mean[13,i], m500.2$parameters$mean[100,i]),
			 rx = sqrt(m500.2$parameters$variance$shape[13]*m500.2$parameters$variance$scale[i]), 
			 ry = sqrt(m500.2$parameters$variance$shape[100]*m500.2$parameters$variance$scale[i]))
}

mclust2Dplot(data500.3[,c(13,100)], parameters = m500.3$parameters, z = m500.3$z, xlim=c(-6,4), ylim=c(-5.2,6.2))
for(i in 1:m500.3$G){
plotellipse(mid = c(m500.2$parameters$mean[13,i], m500.2$parameters$mean[100,i]),
			 rx = sqrt(m500.3$parameters$variance$shape[13]*m500.3$parameters$variance$scale[i]), 
			 ry = sqrt(m500.3$parameters$variance$shape[100]*m500.3$parameters$variance$scale[i]))
}

G250 <- c(m250.1$G,  m250.2$G,  m250.3$G,  m250.4$G,  m250.5$G, 
		m250.6$G,  m250.7$G,  m250.8$G,  m250.9$G,  m250.10$G, 
		m250.11$G, m250.12$G, m250.13$G, m250.14$G, m250.15$G, 
		m250.16$G, m250.17$G, m250.18$G, m250.19$G, m250.20$G, 
		m250.21$G, m250.22$G, m250.23$G, m250.24$G)
G500 <- c(m500.1$G,  m500.2$G,  m500.3$G,  m500.4$G,  m500.5$G, 
		m500.6$G,  m500.7$G,  m500.8$G,  m500.9$G,  m500.10$G, 
		m500.11$G, m500.12$G)
G.Blend <- c(length(SampleBlend( m250.1, m250.2, data500.1, m500.1, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.3, m250.4, data500.2, m500.2, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.5, m250.6, data500.3, m500.3, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.7, m250.8, data500.4, m500.4, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.9, m250.10, data500.5, m500.5, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.11, m250.12, data500.6, m500.6, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.13, m250.14, data500.7, m500.7, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.15, m250.16, data500.8, m500.8, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.17, m250.18, data500.9, m500.9, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.19, m250.20, data500.10, m500.10, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.21, m250.22, data500.11, m500.11, 13, 100, 1, 1)$tm13),
		length(SampleBlend( m250.23, m250.24, data500.12, m500.12, 13, 100, 1, 1)$tm13))

plot(G250, xlim =c(1,24) , ylim=c(0, 16), col="red", lwd=2, xlab="Sample", ylab="BIC")
lines(G250, col="red", lwd=2)
par(new=T)
plot( c(seq(1,23,2)), G500, xlim =c(1,24), ylim=c(0, 16), col="blue" , lwd=2, xlab="Sample", ylab="BIC")
lines( c(seq(1,23,2)), G500, col="blue", lwd=2)
par(new=T)
plot( c(seq(1,23,2)), round(G.Blend), xlim =c(1,24), ylim=c(0, 16), col="green" , lwd=2, xlab="", ylab="")
lines( c(seq(1,23,2)), round(G.Blend), col="green", lwd=2)
legend(16, 15, c("Sample Size: 250","Sample Size: 500", "Aggregated Results"), pch=16, ncol=1, col=c("red", "blue", "green"))



###### Entire data blend 5000
sum(SampleBlend(13, 100, 1, 1)$tpro)

SampleBlend <- function(condition1, condition2, param1, param2){

	var13.s1 <-  m500.1$parameters$variance$scale*m500.1$parameters$variance$shape[condition1]
	var100.s1 <- m500.1$parameters$variance$scale*m500.1$parameters$variance$shape[condition2]
	var13.s2 <-  m500.2$parameters$variance$scale*m500.2$parameters$variance$shape[condition1]
	var100.s2 <- m500.2$parameters$variance$scale*m500.2$parameters$variance$shape[condition2]
	var13.s3 <-  m500.3$parameters$variance$scale*m500.3$parameters$variance$shape[condition1]
	var100.s3 <- m500.3$parameters$variance$scale*m500.3$parameters$variance$shape[condition2]
	var13.s4 <- rep(m500.4$parameters$variance$scale*m500.4$parameters$variance$shape[condition1], 
				length(m500.4$parameters$pro))
	var100.s4 <- rep(m500.4$parameters$variance$scale*m500.4$parameters$variance$shape[condition2], 
				length(m500.4$parameters$pro))
	var13.s5 <- rep(m500.5$parameters$variance$scale*m500.5$parameters$variance$shape[condition1], 
				length(m500.5$parameters$pro))
	var100.s5 <- rep(m500.5$parameters$variance$scale*m500.5$parameters$variance$shape[condition2], 
				length(m500.5$parameters$pro))
	var13.s6 <- rep(m500.6$parameters$variance$scale*m500.6$parameters$variance$shape[condition1], 
				length(m500.6$parameters$pro))
	var100.s6 <- rep(m500.6$parameters$variance$scale*m500.6$parameters$variance$shape[condition2], 
				length(m500.6$parameters$pro))
	var13.s7 <- rep(m500.7$parameters$variance$scale*m500.7$parameters$variance$shape[condition1], 
				length(m500.7$parameters$pro))
	var100.s7 <- rep(m500.7$parameters$variance$scale*m500.7$parameters$variance$shape[condition2], 
				length(m500.7$parameters$pro))
	var13.s8 <- rep(m500.8$parameters$variance$scale*m500.8$parameters$variance$shape[condition1], 
				length(m500.8$parameters$pro))
	var100.s8 <- rep(m500.8$parameters$variance$scale*m500.8$parameters$variance$shape[condition2], 
				length(m500.8$parameters$pro))
	var13.s9 <- rep(m500.9$parameters$variance$scale*m500.9$parameters$variance$shape[condition1], 
				length(m500.9$parameters$pro))
	var100.s9 <- rep(m500.9$parameters$variance$scale*m500.9$parameters$variance$shape[condition2], 
				length(m500.9$parameters$pro))
	var13.s10 <- rep(m500.10$parameters$variance$scale*m500.10$parameters$variance$shape[condition1], 
				length(m500.10$parameters$pro))
	var100.s10 <- rep(m500.10$parameters$variance$scale*m500.10$parameters$variance$shape[condition2], 
				length(m500.10$parameters$pro))
	var13.s11 <- rep(m500.11$parameters$variance$scale*m500.11$parameters$variance$shape[condition1], 
				length(m500.11$parameters$pro))
	var100.s11 <- rep(m500.11$parameters$variance$scale*m500.11$parameters$variance$shape[condition2], 
				length(m500.11$parameters$pro))
	var13.s12 <- rep(m500.12$parameters$variance$scale*m500.12$parameters$variance$shape[condition1], 
				length(m500.12$parameters$pro))
	var100.s12 <- rep(m500.12$parameters$variance$scale*m500.12$parameters$variance$shape[condition2], 
				length(m500.12$parameters$pro))

	target.s1  <- rep(0, 15)
	target.s2  <- rep(0, 15)
	target.s3  <- rep(0, 15)
	target.s4  <- rep(0, 15)
	target.s5  <- rep(0, 15)
	target.s6  <- rep(0, 15)
	target.s7  <- rep(0, 15)
	target.s8  <- rep(0, 15)
	target.s9  <- rep(0, 15)
	target.s10  <- rep(0, 15)
	target.s11  <- rep(0, 15)
	target.s12  <- rep(0, 15)

	for(i in 1:m500.1$G){
		if( m500.1$parameters$pro[i] > (1/(length(m500.1$parameters$pro)+param1))){
				target.s1[i] <- i
		} else if( var13.s1[i] < param2*mean(var13.s1)){
				target.s1[i] <- i
		} else if( var100.s1[i] < param2*mean(var100.s1)){
				target.s1[i] <- i
		}
	}

	for(i in 1:m500.2$G){
		if( m500.2$parameters$pro[i] > (1/(length(m500.2$parameters$pro)+param1))){
				target.s2[i] <- i
		} else if( var13.s2[i] < param2*mean(var13.s2)){
				target.s2[i] <- i
		} else if( var100.s2[i] < param2*mean(var100.s2)){
				target.s2[i] <- i
		}
	}

	for(i in 1:m500.3$G){
		if( m500.3$parameters$pro[i] > (1/(length(m500.3$parameters$pro)+param1))){
				target.s3[i] <- i
		} else if( var13.s3[i] < param2*mean(var13.s3)){
				target.s3[i] <- i
		} else if( var100.s3[i] < param2*mean(var100.s3)){
				target.s3[i] <- i
		}
	}

	for(i in 1:m500.4$G){
		if( m500.4$parameters$pro[i] > (1/(length(m500.4$parameters$pro)+param1))){
				target.s4[i] <- i
		} else if( var13.s4[i] < param2*mean(var13.s4)){
				target.s4[i] <- i
		} else if( var100.s4[i] < param2*mean(var100.s4)){
				target.s4[i] <- i
		}
	}

	for(i in 1:m500.5$G){
		if( m500.5$parameters$pro[i] > (1/(length(m500.5$parameters$pro)+param1))){
				target.s5[i] <- i
		} else if( var13.s5[i] < param2*mean(var13.s5)){
				target.s5[i] <- i
		} else if( var100.s5[i] < param2*mean(var100.s5)){
				target.s5[i] <- i
		}
	}

	for(i in 1:m500.6$G){
		if( m500.6$parameters$pro[i] > (1/(length(m500.6$parameters$pro)+param1))){
				target.s6[i] <- i
		} else if( var13.s6[i] < param2*mean(var13.s6)){
				target.s6[i] <- i
		} else if( var100.s6[i] < param2*mean(var100.s6)){
				target.s6[i] <- i
		}
	}

	for(i in 1:m500.7$G){
		if( m500.7$parameters$pro[i] > (1/(length(m500.7$parameters$pro)+param1))){
				target.s7[i] <- i
		} else if( var13.s7[i] < param2*mean(var13.s7)){
				target.s7[i] <- i
		} else if( var100.s7[i] < param2*mean(var100.s7)){
				target.s7[i] <- i
		}
	}

	for(i in 1:m500.8$G){
		if( m500.8$parameters$pro[i] > (1/(length(m500.8$parameters$pro)+param1))){
				target.s8[i] <- i
		} else if( var13.s8[i] < param2*mean(var13.s8)){
				target.s8[i] <- i
		} else if( var100.s8[i] < param2*mean(var100.s8)){
				target.s8[i] <- i
		}
	}

	for(i in 1:m500.9$G){
		if( m500.9$parameters$pro[i] > (1/(length(m500.9$parameters$pro)+param1))){
				target.s9[i] <- i
		} else if( var13.s9[i] < param2*mean(var13.s9)){
				target.s9[i] <- i
		} else if( var100.s9[i] < param2*mean(var100.s9)){
				target.s9[i] <- i
		}
	}

	for(i in 1:m500.10$G){
		if( m500.10$parameters$pro[i] > (1/(length(m500.10$parameters$pro)+param1))){
				target.s10[i] <- i
		} else if( var13.s10[i] < param2*mean(var13.s10)){
				target.s10[i] <- i
		} else if( var100.s10[i] < param2*mean(var100.s10)){
				target.s10[i] <- i
		}
	}

	for(i in 1:m500.11$G){
		if( m500.11$parameters$pro[i] > (1/(length(m500.11$parameters$pro)+param1))){
				target.s11[i] <- i
		} else if( var13.s11[i] < param2*mean(var13.s11)){
				target.s11[i] <- i
		} else if( var100.s11[i] < param2*mean(var100.s11)){
				target.s11[i] <- i
		}
	}

	for(i in 1:m500.12$G){
		if( m500.12$parameters$pro[i] > (1/(length(m500.12$parameters$pro)+param1))){
				target.s12[i] <- i
		} else if( var13.s12[i] < param2*mean(var13.s12)){
				target.s12[i] <- i
		} else if( var100.s12[i] < param2*mean(var100.s12)){
				target.s12[i] <- i
		}
	}

	tpro  <- c(m500.1$parameters$pro[target.s1], m500.2$parameters$pro[target.s2], 
			m500.3$parameters$pro[target.s3], m500.4$parameters$pro[target.s4], 
			m500.5$parameters$pro[target.s5], m500.6$parameters$pro[target.s6], 
			m500.7$parameters$pro[target.s7], m500.8$parameters$pro[target.s8], 
			m500.9$parameters$pro[target.s9], m500.10$parameters$pro[target.s10], 
			m500.11$parameters$pro[target.s11], m500.12$parameters$pro[target.s12])


	tm13  <- c(m500.1$parameters$mean[condition1,target.s1], m500.2$parameters$mean[condition1,target.s2],
			m500.3$parameters$mean[condition1,target.s3], m500.4$parameters$mean[condition1,target.s4], 
			m500.5$parameters$mean[condition1,target.s5], m500.6$parameters$mean[condition1,target.s6], 
			m500.7$parameters$mean[condition1,target.s7], m500.8$parameters$mean[condition1,target.s8], 
			m500.9$parameters$mean[condition1,target.s9], m500.10$parameters$mean[condition1,target.s10], 
			m500.11$parametersmean[condition1,target.s11], m500.12$parameters$mean[condition1,target.s12])
	tm100 <- c(m500.1$parameters$mean[condition2,target.s1], m500.2$parameters$mean[condition2,target.s2],
			m500.3$parameters$mean[condition2,target.s3], m500.4$parameters$mean[condition2,target.s4], 
			m500.5$parameters$mean[condition2,target.s5], m500.6$parameters$mean[condition2,target.s6], 
			m500.7$parameters$mean[condition2,target.s7], m500.8$parameters$mean[condition2,target.s8], 
			m500.9$parameters$mean[condition2,target.s9], m500.10$parameters$mean[condition2,target.s10], 
			m500.11$parametersmean[condition2,target.s11], m500.12$parameters$mean[condition2,target.s12])	
	tv13  <- c(var13.s1[target.s1], var13.s2[target.s2], var13.s3[target.s3], var13.s4[target.s4],
			 var13.s5[target.s5], var13.s6[target.s6], var13.s7[target.s7], var13.s8[target.s8],
			 var13.s9[target.s9], var13.s10[target.s10], var13.s11[target.s11], var13.s12[target.s12])
	tv100 <- c(var100.s1[target.s1], var100.s2[target.s2], var100.s3[target.s3], var100.s4[target.s4],
			 var100.s5[target.s5], var100.s6[target.s6], var100.s7[target.s7], var100.s8[target.s8],
			 var100.s9[target.s9], var100.s10[target.s10], var100.s11[target.s11], var100.s12[target.s12])

	plot(data[,c(condition1,condition2)], xlim=c(-6,4), ylim=c(-5.2,6.2), pch=".", xlab="Dimension 13", ylab="Dimension 100")
	for(i in 1:length(tm13)){
	plotellipse(mid = c(tm13[i], tm100[i]),
				 rx = sqrt(tv13[i]), 
				 ry = sqrt(tv100[i]))
	}
	list("tpro" = tpro, "tm13" = tm13, "tm100" = tm100, "tv13" = tv13, "tv100" = tv100)
}

SampleBlend(13, 100, 1, 1)

L <- SampleBlend(13, 100, 1, 1)
sort(L$tm100)
plot(data[,c(13,100)], xlim=c(-6,4), ylim=c(-5.2,6.2), pch="+", lwd=6, xlab="Dimension 13", ylab="Dimension 100")


########### Extra ###########

m$parameters$mean[13,]
m$parameters$variance$shape[13]
m$parameters$mean[100,]

muniv <- Mclust(data=data500.1[,13], G=1:20, modelNames = c("E", "V"))
muniv$parameters
muniv2 <- Mclust(data=data500.1[,100], G=1:20, modelNames = c("E", "V"))
muniv2$parameters

m250.1$parameters$variance$sigma[,,13]

m250.1$parameters$variance$scale	    ## For Diagonal, EEI, EVI, VEI & VVI
m250.1$parameters$variance$shape	    ## 














#### Not needed
m250.1$parameters$variance$Sigma	    ## For equal var models, EII, EEI & EEE
m250.1$parameters$variance$sigmasq      ## for one dimensional and spherical models
m250.1$parameters$variance$cholsigma    ## For VVV
m250.1$parameters$variance$cholSigma    ## For EEE
m250.1$parameters$variance$orientation  ## For EEV & VEV




############# TEst stuff

G <- c(m1.1$G, m1.2$G, m2.2$G, m3.2$G, m4.2$G, m5.2$G, m6.2$G, m7.2$G) 
plot(G)

m1.2$parameters$mean[173,]
plot(data[,15:16])

sum(m1$uncertainty > 0)

######
data33 <- data[1:1900,1:173]
m33 <- Mclust(data=data33, G=1:20, modelNames = mclust.options("emModelNames"))


