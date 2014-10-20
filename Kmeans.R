a <- read.table('gasch.dat')
attach(a)
require(mclust)

##### Data Clean #####
n = length(V1)		# Row    Length(V1)
m = 173		  	# Col    V1:173
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
######## Highest Variance Sample 
sigma2 <- matrix(rep(0,n),n,1)

for(i in 1:n){
	sigma2[i] <- var(b[i,])
}


B <- matrix(rep(0,n*(m+1)),n,(m+1))
B[1:n,1:m] <-b
B[1:n,m+1] <-sigma2


data <- B[order(B[,174], decreasing=TRUE),]

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


########### K means ###################

nk <- 13 # Number of Clusters

k1 <- kmeans( data[,1:173], nk, algorithm = "Lloyd", nstart = 100, iter.max = 500)
k1
k1$size
k1$cluster
k1$betweens
k1$centers
sum(k1$withinss)
k1$tot.withinss


k2 <- kmeans( data2, nk, algorithm = "Lloyd", iter.max = 50)

##### Testing for number of components ######

ktot <- rep(0,(100))
for(i in 1:100){
	ktot[i] <- kmeans( data[,1:173], i, algorithm = "Lloyd", nstart = 100, iter.max = 1000)$tot.withinss
}

plot(ktot[1:100], xlab = "k", ylab= "Total Within-Cluster Sum of Squares")


kF10 <- kmeans( data[,1:173], 10, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF11 <- kmeans( data[,1:173], 11, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF12 <- kmeans( data[,1:173], 12, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF13 <- kmeans( data[,1:173], 13, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF14 <- kmeans( data[,1:173], 14, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF15 <- kmeans( data[,1:173], 15, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF16 <- kmeans( data[,1:173], 16, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF17 <- kmeans( data[,1:173], 17, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF18 <- kmeans( data[,1:173], 18, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF19 <- kmeans( data[,1:173], 19, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF20 <- kmeans( data[,1:173], 20, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF21 <- kmeans( data[,1:173], 21, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF22 <- kmeans( data[,1:173], 22, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF23 <- kmeans( data[,1:173], 23, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF24 <- kmeans( data[,1:173], 24, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF25 <- kmeans( data[,1:173], 25, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF26 <- kmeans( data[,1:173], 26, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF27 <- kmeans( data[,1:173], 27, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF28 <- kmeans( data[,1:173], 28, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF29 <- kmeans( data[,1:173], 29, algorithm = "Lloyd", nstart = 100, iter.max = 1000)
kF30 <- kmeans( data[,1:173], 30, algorithm = "Lloyd", nstart = 100, iter.max = 1000)


######### Plotting ###############

kMeansClustPlot <- function(data, clusters, condition1, condition2){
	n <- length(data[,1])
	k <- kmeans( data, clusters, algorithm = "Lloyd", nstart=1000, iter.max=100)
	clust <- array(0, dim=c(n,173,clusters))
	for(b in 1:clusters){
		a <- 1
		for(i in 1:n){
			if(k$cluster[i] == b){
				clust[a, ,b] =  data[i,]
				a <- a + 1
			}
		}
	}

	xmin <- min(data[,condition1])
	xmax <- max(data[,condition1])
	ymin <- min(data[,condition2])
	ymax <- max(data[,condition2])
	colour <- rainbow(clusters, start=0)

	for(i in 1:clusters){
		if(i<9){
			plot(k$centers[i,condition1],k$centers[i,condition2], pch=16, col=colour[i], ylim=c(ymin-0.5,ymax+0.5), xlim=c(xmin-0.5,xmax+0.5))
			par(new=TRUE)
			plot(clust[1:k$size[i],condition1,i],clust[1:k$size[i],condition2,i], pch="+", col=colour[i], ylim=c(ymin-0.5,ymax+0.5), xlim=c(xmin-0.5,xmax+0.5))
			par(new=TRUE)
		}else{
			plot(k$centers[i,condition1],k$centers[i,condition2], pch=18, col=colour[i], ylim=c(ymin-0.5,ymax+0.5), xlim=c(xmin-0.5,xmax+0.5))
			par(new=TRUE)
			plot(clust[1:k$size[i],condition1,i],clust[1:k$size[i],condition2,i], pch="*", col=colour[i], ylim=c(ymin-0.5,ymax+0.5), xlim=c(xmin-0.5,xmax+0.5))
			par(new=TRUE)
		}
	}
}

kMeansClustPlot(data1, 13, 13, 100)
kMeansClustPlot(data[,1:173], 13, 13, 100)

#### Incomplete needs work for final comparison, look at HierClustCompPlot function ####

kMeansCP <- function(data, k, clusters, condition1, condition2){
	n <- length(data[,1])
	clust <- array(0, dim=c(n,173,clusters))
	for(b in 1:clusters){
		a <- 1
		for(i in 1:n){
			if(k$cluster[i] == b){
				clust[a, ,b] =  data[i,]
				a <- a + 1
			}
		}
	}

	clustM <- array(0, dim=c(n,173,clusters))
	clustV <- array(0, dim=c(n,173,clusters))
	tm <- matrix(rep(0, clusters), clusters, 2) 
	tv <- matrix(rep(0, clusters), clusters, 2) 

	for(b in 1:clusters){
		a <- 1
		for(i in 1:n){
			if(k$cluster[i] == b){
				clustM[a, ,b] =  data[i,]
				clustV[a, ,b] =  data[i,]
				a <- a + 1
			}
		}
		nonz1 <- length(clustM[, 1,b]) - sum(clustM[, 1,b] == 0)
		nonz2	<- length(clustM[, 2,b]) - sum(clustM[, 2,b] == 0)
		
		tm[b, 1] <- mean(clustM[1:nonz1, 13,b])
		tm[b, 2] <- mean(clustM[1:nonz2, 100,b]) 

		if(nonz1 == 1){
			tv[b, 1] <- var(clustV[1:nonz1, 13,b])
		} else {
			tv[b, 1] <- var(clustV[1:nonz1, 13,b])
		}
		if(nonz2 == 1){
			tv[b, 2] <- var(clustV[1:nonz2, 100,b])
		} else {
			tv[b, 2] <- var(clustV[1:nonz2, 100,b])
		}
	}

	colour <- rainbow(clusters, start=0)

	for(i in 1:clusters){
		if(i<9){
			plot(clust[1:k$size[i],condition1,i],clust[1:k$size[i],condition2,i], pch="+", col=colour[i], xlim=c(-6,4), ylim=c(-5.2,6.2), xlab="", ylab="")
			par(new=TRUE)
		}else{
			plot(clust[1:k$size[i],condition1,i],clust[1:k$size[i],condition2,i], pch="*", col=colour[i], xlim=c(-6,4), ylim=c(-5.2,6.2), xlab="Dimension 13", ylab="Dimension 100")
			par(new=TRUE)
		}
	}
}

kMeansCP(data[,1:173], kF15, 15, 13, 100)
kMeansCPClust(data[,1:173], kF15, 15, 13, 100)

kF500.1 <- kmeans( data500.1[,1:173], 13, algorithm = "Lloyd", nstart = 100, iter.max = 1000)

kMeansCP(data500.1[,1:173], kF500.1, 13, 13, 100)
kMeansCPClust(data500.1[,1:173], kF500.1, 13, 13, 100)


kMeansCPClust <- function(data, k, clusters, condition1, condition2){
	n <- length(data[,1])
	clust <- array(0, dim=c(n,173,clusters))
	for(b in 1:clusters){
		a <- 1
		for(i in 1:n){
			if(k$cluster[i] == b){
				clust[a, ,b] =  data[i,]
				a <- a + 1
			}
		}
	}

	clustM <- array(0, dim=c(n,173,clusters))
	clustV <- array(0, dim=c(n,173,clusters))
	tm <- matrix(rep(0, clusters), clusters, 2) 
	tv <- matrix(rep(0, clusters), clusters, 2) 

	for(b in 1:clusters){
		a <- 1
		for(i in 1:n){
			if(k$cluster[i] == b){
				clustM[a, ,b] =  data[i,]
				clustV[a, ,b] =  data[i,]
				a <- a + 1
			}
		}
		nonz1 <- length(clustM[, 1,b]) - sum(clustM[, 1,b] == 0)
		nonz2	<- length(clustM[, 2,b]) - sum(clustM[, 2,b] == 0)
		
		tm[b, 1] <- mean(clustM[1:nonz1, 13,b])
		tm[b, 2] <- mean(clustM[1:nonz2, 100,b]) 

		if(nonz1 == 1){
			tv[b, 1] <- var(clustV[1:nonz1, 13,b])
		} else {
			tv[b, 1] <- var(clustV[1:nonz1, 13,b])
		}
		if(nonz2 == 1){
			tv[b, 2] <- var(clustV[1:nonz2, 100,b])
		} else {
			tv[b, 2] <- var(clustV[1:nonz2, 100,b])
		}
	}

	colour <- rainbow(clusters, start=0)

	for(i in 1:clusters){
			plot(k$centers[i,condition1],k$centers[i,condition2], pch=16, col="black", xlim=c(-6,4), ylim=c(-5.2,6.2),xlab="Dimension 13", ylab="Dimension 100")
			par(new=TRUE)
	}
	for(i in 1:clusters){
		plotellipse(mid = c(tm[i,1], tm[i,2]),
				 rx = sqrt(tv[i,1]), 
				 ry = sqrt(tv[i,2]), lcol = colour[i])
	}
}


### ignore
kMeansBIC <- function(data, clusters){
	
	clust <- array(0, dim=c(n,173,clusters))
	for(b in 1:clusters){
		a <- 1
		for(i in 1:n){
			if(k$cluster[i] == b){
				clust[a, ,b] =  data[i,]
				a <- a + 1
			}
		}
	}

	varclust <- rep(0, cluster)
	for(i
	k <- kmeans( data1, nk, nstart = 100, iter.max = 100)
	k$centers
	BIC <- 2*m1$loglik - m1$df*log(500)
	
}


kMeansBIC(data1, 13)


##### MV plotting laplace and gaussian NOT WORKING ####

n <- 50
llim <- -5
ulim <- 5
int <- (ulim-llim)/50

sigma <- array(0, dim=c(2,2,2))
sigma[,,1] <- matrix(c(1,0,0,1), 2, 2)
sigma[,,2] <- matrix(c(0,2,2,0), 2, 2)
mu <- rep(c(0,2),1)
z <- matrix( c(rep(seq(llim, ulim, int),2)) ,n+1,2)

y <-


MN1 <- rep(0,n+1)

for(i in 1:n+1){
	MN1[i] <- det(sigma[,,1])^(1/2) * exp( (-1/2) * t(z[i,]-mu[1]) %*% solve(sigma[,,1]) %*% (z[i,]-mu[1]) )
}

x <- seq(llim, ulim, int)
plot(x,MN1)

for(i in 1:n+1){
	MN1[i] <- det(sigma[,,1])^(1/2) * exp( (-1/2) * t(z[i,]-mu[1]) %*% solve(sigma[,,1]) %*% (z[i,]-mu[1]) )
}


lambda <- 3.5
d <- 1
Q <- t(z[i,]-mu[1]) %*% solve(sigma[,,1]) %*% (z[i,]-mu[1])
ML <- (1/((2*pi)^(d/2))) * (2/lambda) * ( besselY(sqrt((2/lambda)*Q), ((d/2)-1)) / (sqrt((lambda/2)*Q)^((d/2)-1)))
ML

ML1 <- rep(0,n+1)
for(i in 1:n+1){
	ML1[i] <- (1/((2*pi)^(d/2))) * (2/lambda) * ( besselY(sqrt((2/lambda)*Q), ((d/2)-1)) / (sqrt((lambda/2)*Q)^((d/2)-1)))
}
ML1
plot(ML1)

dev.new()

