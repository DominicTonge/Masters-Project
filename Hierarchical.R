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

sigma2

B <- matrix(rep(0,n*(m+1)),n,(m+1))
B[1:n,1:m] <-b
B[1:n,m+1] <-sigma2
B

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

########## Hierarcchical Clustering ##############

d <- dist(data1[1:500,], method="euclidean")
x <- hclust(d, method = "complete")
plot(x)


x$merge
x$order
x$height

groups <- cutree(x, k=13)
rect.hclust(x, k=13, border="red")

HierClustPlot <- function(data, clusters, condition1, condition2, dim){
	n <- length(data[,1])
	d <- dist(data, method="euclidean")
	h <- hclust(d, method="complete")
	groups <- cutree(h, clusters)
	clust <- array(0, dim=c(n,dim,clusters))
	for(b in 1:clusters){
		a <- 1
		for(i in 1:n){
			if(groups[i] == b){
				clust[a, ,b] =  data[i,]
				a <- a + 1
			}
		}
	}
	
	size <- as.data.frame(table(groups))
	
	xmin <- min(data[,condition1])
	xmax <- max(data[,condition1])
	ymin <- min(data[,condition2])
	ymax <- max(data[,condition2])
	colour <- rainbow(clusters,start=0)

	for(i in 1:clusters){
		if(i<9){
			plot(clust[1:size[i,2],condition1,i],clust[1:size[i,2],condition2,i], pch="+", col=colour[i], ylim=c(ymin-0.5,ymax+0.5), xlim=c(xmin-0.5,xmax+0.5))
			par(new=TRUE)
		}else if(i>8 & i<clusters){
			plot(clust[1:size[i,2],condition1,i],clust[1:size[i,2],condition2,i], pch="*", col=colour[i], ylim=c(ymin-0.5,ymax+0.5), xlim=c(xmin-0.5,xmax+0.5))
			par(new=TRUE)
		}else{
			plot(clust[1:size[i,2],condition1,i],clust[1:size[i,2],condition2,i], pch="*", col=colour[i], ylim=c(ymin-0.5,ymax+0.5), xlim=c(xmin-0.5,xmax+0.5))
		}
	}
	list("size" = size, "groups" = groups)
}

HierClustPlot(data1, 10, 13, 100, 173)

####### using gaussian data and 

L <- SampleBlend(13, 100, 1, 1, 2)
L$tv13
L$tm100
LC <- cbind( L$tm13 , L$tm100, L$tv13 , L$tv100 )
LC <- LC[1:58,]


Hier <- HierClustPlot(LC, 15, 1, 2, 2)

Hier$size
Hier$groups

HierClustCompPlot(LC, 15, 1, 2, 2)
clusters <-10
n<- length(LC[,1])
dim <-2
d <- dist(LC[,1:2], method="euclidean")

HierClustCompPlot <- function(data, clusters, condition1, condition2, dim){
	n <- length(data[,1])
	d <- dist(data[,1:2], method="euclidean")
	h <- hclust(d, method="complete")
	groups <- cutree(h, clusters)
	clust <- array(0, dim=c(n,dim,clusters))
	for(b in 1:clusters){
		a <- 1
		for(i in 1:n){
			if(groups[i] == b){
				clust[a, ,b] =  data[i,1:2]
				a <- a + 1
			}
		}
	}

	size <- as.data.frame(table(groups))


	clustM <- array(0, dim=c(n,dim,clusters))
	clustV <- array(0, dim=c(n,dim,clusters))
	tm <- matrix(rep(0, clusters), clusters, 2) 
	tv <- matrix(rep(0, clusters), clusters, 2) 

	for(b in 1:clusters){
		a <- 1
		for(i in 1:n){
			if(groups[i] == b){
				clustM[a, ,b] =  data[i,1:2]
				clustV[a, ,b] =  data[i,3:4]
				a <- a + 1
			}
		}
		nonz1 <- length(clustM[, 1,b]) - sum(clustM[, 1,b] == 0)
		nonz2	<- length(clustM[, 2,b]) - sum(clustM[, 2,b] == 0)
		
		tm[b, 1] <- mean(clustM[1:nonz1, 1,b])
		tm[b, 2] <- mean(clustM[1:nonz2, 2,b]) 

		if(nonz1 == 1){
			tv[b, 1] <- mean(clustV[1:nonz1, 1,b])
		} else {
			tv[b, 1] <- mean(clustV[1:nonz1, 1,b])*size[b,]$Freq/3
		}
		if(nonz2 == 1){
			tv[b, 2] <- mean(clustV[1:nonz2, 2,b])
		} else {
			tv[b, 2] <- mean(clustV[1:nonz2, 2,b])*size[b,]$Freq/3
		}
	}

	colour <- rainbow(clusters,start=0)

	for(i in 1:clusters){
		if(i<9){
			plot(clust[1:size[i,2],condition1,i],clust[1:size[i,2],condition2,i], pch="+", col=colour[i], xlim=c(-6,4), ylim=c(-5.2,6.2), xlab = "", ylab = "")
			par(new=TRUE)
		}else if(i>8 & i<clusters){
			plot(clust[1:size[i,2],condition1,i],clust[1:size[i,2],condition2,i], pch="*", col=colour[i], xlim=c(-6,4), ylim=c(-5.2,6.2), xlab = "", ylab = "")
			par(new=TRUE)
		}else{
			plot(clust[1:size[i,2],condition1,i],clust[1:size[i,2],condition2,i], pch="*", col=colour[i], xlim=c(-6,4), ylim=c(-5.2,6.2), xlab = "Dimension 13", ylab = "Dimension 100")
			par(new=TRUE)
		}
	}
	for(i in 1:clusters){
		plot(tm[i, 1], tm[i, 2], pch=15, col=colour[i], xlim=c(-6,4), ylim=c(-5.2,6.2), xlab = "", ylab = "")
		par(new=TRUE)
	}
	for(i in 1:clusters){
		plotellipse(mid = c(tm[i,1], tm[i,2]),
				 rx = sqrt(tv[i,1]), 
				 ry = sqrt(tv[i,2]), lcol = colour[i])
	}
	list("size" = size, "groups" = groups, "tm" = tm, "tv" = tv)
}

HierClustCompPlot(LC, 15, 1, 2, 2)

O <- HierClustCompPlot(LC, 15, 1, 2, 2)






