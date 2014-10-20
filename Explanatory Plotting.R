require(shape)
detach("package:shape", unload=TRUE)


############ EII Model ###################
sd <- 0.3
var <- sd^2

x1 <- rnorm(10,0.5,var)
x2 <- rnorm(10,1.5,var)
x3 <- rnorm(10,2,var)
x <- c(x1,x2,x3)

y1 <- rnorm(10,1.5,var)
y2 <- rnorm(10,3,var)
y3 <- rnorm(10,2,var)
y <- c(y1,y2,y3)

plot(x,y, xlim=c(0,2.5), ylim=c(0,4), pch="+")
plotcircle(mid = c(0.5, 1.5), r = sd, lcol="blue")
plotcircle(mid = c(1.5, 3), r = sd, lcol="red")
plotcircle(mid = c(2, 2), r = sd)

############ VII Model ###################
sd1 <- 0.2
var1 <- sd1^2
sd2 <- 0.4
var2 <- sd2^2
sd3 <- 0.6
var3 <- sd3^2

x1 <- rnorm(10,0.5,var1)
x2 <- rnorm(10,1.5,var2)
x3 <- rnorm(10,1.5,var3)
x <- c(x1,x2,x3)

y1 <- rnorm(10,1.5,var1)
y2 <- rnorm(10,3,var2)
y3 <- rnorm(10,1,var3)
y <- c(y1,y2,y3)

plot(x,y, xlim=c(0,2.5), ylim=c(0,4), pch="+")
plotcircle(mid = c(0.5, 1.5), r = sd1, lcol="blue")
plotcircle(mid = c(1.5, 3), r = sd2, lcol="red")
plotcircle(mid = c(1.5, 1), r = sd3)

############ EEI Model ###################
sdx1 <- 0.2
varx1 <- sdx1^2
sdx2 <- 0.2
varx2 <- sdx2^2
sdx3 <- 0.2
varx3 <- sdx3^2

sdy1 <- 0.8
vary1 <- sdy1^2
sdy2 <- 0.8
vary2 <- sdy2^2
sdy3 <- 0.8
vary3 <- sdy3^2

x1 <- rnorm(10,0.5,varx1)
x2 <- rnorm(10,1.5,varx2)
x3 <- rnorm(10,2,varx3)
x <- c(x1,x2,x3)

y1 <- rnorm(10,1.5,vary1)
y2 <- rnorm(10,3,vary2)
y3 <- rnorm(10,1,vary3)
y <- c(y1,y2,y3)

plot(x,y, xlim=c(0,2.5), ylim=c(0,4), pch="+")
plotellipse(mid = c(0.5, 1.5),rx = sdx1, ry = sdy1, lcol="blue")
plotellipse(mid = c(1.5, 3), rx = sdx2, ry = sdy2, lcol="red")
plotellipse(mid = c(2, 1), rx = sdx3, ry = sdy3)

############ VEI Model ###################
sdx1 <- 0.1
varx1 <- sdx1^2
sdx2 <- 0.1
varx2 <- sdx2^2
sdx3 <- 0.1
varx3 <- sdx3^2

sdy1 <- 0.3
vary1 <- sdy1^2
sdy2 <- 0.5
vary2 <- sdy2^2
sdy3 <- 0.8
vary3 <- sdy3^2

x1 <- rnorm(10,0.5,varx1)
x2 <- rnorm(10,1.5,varx2)
x3 <- rnorm(10,2,varx3)
x <- c(x1,x2,x3)

y1 <- rnorm(10,1.5,vary1)
y2 <- rnorm(10,3,vary2)
y3 <- rnorm(10,1,vary3)
y <- c(y1,y2,y3)

plot(x,y, xlim=c(0,2.5), ylim=c(0,4), pch="+")
plotellipse(mid = c(0.5, 1.5),rx = sdx1, ry = sdy1, lcol="blue")
plotellipse(mid = c(1.5, 3), rx = sdx2, ry = sdy2, lcol="red")
plotellipse(mid = c(2, 1), rx = sdx3, ry = sdy3)

############ EVI Model ###################
sdx1 <- 0.1
varx1 <- sdx1^2
sdx2 <- 0.5
varx2 <- sdx2^2
sdx3 <- 0.1
varx3 <- sdx3^2

sdy1 <- 0.5
vary1 <- sdy1^2
sdy2 <- 0.1
vary2 <- sdy2^2
sdy3 <- 0.5
vary3 <- sdy3^2

x1 <- rnorm(10,0.5,varx1)
x2 <- rnorm(10,1.5,varx2)
x3 <- rnorm(10,2,varx3)
x <- c(x1,x2,x3)

y1 <- rnorm(10,1.5,vary1)
y2 <- rnorm(10,2,vary2)
y3 <- rnorm(10,1,vary3)
y <- c(y1,y2,y3)

plot(x,y, xlim=c(0,2.5), ylim=c(0,2.5), pch="+")
plotellipse(mid = c(0.5, 1.5),rx = sdx1, ry = sdy1, lcol="blue")
plotellipse(mid = c(1.5, 2), rx = sdx2, ry = sdy2, lcol="red")
plotellipse(mid = c(2, 1), rx = sdx3, ry = sdy3)

############ VVI Model ###################
sdx1 <- 0.1
varx1 <- sdx1^2
sdx2 <- 0.7
varx2 <- sdx2^2
sdx3 <- 0.3
varx3 <- sdx3^2

sdy1 <- 0.2
vary1 <- sdy1^2
sdy2 <- 0.1
vary2 <- sdy2^2
sdy3 <- 0.5
vary3 <- sdy3^2

x1 <- rnorm(10,0.5,varx1)
x2 <- rnorm(10,1.5,varx2)
x3 <- rnorm(10,2,varx3)
x <- c(x1,x2,x3)

y1 <- rnorm(10,1.5,vary1)
y2 <- rnorm(10,2,vary2)
y3 <- rnorm(10,1,vary3)
y <- c(y1,y2,y3)

plot(x,y, xlim=c(0,2.5), ylim=c(0,2.5), pch="+")
plotellipse(mid = c(0.5, 1.5),rx = sdx1, ry = sdy1, lcol="blue")
plotellipse(mid = c(1.5, 2), rx = sdx2, ry = sdy2, lcol="red")
plotellipse(mid = c(2, 1), rx = sdx3, ry = sdy3)

############ Test EEE Model ###################
require(mvtnorm)

sdx1 <- 0.1
varx1 <- sdx1^2
sdx2 <- 0.1
varx2 <- sdx2^2

sdy1 <- 0.6
vary1 <- sdy1^2
sdy2 <- 0.6
vary2 <- sdy2^2

cx1 <- 0.5
cx2 <- 2
cy1 <- 1.5
cy2 <- 1.5

X <-  rmvnorm(n=1000,mean=c(1,2),sigma=matrix(c(2,0,0,1),2,2))
plot(X)
x1lim <- c(cx1 - (sdy1*sin(30*(180/pi))), cx1 + (sdy1*sin(30*(180/pi)))  )
x2lim <- c(cx2 - (sdy2*sin(30*(180/pi))), cx2 + (sdy2*sin(30*(180/pi)))  )
y1lim <- c(cy1 - (sdy1*cos(30*(180/pi))), cy1 + (sdy1*cos(30*(180/pi)))  )
y2lim <- c(cy2 - (sdy2*cos(30*(180/pi))), cy2 + (sdy2*cos(30*(180/pi)))  )

x1 <- seq(x1lim[1],x1lim[2],((x1lim[2]-x1lim[1])/10) )
x2 <- seq(x2lim[1],x2lim[2],((x2lim[2]-x2lim[1])/10) )
x <- c(x1,x2)

y1 <- seq(y1lim[2],y1lim[1],abs((y1lim[2]-y1lim[1])/10) )
y2 <- seq(y2lim[2],y2lim[1],abs((y2lim[2]-y2lim[1])/10) )
y <- c(y1,y2)

plot(x,y, xlim=c(0,2.5), ylim=c(0,2.5), pch="+")
plotellipse(mid = c(cx1, cy1),rx = sdx1, ry = sdy1, lcol="blue", angle=25)
plotellipse(mid = c(cx2, cy2), rx = sdx2, ry = sdy2, lcol="red", angle=25)

############ EEE Model ###################
require(mvtnorm)

mean1 <- c(1,2)
lambda1 <- 0.5
D1 <- matrix(c(0.5,0,0.2,0.2),2,2)
A1 <- matrix(c(0.5,0,0,4),2,2)
sig1 <- lambda1 * D1 %*% A1 %*% t(D1)
sig1
X1 <-  rmvnorm(n=100,mean=mean1,sigma=sig1)

mean2 <- c(3,2)
lambda2 <- 0.5
D2 <- matrix(c(0.5,0,0.2,0.2),2,2)
A2 <- matrix(c(0.5,0,0,4),2,2)
sig2 <- lambda2 * D2 %*% A2 %*% t(D2)
sig2
X2 <-  rmvnorm(n=100,mean=mean2,sigma=sig2)

X <- rbind(X1, X2)

plot(X, xlim=c(-1,5), ylim=c(-1,5), pch="+")
plotellipse(mid = mean1,rx = 0.5, ry = 1.5, lcol="blue", angle=-55)
plotellipse(mid = mean2, rx = 0.5, ry = 1.5, lcol="red", angle=-55)

############ EEV Model ###################
require(mvtnorm)

mean1 <- c(1,2)
lambda1 <- 0.5
D1 <- matrix(c(0.5,0,0.2,0.2),2,2)
A1 <- matrix(c(0.5,0,0,4),2,2)
sig1 <- lambda1 * D1 %*% A1 %*% t(D1)
sig1
X1 <-  rmvnorm(n=100,mean=mean1,sigma=sig1)

mean2 <- c(3,2)
lambda2 <- 0.5
D2 <- matrix(c(0.4,0.3,0.3,0),2,2)
A2 <- matrix(c(0.5,0,0,4),2,2)
sig2 <- lambda2 * D2 %*% A2 %*% t(D2)
sig2
X2 <-  rmvnorm(n=100,mean=mean2,sigma=sig2)

X <- rbind(X1, X2)

plot(X, xlim=c(-1,5), ylim=c(-1,5), pch="+")  
plotellipse(mid = mean1,rx = 0.5, ry = 1.5, lcol="blue", angle=-55)
plotellipse(mid = mean2, rx = 0.5, ry = 1.5, lcol="red", angle=-84)

############ VEV Model ###################
require(mvtnorm)

mean1 <- c(1,2)
lambda1 <- 0.5
D1 <- matrix(c(0.5,0,0.2,0.2),2,2)
A1 <- matrix(c(0.5,0,0,4),2,2)
sig1 <- lambda1 * D1 %*% A1 %*% t(D1)
sig1
X1 <-  rmvnorm(n=100,mean=mean1,sigma=sig1)

mean2 <- c(3,2)
lambda2 <- 0.5
D2 <- matrix(c(-0.5,0,0.1,-0.5),2,2)
A2 <- matrix(c(0.5,0,0,4),2,2)
sig2 <- lambda2 * D2 %*% A2 %*% t(D2)
sig2
X2 <-  rmvnorm(n=100,mean=mean2,sigma=sig2)

X <- rbind(X1, X2)

plot(X, xlim=c(-1,5), ylim=c(-1,5), pch="+")  
plotellipse(mid = mean1,rx = 0.5, ry = 1.5, lcol="blue", angle=-55)
plotellipse(mid = mean2, rx = 0.7, ry = 2, lcol="red", angle=10)

########### Manual K means #######################

###### Initial
sd1 <- 0.35
var1 <- sd1^2
sd2 <- 0.6
var2 <- sd2^2

x1 <- rnorm(100,0.5,var1)
x2 <- rnorm(100,2,var2)
x <- c(x1,x2)
y1 <- rnorm(100,1.5,var1)
y2 <- rnorm(100,3,var2)
y <- c(y1,y2)

gx1 <- 2.5; gy1 <- 1;
gx2 <- 0.5; gy2 <- 3;

plot(x,y, xlim=c(0,3), ylim=c(0,4), pch="+", col ="green", xlab="X", ylab="Y")
par(new=T)
plot(gx1, gy1, xlim=c(0,3), ylim=c(0,4), pch=16, col ="red", xlab="X", ylab="Y")
par(new=T)
plot(gx2,gy2, xlim=c(0,3), ylim=c(0,4), pch=16, col ="blue", xlab="X", ylab="Y")

############ Step 1 Assign #################

gx1 <- 2.5; gy1 <- 1;
gx2 <- 0.5; gy2 <- 3;

ED1 <- rep(0, length(y))
ED2 <- rep(0, length(y))
xc1 <- rep(0, length(y)); xc2 <- rep(0, length(y))
yc1 <- rep(0, length(y)); yc2 <- rep(0, length(y))
for(i in 1:length(y)){
	ED1[i] <- abs(x[i] - gx1)^2 + abs(y[i] - gy1)^2
	ED2[i] <- abs(x[i] - gx2)^2 + abs(y[i] - gy2)^2
	if(ED1[i] < ED2[i]){
		xc1[i] <- x[i]
		yc1[i] <- y[i]
	} else {
		xc2[i] <- x[i]
		yc2[i] <- y[i]
	}	 
}

xc1 <- xc1[xc1 != 0] 
xc2 <- xc2[xc2 != 0] 
yc1 <- yc1[yc1 != 0] 
yc2 <- yc2[yc2 != 0]

plot(xc1,yc1, xlim=c(0,3), ylim=c(0,4), pch="+", col ="red", xlab="X", ylab="Y")
par(new=T)
plot(xc2,yc2, xlim=c(0,3), ylim=c(0,4), pch="+", col ="blue", xlab="X", ylab="Y")
par(new=T)
plot(gx1, gy1, xlim=c(0,3), ylim=c(0,4), pch=16, col ="red", xlab="X", ylab="Y")
par(new=T)
plot(gx2,gy2, xlim=c(0,3), ylim=c(0,4), pch=16, col ="blue", xlab="X", ylab="Y")

############ Step 1 Update #################

gx1 <- mean(xc1); gy1 <- mean(yc1);
gx2 <- mean(xc2); gy2 <- mean(yc2);


plot(xc1,yc1, xlim=c(0,3), ylim=c(0,4), pch="+", col ="red", xlab="X", ylab="Y")
par(new=T)
plot(xc2,yc2, xlim=c(0,3), ylim=c(0,4), pch="+", col ="blue", xlab="X", ylab="Y")
par(new=T)
plot(gx1, gy1, xlim=c(0,3), ylim=c(0,4), pch=16, col ="red", xlab="X", ylab="Y")
par(new=T)
plot(gx2,gy2, xlim=c(0,3), ylim=c(0,4), pch=16, col ="blue", xlab="X", ylab="Y")


############ Step 2 Assign #################

gx1 <- mean(xc1); gy1 <- mean(yc1);
gx2 <- mean(xc2); gy2 <- mean(yc2);

ED1 <- rep(0, length(y))
ED2 <- rep(0, length(y))
xc1 <- rep(0, length(y)); xc2 <- rep(0, length(y))
yc1 <- rep(0, length(y)); yc2 <- rep(0, length(y))
for(i in 1:length(y)){
	ED1[i] <- abs(x[i] - gx1)^2 + abs(y[i] - gy1)^2
	ED2[i] <- abs(x[i] - gx2)^2 + abs(y[i] - gy2)^2
	if(ED1[i] < ED2[i]){
		xc1[i] <- x[i]
		yc1[i] <- y[i]
	} else {
		xc2[i] <- x[i]
		yc2[i] <- y[i]
	}
	
}

xc1 <- xc1[xc1 != 0] 
xc2 <- xc2[xc2 != 0] 
yc1 <- yc1[yc1 != 0] 
yc2 <- yc2[yc2 != 0] 

plot(xc1,yc1, xlim=c(0,3), ylim=c(0,4), pch="+", col ="red", xlab="X", ylab="Y")
par(new=T)
plot(xc2,yc2, xlim=c(0,3), ylim=c(0,4), pch="+", col ="blue", xlab="X", ylab="Y")
par(new=T)
plot(gx1, gy1, xlim=c(0,3), ylim=c(0,4), pch=16, col ="red", xlab="X", ylab="Y")
par(new=T)
plot(gx2,gy2, xlim=c(0,3), ylim=c(0,4), pch=16, col ="blue", xlab="X", ylab="Y")

############ Step 2 Update #################

gx1 <- mean(xc1); gy1 <- mean(yc1);
gx2 <- mean(xc2); gy2 <- mean(yc2);


plot(xc1,yc1, xlim=c(0,3), ylim=c(0,4), pch="+", col ="red", xlab="X", ylab="Y")
par(new=T)
plot(xc2,yc2, xlim=c(0,3), ylim=c(0,4), pch="+", col ="blue", xlab="X", ylab="Y")
par(new=T)
plot(gx1, gy1, xlim=c(0,3), ylim=c(0,4), pch=16, col ="red", xlab="X", ylab="Y")
par(new=T)
plot(gx2,gy2, xlim=c(0,3), ylim=c(0,4), pch=16, col ="blue", xlab="X", ylab="Y")

z <- c(x,y)
dim(z) <- c(200,2)
z

k <- kmeans(z, 2, nstart = 100, iter.max = 100)
k$centers

n <- length(z[,1])
clusters <- 2
clust <- array(0, dim=c(n,2,clusters))
for(b in 1:clusters){
	a <- 1
	for(i in 1:n){
		if(k$cluster[i] == b){
			clust[a, ,b] =  z[i,]
			a <- a + 1
		}
	}
}

	xmin <- min(z[,1])
	xmax <- max(z[,1])
	ymin <- min(z[,2])
	ymax <- max(z[,2])
}


for(i in 1:clusters){
	plot(k$centers[1,1],k$centers[1,2], pch=16, col="blue",  xlim=c(0,3), ylim=c(0,4), xlab="X", ylab="Y")
	par(new=TRUE)
	plot(clust[1:k$size[i],1,1],clust[1:k$size[i],2,1], pch="+", col="blue",  xlim=c(0,3), ylim=c(0,4), xlab="X", ylab="Y")
	par(new=TRUE)
	plot(k$centers[2,1],k$centers[2,2], pch=16, col="red",  xlim=c(0,3), ylim=c(0,4), xlab="X", ylab="Y")
	par(new=TRUE)
	plot(clust[1:k$size[i],1,2],clust[1:k$size[i],2,2], pch="+", col="red",  xlim=c(0,3), ylim=c(0,4), xlab="X", ylab="Y")
	par(new=TRUE)
}

j <- kMeansClustPlot(z, 2, 1, 2)


############## Hierarchical ###############


data[1:500,]

d <- dist(data[11:20,], method="euclidean")
x <- hclust(d, method = "complete")
plot(x)

x$merge
x$order
x$height
x$method
x$dist.method

dist(rbind(data[13,],data[20,]))

groups <- cutree(x, k=5)
rect.hclust(x, k=4, border="red")

