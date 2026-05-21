set.seed(123)
n<-10000
rho<--1
U<-rnorm(n)
V<-rnorm(n)
X<-U
Y<-rho*U+sqrt(1-rho^2)*V
cov(X,Y)
plot(X,Y,pch=18,cex=0.4,main="simulated BN",xlab="X",ylab="Y")
fxy<-function(x,y,rho){
  1/(2*pi*sqrt(1-rho^2))*exp(-(x^2-2*rho*x*y+y^2)/(2*(1-rho^2)))
}
fxy(0.5,0.2,rho=0.7)
cov(cbind(X,Y))
rho<-0.7
x<-seq(-3,3,length=100)
y<-seq(-3,3,length=100)
z<-outer(x,y,Vectorize(function(x,y)fxy(x,y,rho)))
contour(x,y,z,xlab="x",ylab="y",main=paste("Contour plot,rho=",rho))
filled.contour(x,y,z,xlab="x",ylab="y",main=paste("Filled contour,rho=",rho))
persp(x,y,z,theta=30,phi=30,col="lightblue",ticktype="detailed",main=paste("3D surface,rho=",rho))
contour(x,y,z,nlevels=20,col="blue",main=paste("Contour plot,rho=",rho))
