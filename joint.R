
lambda1<-2
lambda2<-3
set.seed(123)
x<-rexp(1000,rate=lambda1)
y<-rexp(1000,rate=lambda2)
joint_pdf<-function(x,y){
  lambda1*lambda2
  exp(-(lambda1*x+lambda2*y))
  
}
fx<-function(x){
  lambda1*exp(-lambda1*x)
  
  
}
fy<-function(y){
  lambda2*exp(-lambda2*x)
}
covariance<-cov(x,y)
print(covariance)
cor(x,y)

