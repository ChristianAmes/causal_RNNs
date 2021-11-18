# creation of time series 

n<-100
t1<- abs(sin((1:n)/7))+rnorm(n,0,0.1)
t2<- c(rnorm(1,0,0.1),0.3*t1+rnorm(n,0,0.1))[1:n-1]
t3<- c(rnorm(1,0,0.1),0.3*t1+rnorm(n,0,0.1))[1:n-1]
plot(t1,type = "l",col="blue")
lines(t2,col="red")
lines(t3,col="green")

hist(t2-t3)
