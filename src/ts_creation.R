
library(doParallel)  
no_cores <- detectCores() - 1  
registerDoParallel(cores=no_cores)  

# creation of time series 
len <- 100
n_species<- 3
n_sites <- 10
n_env<- 2
mar_p<- 4
density_mar<- 0.3


x<- sim.mar(n_series = 2, mar_p = 1,density_mar = 0.5,sd_mar = 2,sd_noise=0.3,sd_initial = 1)
a_mar<- x$A_mar
x<- x$time_series
a_mar
causal.test(x,p=1, method= "gc.pairwise")
causal.test(x,p=1, method= "ccm")


#A Interaction matrix 
A <- matrix((runif(n_species^2)),nrow = n_species,ncol=n_species)
A <- lapply(A,function(x) if(x<0.7){x<-0}else{x})
A <- matrix(unlist(A),nrow = n_species,ncol=n_species)
diag(A)<- abs(rnorm(n_species,sd = 0.2))


#B Environmental influences on taxa 
B<- matrix(runif(n_env^2),nrow=n_env,ncol=n_env) 


foreach(i=1:dim(time_series)[1]) %dopar%{ 
 time_series[i]
}


t1<- abs(sin((1:len)/7))+rnorm(len,0,0.1)
t2<- c(rnorm(1,0,0.1),0.3*t1+rnorm(len,0,0.1))[1:len-1]
t3<- c(rnorm(1,0,0.1),0.3*t1+rnorm(len,0,0.1))[1:len-1]
plot(t1,type = "l",col="blue")
lines(t2,col="red")
lines(t3,col="green")

hist(t2-t3)
