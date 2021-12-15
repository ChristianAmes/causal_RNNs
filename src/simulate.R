sim.mar<- function(n_series=5,mar_p=2,len=100,A_mar= F, density_mar=0.3,
                   sd_mar=0.3, sd_initial=0.3, sd_noise=0.1,
                   center=T, scale=T){
  
  #MAR matrices
  if (is.list(A_mar)){
    mar_p<- length(A_mar[[1]][,1])
  } else if (!A_mar) { 
    A_mar<- list()
    for( i  in 1:n_series){
      A_mar[[i]] <-   Matrix::rsparsematrix(nrow=mar_p,ncol=n_series, 
                            density= density_mar,
                            rand.x = function(n) rnorm(n,0,sd_mar))
    }
  } else { 
  stop ("A_mar must be either F or list of matrices with dimension [n_series,mar_p]") 
  }
  
  
  #create data array
  time_series<- array(0,dim=c(len, n_series))
  time_series[1:mar_p,]<- rnorm(n_series*mar_p,0,sd_initial)
  
  for (i in (mar_p+1):len){
    for(j in 1:n_series){
      time_series[i,j] <- sum(A_mar[[j]] * time_series[(i-mar_p):(i-1),]) + rnorm(1,mean=0,sd=sd_noise)
      }
  }
  
  
  #normalize
  time_series<- scale(time_series, center=center, scale=scale)
  
  
  #create plot
  if(n_series>3){k<-3}else{k<-n_series}
  par(mfrow=c(k,1))
  
  for (i in 1:k){
    try(plot(time_series[,i],type="l",xlab="",ylab="",main = paste("Time Series ",i)))
    
    try(abline(v=mar_p+0.4,col="red",xpd=FALSE))
  }
  
  colnames(time_series)<- paste("Var.",c(1:n_series))
  
  return_obj<- list()
  return_obj$time_series<- time_series
  return_obj$A_mar<- A_mar 
  
  
  return(return_obj) 
}



