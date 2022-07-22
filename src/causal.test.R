causal.test <- function(x,p=1,method= "gc.pairwise"){
  
  n_series<- length(x[1,])
  len<- length(x[,1])
  
  if((1+p)> len){
    stop("invalid choice of p, p hast to be smaller then amount of time points")
  }
  
  if(is.null(colnames(x))) { 
    colnames(x)<- paste("Var.",1:n_series,sep="")
  }else{ 
    colnames(x)<- gsub(" ","",colnames(x))
  }
  
  cause_mat<- switch(method,
     gc.pairwise = gc.pairwise(x,p,n_series,len),
     gc.conditional = gc.contiditonal(x,p,n_series,len),
     ccm = gc.ccm(x,p,n_series,len),
     stop(paste(method, "is not a valid method"))
    )

  
  return(cause_mat)
}


#performs pairwise gc test of 
gc.pairwise<- function(x,p,n_series,len){
  
  cause_mat <- matrix(0, nrow=n_series, ncol= n_series)
  
  for(i in 1:n_series){
    for(j in 1:n_series){
      if (i!=j){
        pairwise<- lmtest::grangertest(x[,i],x[,j],order=p)
        qcause_mat[i,j]<- pairwise$`Pr(>F)`[2]
      }          
    }
  }
  
  
  return (cause_mat)
}

gc.conditional <- function(x,p,n_series,len){
  var_model<- vars::VAR(x,type = "none",p= p)
  gc<- vars::causality(var_model,cause=colnames(x)[2])
  gc
  
  
  return (cause_mat)  
}


gc.ccm<- function (x,p,n_series,len){
  
  x<- data.frame(c(1:len),x)
  
  
  vars = colnames(x[-1])
  var_pairs = combn(vars, 2) 
  libSize = paste(nrow(x) - E, nrow(x) - E, 10, collapse = " ")
  ccm_matrix = array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars,
                                                                              vars))
  for (i in 1:ncol(var_pairs)) {
    ccm_out = CCM(dataFrame = x, columns = var_pairs[1, i], target = var_pairs[2,
                                                                                    i], libSizes = libSize, Tp = 0, E = E, sample = 100)
    outVars = names(ccm_out)
    var_out = unlist(strsplit(outVars[2], ":"))
    ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 2]
    var_out = unlist(strsplit(outVars[3], ":"))
    ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 3]
  }
  
  
  cause_mat<- ccm_matrix
  
  
  return(cause_mat)
}

  