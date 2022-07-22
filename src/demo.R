# demonstration R script 


# simulate time series data based on random multivariate autoregression  
x<- sim.mar(n_series = 5, mar_p = 2,density_mar = 0.5,sd_mar = 2,
            sd_noise=0.3,sd_initial = 0.2)


# pairwise Granger causality based on lm
causal.test(x = x$time_series ,p=1, method= "gc.pairwise")


# ---- 


x<- sim.mar()
z<- cbind(c(1:100),rep(1,100),x$time_series)
colnames(z)[1:2]<- c("index","key")

z<- tsibble::as_tibble(z)
z<- tsibble::as_tsibble(z,index=index,key=key)

dcmp <- z %>%
  model(STL(Var.1 ~ .))
components(dcmp)
