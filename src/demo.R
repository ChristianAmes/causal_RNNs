# demonstration R script 


# simulate time series data based on random multivariate autoregression  
x<- sim.mar(n_series = 2, mar_p = 1,density_mar = 0.5,sd_mar = 2,
            sd_noise=0.3,sd_initial = 1)


# pairwise Granger causality based on lm
causal.test(x = x$time_series ,p=1, method= "gc.pairwise")
