# Empirical Dynamic Modeling

if (!require("rEDM")) install.packages("rEDM")
library(rEDM)



# Nearest Neighbor Forecasting using Simplex Projection ----
str(TentMap)
x<- rEDM::TentMap
plot(x,type="l")


simplex_out <- Simplex(dataFrame = TentMap, lib = "1 100", 
                       pred = "201 500", columns = "TentMap",
                       target = "TentMap", E = 3)
simplex_out[c(1:2, 300:301), ]


ComputeError(simplex_out$Observations, simplex_out$Predictions)


rho_E <- EmbedDimension(dataFrame = TentMap, lib = "1 100", 
                        pred = "201 500", columns = "TentMap",
                        target = "TentMap")


rho_Tp <- PredictInterval(dataFrame = TentMap, lib = "1 100", 
                          pred = "201 500", target = "TentMap",
                          columns = "TentMap", E = 2)

rho_theta <- PredictNonlinear(dataFrame = TentMapNoise, 
                              lib = "1 100", pred = "201 500",
                              target = "TentMap", columns = "TentMap",
                              E = 2)


tentMapPredict <- Simplex(dataFrame = TentMap, lib = "1 100", 
                          pred = "201 500", target = "TentMap",
                          columns = "TentMap", E = 2)
ComputeError(tentMapPredict$Observations, tentMapPredict$Predictions)$rho

smap = SMap(dataFrame = TentMapNoise, lib = "1 100", 
            pred = "201 500", target = "TentMap",
            columns = "TentMap", E = 2, theta = 3)


head(cbind(smap$predictions, smap$coefficients), 2)
tail(cbind(smap$predictions, smap$coefficients), 2)

# Generalized Takens Theorem ----

head(block_3sp, 3)
plot(block_3sp$x_t,type="l")
lines(block_3sp$y_t,type="l",col="red")
lines(block_3sp$z_t,type="l",col="blue")

smplx_3species = Simplex(dataFrame = block_3sp, 
                         lib = "1 100", pred = "101 190",
                         E = 3, columns = "x_t x_t-1 z_t", 
                         target = "x_t", embedded = TRUE)

err = ComputeError(smplx_3species$Observations, 
                   smplx_3species$Predictions)
plot(smplx_3species$Observations, smplx_3species$Predictions, 
     pch = 19, cex = 0.5, xlab = "Observations",
     ylab = "Predictions", main = "3 Species x_t")
abline(a = 0, b = 1, lty = 2, col = "blue")
text(-1, 1, paste(capture.output(cbind(err)), collapse = "\n"))


# S-map Coefficients ----

smap_Lorenz <- SMap(dataFrame = Lorenz5D, lib = "1 500", 
                    pred = "601 900", E = 4,
                    theta = 3, columns = "V1 V2 V3 V4", 
                    target = "V1", embedded = TRUE)


head(cbind(smap_Lorenz$predictions, smap_Lorenz$coefficients[, 2:6]), 3)

predictions = smap_Lorenz$predictions
coefficients = smap_Lorenz$coefficients
colnames(coefficients)<- c("Time","C0","C1","C2","C3","C4")
Time = predictions$Time

par(mfrow=c(2,2))
plot(Time, predictions$Observations, type = "l", col = "blue", ylab = "V1", xlab = "",
     lwd = 2, cex.lab = 1.3, cex.axis = 1.3)
lines(Time, predictions$Predictions, lwd = 2, col = "red")
legend("topright", legend = c("observed", "predicted"), fill = c("blue", "red"),
       bty = "n", cex = 1.3)
plot(Time, coefficients$C4, type = "l", col = "brown", ylab = "dV4/dV1", xlab = "",
     lwd = 2, cex.lab = 1.3, cex.axis = 1.3)
plot(Time, coefficients$C3, type = "l", col = "darkgreen", ylab = "dV3/dV1", xlab = "",
     lwd = 2, cex.lab = 1.3, cex.axis = 1.3)
plot(Time, coefficients$C2, type = "l", col = "blue", ylab = "dV2/dV1", xlab = "",
     lwd = 2, cex.lab = 1.3, cex.axis = 1.3)

par(mfrow=c(1,1))

# Multiview Embedding ---- 

Mview = Multiview(dataFrame = block_3sp, lib = "1 100", 
                  pred = "101 190", E = 3,
                  columns = "x_t y_t z_t", target = "x_t")

Mview$View[which(Mview$View$rho > 0.91), ]



# Convergent Cross Mapping ---- 

str(sardine_anchovy_sst)

cmap <- CCM(dataFrame = sardine_anchovy_sst, E = 3, 
            Tp = 0, columns = "anchovy",
            target = "np_sst", libSizes = "10 70 5", 
            sample = 100, showPlot = TRUE)

str(cmap)


# Example Apple-BLossom Thrips ----

head(Thrips, 2)

par(mfrow=c(2,2))
plot(Thrips$Thrips_imaginis,type="l",col="dark green")
plot(Thrips$maxT_degC,type="l",col="dark red")
plot(Thrips$Rain_mm,type="l",col="dark blue")
plot(Thrips$Season,type="l",col="purple")
par(mfrow=c(1,1))


rho_E <- EmbedDimension(dataFrame = Thrips, 
                        columns = "Thrips_imaginis", 
                        target = "Thrips_imaginis",
                        lib = "1 72", pred = "1 72", 
                        showPlot = TRUE)


E = 8
rho_theta_e3 = PredictNonlinear(dataFrame = Thrips, 
                                columns = "Thrips_imaginis",
                                target = "Thrips_imaginis", 
                                lib = "1 73", pred = "1 73", E = E)


vars = colnames(Thrips[3:6])
var_pairs = combn(vars, 2) # Combinations of vars, 2 at a time
libSize = paste(NROW(Thrips) - E, NROW(Thrips) - E, 10, collapse = " ")
ccm_matrix = array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars,
                                                                            vars))
for (i in 1:ncol(var_pairs)) {
  ccm_out = CCM(dataFrame = Thrips, columns = var_pairs[1, i], target = var_pairs[2,
                                                                                  i], libSizes = libSize, Tp = 0, E = E, sample = 100)
  outVars = names(ccm_out)
  var_out = unlist(strsplit(outVars[2], ":"))
  ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 2]
  var_out = unlist(strsplit(outVars[3], ":"))
  ccm_matrix[var_out[2], var_out[1]] = ccm_out[1, 3]
}

corr_matrix <- array(NA, dim = c(length(vars), length(vars)), dimnames = list(vars,
                                                                              vars))
for (ccm_from in vars) {
  for (ccm_to in vars[vars != ccm_from]) {
    ccf_out <- ccf(Thrips[, ccm_from], Thrips[, ccm_to], type = "correlation",
                   lag.max = 6, plot = FALSE)$acf
    corr_matrix[ccm_from, ccm_to] <- max(abs(ccf_out))
  }
}

#CCM matrix, which shows the estimated causal relations 
ccm_matrix
corr_matrix


par(mfrow=c(1,3))
thrips_xmap_maxT <- CCM(dataFrame = Thrips, E = E, Tp = 0, columns = "Thrips_imaginis",
                        target = "maxT_degC", libSizes = "13 73 3", sample = 300, showPlot = TRUE)
abline(h = corr_matrix["Thrips_imaginis", "maxT_degC"], col = "black", lty = 2)
thrips_xmap_maxT <- CCM(dataFrame = Thrips, E = E, Tp = 0, columns = "Thrips_imaginis",
                        target = "Rain_mm", libSizes = "13 73 3", sample = 300, showPlot = TRUE)
abline(h = corr_matrix["Thrips_imaginis", "Rain_mm"], col = "black", lty = 2)
thrips_xmap_maxT <- CCM(dataFrame = Thrips, E = E, Tp = 0, columns = "Thrips_imaginis",
                        target = "Season", libSizes = "13 73 3", sample = 300, showPlot = TRUE)
abline(h = corr_matrix["Thrips_imaginis", "Season"], col = "black", lty = 2)


# Create matrix with temperature and rain surrogates (1000 time series vectors)
surr_maxT = SurrogateData(Thrips$maxT_degC, method = "seasonal", T_period = 12, num_surr = 1000,
                          alpha = 3)
surr_rain = SurrogateData(Thrips$Rain_mm, method = "seasonal", T_period = 12, num_surr = 1000,
                          alpha = 3)


# Rain cannot be negative
surr_rain = apply(surr_rain, 2, function(x) {
  i = which(x < 0)
  x[i] = 0
  x
})
# data.frame to hold CCM rho values between Thrips abundance and variable
rho_surr <- data.frame(maxT = numeric(1000), Rain = numeric(1000))
# data.frames with time, Thrips, and 1000 surrogate climate variables for CCM()
maxT_data = as.data.frame(cbind(seq(1:nrow(Thrips)), Thrips$Thrips_imaginis, surr_maxT))
names(maxT_data) = c("time", "Thrips_imaginis", paste("T", as.character(seq(1, 1000)),
                                                      sep = ""))
rain_data = as.data.frame(cbind(seq(1:nrow(Thrips)), Thrips$Thrips_imaginis, surr_rain))
names(rain_data) = c("time", "Thrips_imaginis", paste("R", as.character(seq(1, 1000)),
                                                      sep = ""))
# Cross mapping
for (i in 1:1000) {
  targetCol = paste("T", i, sep = "") # as in maxT_data
  ccm_out = CCM(dataFrame = maxT_data, E = E, Tp = 0, columns = "Thrips_imaginis",
                target = targetCol, libSizes = "73 73 5", sample = 1)
  col = paste("Thrips_imaginis", ":", targetCol, sep = "")
  rho_surr$maxT[i] = ccm_out[1, col]
}
for (i in 1:1000) {
  targetCol = paste("R", i, sep = "") # as in rain_data
  ccm_out = CCM(dataFrame = rain_data, E = E, Tp = 0, columns = "Thrips_imaginis",
                target = targetCol, libSizes = "73 73 5", sample = 1)
  col = paste("Thrips_imaginis", ":", targetCol, sep = "")
  rho_surr$Rain[i] = ccm_out[1, col]
}

1 - ecdf(rho_surr$maxT)(ccm_matrix["maxT_degC", "Thrips_imaginis"])

1 - ecdf(rho_surr$Rain)(ccm_matrix["Rain_mm", "Thrips_imaginis"])

