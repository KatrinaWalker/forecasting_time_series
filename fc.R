library(ggplot2)
library(tseries)

setwd('/home/viviana/Dropbox/Data Science/02 Financial Econometrics/fc')
data <- read.table('forecast-competition-training.csv',header=TRUE,sep=',')


# ADF test
adf.test(data$TARGET , k = 10) # first you check stationary 
#     reject the null of unit root

# t:t-8 correlations between target and the other variables 
# 8 is lags
corr_t_name <- character(length = 0)
corr_number <- vector(mode="double", length=0)
corr_t_value <- vector(mode="double", length=0)
corr_t_1_value <- vector(mode="double", length=0)
corr_t_2_value <- vector(mode="double", length=0)
corr_t_3_value <- vector(mode="double", length=0)
corr_t_4_value <- vector(mode="double", length=0)
corr_t_5_value <- vector(mode="double", length=0)
corr_t_6_value <- vector(mode="double", length=0)
corr_t_7_value <- vector(mode="double", length=0)
corr_t_8_value <- vector(mode="double", length=0)
for (i in 2:50){
  corr_number[i-1] <- i
  corr_t_name[i-1] <- names(data)[i]
  corr_t_value[i-1] <- cor(data[,1], data[,i])
  corr_t_1_value[i-1] <- cor(data[2:500,1], data[1:499,i])
  corr_t_2_value[i-1] <- cor(data[3:500,1], data[1:498,i])
  corr_t_3_value[i-1] <- cor(data[4:500,1], data[1:497,i])
  corr_t_4_value[i-1] <- cor(data[5:500,1], data[1:496,i])
  corr_t_5_value[i-1] <- cor(data[6:500,1], data[1:495,i])
  corr_t_6_value[i-1] <- cor(data[7:500,1], data[1:494,i])
  corr_t_7_value[i-1] <- cor(data[8:500,1], data[1:493,i])
  corr_t_8_value[i-1] <- cor(data[9:500,1], data[1:492,i])
}
corr <- as.data.frame(cbind(corr_number, corr_t_name,corr_t_value,corr_t_1_value,corr_t_2_value,corr_t_3_value,corr_t_4_value,corr_t_5_value,corr_t_6_value,corr_t_7_value,corr_t_8_value))
corr <- corr[order(corr$corr_t_1_value),]
#   from this we get that the most correlated series are 5, 11, 24, 40 and 4

# ARMA 50 periods ahead
AR <- vector(mode="double", length=0) # an empty vector with double variables
MA <- vector(mode="double", length=0)
X <- character(length = 16)
RMSD <- vector(mode="double", length=0)
target_original <- as.matrix(data[,1])
t <- 1
for (j in 0:3){
  for (k in 0:3){
    target_pred <- as.matrix(data[1:450,1])
    pred <- predict(arima(target_pred, order = c(j,0,k)), n.ahead = 50)$pred
    AR[t] <- j
    MA[t] <- k
    RMSD[t] <- sqrt((t(pred-target_original[451:500]) %*% (pred-target_original[451:500]))/50)
    t <- t+1
  }
}


# ARMAX with one variable t-1 50 periods ahead 
# an arma with lags of other variables
x_var <- c(5, 11, 24, 40, 4, 46, 38)
for (q in 1:7){
  for (j in 0:3){
    for (k in 0:3){
      target_pred <- as.matrix(data[1:450,1])
      pred <- predict(arima(target_pred[2:450], order = c(j,0,k), xreg=data[1:449,x_var[q]]), n.ahead = 50, newxreg = data[450:499, x_var[q]])$pred
      AR[t] <- j
      MA[t] <- k
      X[t] <- paste(x_var[q], "t-1", sep = '_')
      RMSD[t] <- sqrt((t(pred-target_original[451:500]) %*% (pred-target_original[451:500]))/50)
      t <- t+1
    }
  } 
}

# t-2
for (q in 1:7){
  for (j in 0:3){
    for (k in 0:3){
      target_pred <- as.matrix(data[1:450,1])
      pred <- predict(arima(target_pred[3:450], order = c(j,0,k), xreg=cbind(data[2:449,x_var[q]], data[1:448,x_var[q]])), n.ahead = 50, newxreg = cbind(data[450:499, x_var[q]], data[449:498, x_var[q]]))$pred
      AR[t] <- j
      MA[t] <- k
      X[t] <- paste(x_var[q], "t-1", "t-2", sep = '_')
      RMSD[t] <- sqrt((t(pred-target_original[451:500]) %*% (pred-target_original[451:500]))/50)
      t <- t+1
    }
  } 
}
results <- as.data.frame(cbind(AR, MA, X, RMSD))
results <- results[order(RMSD),]
