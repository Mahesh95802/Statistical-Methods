library(utils)
library(stats)
library(graphics)
library(modeest)

X <- as.data.frame(read.csv("https://raw.githubusercontent.com/Maheshwaran-k2019/StatisticalMethods/main/FootballPlayerStats.csv"))
Y <- X[['MinutesPlayed']]

#Mean Function
user_mean <- function(y) {
  mean_y <- sum(y)/length(y)
  return(mean_y)
}
mean_y <- user_mean(Y)
print(paste0("Mean: ",mean_y))

#Median Function
user_median <- function(y) {
  y <- sort(y)
  median_y <- ifelse(length(y)%%2==1,y[(length(y)+1)/2],mean(y[length(y)/2+0:1]))
  return(median_y)
}
print(paste0("Median: ",user_median(Y)))

#Mode Function
user_mode <- function(y) {
  mode_y <- y[which.max(tabulate(match(y,unique(y))))]
  return(mode_y)
}
print(paste0("Mode: ",user_mode(Y)))

#IQR Function
user_iqr <- function(y) {
  iqr_y <- summary(y)["3rd Qu."]-summary(y)["1st Qu."]
  return(iqr_y)
}
print(paste0("IQR: ",user_iqr(Y)))

#Standard Deviation Function
user_sd <- function(y,mean_y) {
  sd_y <- sqrt(sum((y-mean_y)^2)/(length(y)-1))
}
sd_y <- user_sd(Y,mean_y)
print(paste0("SD: ",sd_y))

#Probability Values on Empirical Rule
#values within 1 SD
p1sd <- sum( Y >= mean_y-sd_y & Y <= mean_y+sd_y )/length(Y)
print(paste0("Probability of values inside Mean \u00b1 SD: ",p1sd))
#values within 2 SD
p2sd <- sum( Y >= mean_y-(2*sd_y) & Y <= mean_y+(2*sd_y) )/length(Y)
print(paste0("Probability of values inside Mean \u00b1 2*SD: ",p2sd))
#values within 3 SD
p3sd <- sum( Y >= mean_y-(3*sd_y) & Y <= mean_y+(3*sd_y) )/length(Y)
print(paste0("Probability of values inside Mean \u00b1 3*SD: ",p3sd))

#Histogram and Standard Normal Distribution Curve
hist(Y,probability = T)
curve(dnorm(x,mean = mean_y,sd=sd(Y)),add=T)

#Comparing Functions
print(paste0("User-Defined Mean: ",user_mean(Y),"     Predefined Mean: ",mean(Y)))
print(paste0("User-Defined Meadian: ",user_median(Y),"     Predefined Median: ",median(Y)))
print(paste0("User-Defined Mode: ",user_mode(Y),"     Predefined Mode: ",mlv(Y, method = "mfv")))
print(paste0("User-Defined IQR: ",user_iqr(Y),"     Predefined IQR: ",IQR(Y)))
print(paste0("User-Defined SD: ",user_sd(Y,mean_y),"     Predefined SD: ",sd(Y)))
