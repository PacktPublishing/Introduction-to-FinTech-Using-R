library(quantmod)
Nom <- c("SPY", "GOOGL")
getSymbols(Nom)
assetReturn1 <- YinsCapital::Basic_MM(SPY, 10)$returnDataDaily
assetReturn2 <- YinsCapital::Basic_MM(GOOGL, 10)$returnDataDaily
past_n_time_stamps <- 250
linearModel <- lm(tail(assetReturn2, past_n_time_stamps)~
                    tail(assetReturn1, past_n_time_stamps))
summary(linearModel)

# Two Assets
returnPath <- c()
riskPath <- c()

# Begin: collect data
w1_space <- seq(0, 1, 0.05)
for (w1 in w1_space) {
  w2 <- 1 - w1
  ER1 <- mean(assetReturn1)
  ER2 <- mean(assetReturn2)
  var1 <- sd(assetReturn1)^2
  var2 <- sd(assetReturn2)^2
  portfolioReturn <- w1*ER1 + w2*ER2
  portfolioRisk <- matrix(c(w1, w2), nrow = 1) %*% 
    matrix(c(
      var1,
      var1*var2*var1*var2*cor( tail(assetReturn1, past_n_time_stamps), tail(assetReturn2, past_n_time_stamps)),
      var1*var2*cor( tail(assetReturn1, past_n_time_stamps), tail(assetReturn2, past_n_time_stamps)),
      var2 ), nrow = 2, ncol = 2) %*% 
    matrix(c(w1, w2), ncol = 1)
  returnPath <- c(returnPath, portfolioReturn)
  riskPath <- c(riskPath, portfolioRisk)
} # End of loop

# Optimal Portfolio
w1 <- w1_space[which.max(returnPath/riskPath)]
w2 <- 1 - w1
ER1 <- mean(assetReturn1)
ER2 <- mean(assetReturn2)
var1 <- sd(assetReturn1)^2
var2 <- sd(assetReturn2)^2
portfolioReturn <- w1*ER1 + w2*ER2
portfolioRisk <- matrix(c(w1, w2), nrow = 1) %*% 
  matrix(c(
    var1,
    var1*var2*var1*var2*cor( tail(assetReturn1, past_n_time_stamps), tail(assetReturn2, past_n_time_stamps)),
    var1*var2*cor( tail(assetReturn1, past_n_time_stamps), tail(assetReturn2, past_n_time_stamps)),
    var2 ), nrow = 2, ncol = 2) %*% 
  matrix(c(w1, w2), ncol = 1)

# Plot
plot(riskPath, returnPath, 
     type = "b", col = "blue", lwd = 3,
     main = paste0("Efficient Frontier (Blue) & Capital Market Line (Green)", 
                   "\nOptimal Weight: [", w1, ",", w2, "]")); 
abline(a = 1e-8, b = portfolioReturn/portfolioRisk,
       lty = 3, lwd = 3, col = "green")
