# ASSET PRICING, GROWTH STRATEGY
# Carhart (1995)
# Source: https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1540-6261.1997.tb03808.x
# Original idea @ Carhart (1995) PhD Dissertation

# Package
library(quantmod)

# Get Data
symbols <- c(
  # Market
  "SPY",
  # Tech
  "AAPL", "FB", # "NVDA", "GOOGL", "AMZN", "MSFT", 
  # Financials
  "BAC", "GS"#, #"JPM", "MS", "MA", "V", "PYPL",
  # Industrials
  #"LMT", "RTN", #"BA", "CAT",
  # Consumers
  #"WMT", "M", "TGT"
  )

# Initialize: an empty space for data
myList <- list()
myList <- lapply(symbols, function(x) {getSymbols(x, auto.assign = FALSE)})
names(myList) <- symbols

# Return Data
pastNforGrowth <- 20
verbatim <- TRUE
returnData <- cbind()
forwardLookingReturnData <- cbind()
for (i in 1:length(symbols)) {
  returnData <- cbind(
    returnData, 
    quantmod::dailyReturn(as.xts(data.frame(myList[i]))))
  # Create Data for Prediction Problem
  # X[t], X[t-1], ...
  data <- cbind(
    quantmod::dailyReturn(as.xts(data.frame(myList[i]))),
    lag(quantmod::dailyReturn(as.xts(data.frame(myList[i]))), 1) )
  for (j in 2:pastNforGrowth) {
    data <- cbind(data, cbind(lag(quantmod::dailyReturn(as.xts(data.frame(myList[i]))), j))) }
  # Clean Up
  data <- data.frame(na.omit(data))
  X <- data[, -1]; Y <- data[, 1]

  
  # Create a Performance Data Using Polynomial Regression
  probDailyVector <- c()
  for (k in pastNforGrowth:nrow(data)) {
    new_X <- X[1:k, ]
    new_Y <- Y[1:k] # regression problem (what we are doing)
    new_Y_discrete <- as.numeric(Y[1:k] > mean(Y[1:k])) # classification (2-class)
    tmpLM <- YinsLibrary::Linear_Regression_Predictor(
      x = cbind(new_X, new_X^2, new_X^3), y = new_Y, cutoff = 0.5)
    probDailyVector <- c(probDailyVector, mean(c(tail(tmpLM$Test$y, 1))))
  }
  forwardLookingReturnData <- cbind(forwardLookingReturnData, probDailyVector)

  if (verbatim) {print(paste0("Finished with the ", i, "th symbol in the list."))}
} # end of loop
returnData <- data.frame(na.omit(returnData)); names(returnData) <- symbols
forwardLookingReturnData <- data.frame(na.omit(forwardLookingReturnData)); names(forwardLookingReturnData) <- symbols

# Check
n1 <- nrow(returnData); n2 <- nrow(forwardLookingReturnData)
if (n1 < n2) {
  forwardLookingReturnData <- forwardLookingReturnData[-c(1:(n2 - n1)), ]
} else {
  returnData <- returnData[-c(1:(n1 - n2)), ]
}; dim(returnData); dim(forwardLookingReturnData)

# Create AI-Driven Strategy
# Dynamic Programming: 
#      start with a for loop and implement policy to concatenate different values
#      O(n) space | O(n) time
topGrowthNum <- 4
growthStrategy <- c()
selectedStockIndex <- c()
listNom <- rbind()
for (i in 1:nrow(returnData)) {
  if (i < pastNforGrowth) {
    growthStrategy <- c(growthStrategy, mean(as.numeric(as.character(returnData[i, ]))))
  } else if (i %% pastNforGrowth == 0) {
    Nom <- names(sort(forwardLookingReturnData[i, ], decreasing = TRUE)[1:topGrowthNum])
    listNom <- rbind(listNom, Nom)
    selectedStockIndex <- sort(sapply(1:length(Nom), function(j) {which(symbols == Nom[j])}))
    growthStrategy <- c(
      growthStrategy,
      mean(as.numeric(as.character(returnData[i, selectedStockIndex]))))
  } else {
    growthStrategy <- c(
      growthStrategy,
      mean(as.numeric(as.character(returnData[i, selectedStockIndex]))))
  }
} # end of loop

# OLS Model
newData <- data.frame(Strategy = growthStrategy, MKT = returnData$SPY)
marketModel <- lm(Strategy~MKT, newData)
summary(marketModel)

# List of Stocks Held
dateListNom <- c()
for (i in 1:nrow(returnData)) {
  if (i %% pastNforGrowth == 0) {
    dateListNom <- c(dateListNom, rownames(returnData)[i])
  }
}
rownames(listNom) <- dateListNom

# Performance Plot by Converting Returns back to Values
library(dygraphs)
proposedStrategyPath <- cumprod(1 + growthStrategy)
marketPath <- cumprod(1 + returnData$SPY)
pathData <- data.frame(Strategy = proposedStrategyPath, Market = marketPath)
rownames(pathData) <- rownames(returnData)
dyPlot <- dygraph(
  pathData,
  main = paste0(
    "Proposed: AI-Driven Strategy SR=", round(mean(newData[, 1])/sd(newData[, 1]), 4),
    " vs. Benchmark: Market Index Fund SR=", round(mean(newData[, 2])/sd(newData[, 2]), 4),
    " Alpha = ", round((1 + marketModel$coefficients[1])^(250)-1, 4)
  )) %>%
  dyRebase(value = 1) %>%
  dyLegend(show = "follow")

# Update Holdings
eventNotation <- TRUE
if (eventNotation) {
  for (k in 1:length(dateListNom)) {
    dyPlot <- dyPlot %>% 
      dyEvent(dateListNom[k], paste0(listNom[k, ], collapse = "_"), 
              labelLoc = "bottom",
              strokePattern = "dotted") } }


# Final Visualization
dyPlot

