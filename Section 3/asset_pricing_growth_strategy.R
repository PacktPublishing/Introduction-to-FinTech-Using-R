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
  "AAPL", "FB", "NVDA", "GOOGL", "AMZN", "MSFT", 
  # Financials
  "BAC", "GS", "JPM", "MS", "MA", "V", "PYPL",
  # Industrials
  "LMT", "RTN", "BA", "CAT",
  # Consumers
  "WMT", "M", "TGT")

# Initialize: an empty space for data
myList <- list()
myList <- lapply(symbols, function(x) {getSymbols(x, auto.assign = FALSE)})
names(myList) <- symbols

# Return Data
pastNforGrowth <- 10
returnData <- cbind()
lastPeriodReturnData <- cbind()
for (i in 1:length(symbols)) {
  returnData <- cbind(
    returnData, 
    quantmod::dailyReturn(as.xts(data.frame(myList[i]))))
  lastPeriodReturnData <- cbind(
    lastPeriodReturnData,
    quantmod::dailyReturn(as.xts(data.frame(myList[i])))/lag(
      quantmod::dailyReturn(as.xts(data.frame(myList[i]))), pastNforGrowth) - 1 )
} # end of loop
returnData <- data.frame(na.omit(returnData)); names(returnData) <- symbols
lastPeriodReturnData <- data.frame(na.omit(lastPeriodReturnData)); names(lastPeriodReturnData) <- symbols

# Check
n1 <- nrow(returnData); n2 <- nrow(lastPeriodReturnData)
if (n1 > n2) {returnData <- returnData[-c(1:(n1-n2)), ]}
dim(returnData); dim(lastPeriodReturnData)

# Create Growth Strategy
# Dynamic Programming: 
#      start with a for loop and implement policy to concatenate different values
topGrowthNum <- 4
growthStrategy <- c()
selectedStockIndex <- c()
listNom <- rbind()
for (i in 1:nrow(returnData)) {
  if (i < pastNforGrowth) {
    growthStrategy <- c(growthStrategy, mean(as.numeric(as.character(returnData[i, ]))))
  } else if (i %% pastNforGrowth == 0) {
    Nom <- names(sort(lastPeriodReturnData[i, ], decreasing = TRUE)[1:topGrowthNum])
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
summary(lm(Strategy~MKT, newData))

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
    "Proposed: Growth Strategy SR=", round(mean(newData[, 1])/sd(newData[, 1]), 4),
    " vs. Benchmark: Market Index Fund SR=", round(mean(newData[, 2])/sd(newData[, 2]), 4)
  )) %>%
  dyRebase(value = 1) %>%
  dyLegend(show = "follow")

# Update Holdings
for (k in 1:length(dateListNom)) {
  dyPlot <- dyPlot %>% 
    dyEvent(dateListNom[k], paste0(listNom[k, ], collapse = "_"), 
            labelLoc = "bottom",
            strokePattern = "dotted") }

# Final Visualization
dyPlot
