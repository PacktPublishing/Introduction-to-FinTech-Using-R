
#### IDEA ####

# Get Data
library(quantmod)
getSymbols("AAPL")
target <- AAPL
hist(quantmod::dailyReturn(target), breaks = 30, xlab = "Value", main = "Histogram of AAPL")

# Create Distance Data
smaData <- cbind(target[, 4])
Nom <- paste0("Lag=", c(10, 20, 50, 150, 200))
for (i in c(10, 20, 50, 150, 200)) {smaData <- cbind(smaData, SMA(target[, 4], n = i))}
smaData <- na.omit(smaData)
distData <- smaData[, 1]
for (i in 2:ncol(smaData)) {distData <- cbind(distData, smaData[, 1] - smaData[, i])}
matplot(distData, type = "l", xlab = "Time Stamp", ylab = "Value", main = "Distance Data for AAPL")
abline(h = max(distData[, 6]), lty = 3, col = "green")
abline(h = min(distData[, 6]), lty = 3, col = "red")

# Buy Signal
buyCoef <- -2
buyData <- distData[, 1]
for (i in 2:ncol(distData)) {buyData <- cbind(buyData, as.numeric(distData[, i] < buyCoef * sd(distData[, i])))}
colnames(buyData) <- c("Close", Nom)

# Sell Signal
sellCoef <- +2
sellData <- distData[, 1]
for (i in 2:ncol(distData)) {sellData <- cbind(sellData, as.numeric(distData[, i] > sellCoef * sd(distData[, i])))}
colnames(sellData) <- c("Close", Nom)

# All Signal
height <- 30
buysellTable <- cbind(distData[, 1], rowMeans(buyData[, -1]), rowMeans(sellData[, -1]))
buysellTable[, -1] <- buysellTable[, -1] * height
matplot(buysellTable, type = "l", xlab = "Time Stamp", ylab = "Value + Signal", main = "Path with Buy/Sell Signals")

#### EXECUTABLE PROGRAM ####

# Def Program
YinsTimer <- function(
  target = AAPL,
  buyCoef = -2,
  sellCoef = +2,
  height = 30
) {
  
  # Data
  HIST <- hist(quantmod::dailyReturn(target), breaks = 30, xlab = "Value", main = "Histogram of AAPL")
  
  # Create Distance Data
  smaData <- cbind(target[, 4])
  Nom <- paste0("Lag=", c(10, 20, 50, 150, 200))
  for (i in c(10, 20, 50, 150, 200)) {smaData <- cbind(smaData, SMA(target[, 4], n = i))}
  smaData <- na.omit(smaData)
  distData <- smaData[, 1]
  for (i in 2:ncol(smaData)) {distData <- cbind(distData, smaData[, 1] - smaData[, i])}
  p1 <- matplot(distData, type = "l", xlab = "Time Stamp", ylab = "Value", main = "Distance Data for AAPL"); 
  abline(h = max(distData[, 6]), lty = 3, col = "green"); abline(h = min(distData[, 6]), lty = 3, col = "red")
  
  # Buy Signal
  #buyCoef <- -2
  buyData <- distData[, 1]
  for (i in 2:ncol(distData)) {buyData <- cbind(buyData, as.numeric(distData[, i] < buyCoef * sd(distData[, i])))}
  colnames(buyData) <- c("Close", Nom)
  
  # Sell Signal
  #sellCoef <- +2
  sellData <- distData[, 1]
  for (i in 2:ncol(distData)) {sellData <- cbind(sellData, as.numeric(distData[, i] > sellCoef * sd(distData[, i])))}
  colnames(sellData) <- c("Close", Nom)
  
  # All Signal
  #height <- 30
  buysellTable <- cbind(distData[, 1], rowMeans(buyData[, -1]), rowMeans(sellData[, -1]))
  buysellTable[, -1] <- buysellTable[, -1] * height
  colnames(buysellTable) <- c("TargetClosingPrice", "BuySignal", "SellSignal")
  p2 <- matplot(buysellTable, type = "l", xlab = "Time Stamp", ylab = "Value + Signal", main = "Path with Buy/Sell Signals")
  
  return(list(
    Histogram = HIST,
    DistanceData = p1,
    buyData = buyData,
    sellData = sellData,
    buysellTable = buysellTable,
    FinalPlot = p2
  ))
} # End of program

# Test Run
library(quantmod)
getSymbols("AAPL")
tmp <- YinsTimer(AAPL, -2, +2, 30)
tail(tmp$buysellTable)
