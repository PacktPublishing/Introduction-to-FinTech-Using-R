# 2019-10-14

# Def Program
YinsTimer <- function(
  target = AAPL,
  buyCoef = -2,
  sellCoef = +2,
  height = 30,
  recent_n_days = 200
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
  #p2 <- matplot(buysellTable, type = "l", xlab = "Time Stamp", ylab = "Value + Signal", main = "Path with Buy/Sell Signals")
  
  # GGplot
  #library(ggplot2)
  #library(reshape2)
  #ts_buysellTable <- data.frame(cbind(rownames(data.frame(buysellTable)), data.frame(buysellTable)))
  #colnames(ts_buysellTable) <- c("Date", "TargetClosingPrice", "BuySignal", "SellSignal")
  #melt_ts_buysellTable <- melt(ts_buysellTable, id = "Date")
  #p2 <- ggplot(melt_ts_buysellTable, aes(x = Date, y = value, colour = variable, group = variable)) + geom_line()
  
  # Dygraph
  p2 <- dygraphs::dygraph(tail(buysellTable, recent_n_days))
  
  # Signal Statistics
  signalStatistics <- DT::datatable(data.frame(
    Ave = round(apply(buysellTable[, -1], 2, function(i) mean(as.numeric(i > 0))), 4),
    SD = round(apply(buysellTable[, -1], 2, function(i) sd(as.numeric(i > 0))), 4)
  ))
  
  return(list(
    Histogram = HIST,
    DistanceData = p1,
    buyData = buyData,
    sellData = sellData,
    buysellTable = buysellTable,
    Statistics = signalStatistics,
    FinalPlot = p2
  ))
} # End of program



















# Library
library(quantmod)

# Index Fund
getSymbols("SPY")
tmp <- YinsTimer(SPY, -2.5, +1.8, 50, 250*3)
tail(tmp$buysellTable)
tmp$Statistics
tmp$FinalPlot





# Large Cap Growth Stock
getSymbols("AAPL")
tmp <- YinsTimer(AAPL, -3.5, +3, 50, 250*3)
tail(tmp$buysellTable)
tmp$Statistics
tmp$FinalPlot






# Small Cap Growth Stock
getSymbols("AMD")
tmp <- YinsTimer(AMD, -2, +2.5, 3, 250*3)
tail(tmp$buysellTable)
tmp$Statistics
tmp$FinalPlot





# IPOs
getSymbols("UBER")
tmp <- YinsTimer(UBER, -2, +2.5, 3, 250*3)
tail(tmp$buysellTable)
tmp$Statistics
tmp$FinalPlot