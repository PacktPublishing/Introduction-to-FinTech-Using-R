#### BEGIN SCRIPT ####

# Library
library(quantmod)
library(dygraphs)
library(DT)

#### DEFINE FUNCTIONS ####

# Def
# EP: HOW I TIME STOCK MARKET
AdvBuySellAlgorithm <- function(
  target = "AAPL",
  buyCoef = -2,
  sellCoef = +2,
  height = 30,
  movingAvEnv = c(10, 20, 50, 70, 100)
) {
  
  # Data
  myList <- list()
  myList <- lapply(target, function(x) {quantmod::getSymbols(x, auto.assign = FALSE)})
  names(myList) <- target
  target <- myList[[1]]
  HIST <- hist(
    quantmod::dailyReturn(target), breaks = 30,
    xlab = "Value", main = "Histogram of Target Stock")
  
  # Create Distance Data
  smaData <- cbind(target[, 4])
  Nom <- paste0("Lag=", movingAvEnv)
  for (i in movingAvEnv) {smaData <- cbind(smaData, SMA(target[, 4], n = i))}
  smaData <- na.omit(smaData)
  distData <- smaData[, 1]
  for (i in 2:ncol(smaData)) {distData <- cbind(distData, smaData[, 1] - smaData[, i])}
  p1 <- matplot(distData, type = "l",
                xlab = "Time Stamp", ylab = "Value",
                main = "Distance Data for Target Stock"); abline(
                  h = max(distData[, 6]), lty = 3, col = "green"); abline(
                    h = min(distData[, 6]), lty = 3, col = "red")
  
  # Buy Signal
  buyData <- distData[, 1]
  for (i in 2:ncol(distData)) {buyData <- cbind(buyData, as.numeric(distData[, i] < buyCoef * sd(distData[, i])))}
  colnames(buyData) <- c("Close", Nom)
  
  # Sell Signal
  sellData <- distData[, 1]
  for (i in 2:ncol(distData)) {sellData <- cbind(sellData, as.numeric(distData[, i] > sellCoef * sd(distData[, i])))}
  colnames(sellData) <- c("Close", Nom)
  
  # All Signal
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
  
  # Statistics
  signalStatistics <- data.frame(
    Ave = round(apply(buysellTable[, -1], 2, function(i) mean(as.numeric(i > 0))), 4),
    SD = round(apply(buysellTable[, -1], 2, function(i) sd(as.numeric(i > 0))), 4) )
  
  # Dygraph
  p2 <- dygraphs::dygraph(buysellTable)
  
  return(list(
    Histogram = HIST,
    DistancePlot = p1,
    DistanceData = distData,
    buyData = buyData,
    sellData = sellData,
    buysellTable = buysellTable,
    Statistics = signalStatistics,
    FinalPlot = p2
  ))
} # end of function

# Def
# ASSET PRICING GROWTH STRATEGY
QuantGrowthStrategy <- function(
  # Get Data
  symbols = c(
    # Market
    "SPY",
    # Tech
    "AAPL", "FB", "NVDA", "GOOGL", "AMZN", "MSFT", "AMD", "MU", "MCHP",
    # Financials
    "BAC", "GS", "JPM", "MS", "MA", "V", "PYPL",
    # Industrials
    "LMT", "RTN", "BA", "CAT",
    # Consumers
    "WMT", "M", "TGT", "PEP", "DIS", "KO"),
  pastNforGrowth = 20,
  topGrowthNum = 4,
  intializedValue = 1,
  howMuchtoInvest = 1e4
) {
  
  # ASSET PRICING, GROWTH STRATEGY
  # Carhart (1995)
  # Source: https://onlinelibrary.wiley.com/doi/epdf/10.1111/j.1540-6261.1997.tb03808.x
  # Original idea @ Carhart (1995) PhD Dissertation
  
  # Package
  library(quantmod)
  
  # Initialize: an empty space for data
  myList <- list()
  myList <- lapply(symbols, function(x) {getSymbols(x, auto.assign = FALSE)})
  names(myList) <- symbols
  
  # Return Data
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
  linearModel <- lm(Strategy~MKT, newData)
  summary(linearModel)
  
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
    dyRebase(value = intializedValue) %>%
    dyLegend(show = "follow")
  
  # Update Holdings
  for (k in 1:length(dateListNom)) {
    dyPlot <- dyPlot %>%
      dyEvent(dateListNom[k], paste0(listNom[k, ], collapse = "_"),
              labelLoc = "bottom",
              strokePattern = "dotted") }
  
  # Final Visualization
  # dyPlot
  
  # Execution
  eachWeight <- howMuchtoInvest/length(selectedStockIndex)
  sharesVector <- c()
  weightVector <- sapply(selectedStockIndex, function(x) {sharesVector <- c(
    sharesVector, eachWeight/tail(data.frame(myList[x])[, 4], 1))})
  #weightVector
  
  # Output
  #return(list(Stocks = symbols, EqualWeight = weightVector))
  
  # Output
  return(list(
    Original_Stock_Data_List = myList,
    Return_Data = returnData,
    Return_Data_for_Past_Period = lastPeriodReturnData,
    Name_of_Stock_Held = listNom,
    Return_Data_New = newData,
    Path_Data_New = pathData,
    Linear_Model = summary(linearModel),
    Visualization = dyPlot,
    ExeStocks = symbols[c(selectedStockIndex)],
    ExeShsEqWeight = weightVector
  ))
} # enc of function

#### DESIGN APP ####

# Design App
shinyApp(
  # UI
  ui = tagList(
    shinythemes::themeSelector(),
    navbarPage(
      # theme = "cerulean",  # <--- To use a theme, uncomment this
      "YIN'S Q BRANCH",
      fluid = TRUE,
      collapsible = TRUE,
      tabPanel("Navbar 1: TIMING STOCK MARKET",
               sidebarPanel(
                 ## YOUR CODE HERE ##
                 textInput("ticker", "Target Stock:", "AAPL"),
                 sliderInput("coef", "Buy/Sell Coefficients", min = -5, max = +5, value = c(-2, 2), step = 0.2),
                 sliderInput("height", "Height of Signals", min = 1, max = 500, value = 30, step = 10),
                 textInput("movingAvEnv", "Multivariate Moving Average", "10, 20, 50, 70, 100"),
                 submitButton("Submit", width = "100%")
                 ## END YOUR CODE ##
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Yin's Timer",
                            ## YOUR CODE HERE ##
                            h3("Visualization"),
                            helpText("This path presents stock performance along with buy and sell signals (if zoom in, height of signals can be controlled from left)."),
                            dygraphOutput("timing_FinalPlot"),
                            h3("Buy Sell Table Along with Price"),
                            helpText("Price Data (closing price) of target stock and related buy/sell signals."),
                            dataTableOutput("timing_buysellTable"),
                            h3("Statistics from Timing Results"),
                            helpText("The following table summarizes the frequency and variations of buy/sell signals."),
                            dataTableOutput("timing_Statistics"),
                            helpText("Comment: I would recommend to pick stocks with long history and to keep buy signals to be 1-2% and sell signals to be less than 1%.")
                            ## END YOUR CODE ##
                            ),
                   tabPanel("Tab 2", "This panel is intentionally left blank")
                 )
               )
      ),
      tabPanel("Navbar 2: CONSTRUCT PORTFOLIO", 
               sidebarPanel(
                 textInput(
                   "text", "Enter Tickers:",
                   "SPY, AAPL, FB, NVDA, GOOGL, AMZN, MSFT, AMD, MU, MCHP, BAC, GS, JPM, MS, MA, V, PYPL, LMT, RTN, BA, CAT, WMT, M, TGT, PEP, DIS, KO"),
                 helpText("Note: "),
                 helpText("- Enter Stock Tickers (separate by comma and a space, i.e. ', ')"),
                 helpText("- Keep it short. Ex: Default 27 stocks takes about ~ 40 sec."),
                 helpText("- Early morning/late evening sites will be busy. If error appears, try again in a few hours."),
                 sliderInput(inputId = "pastNforGrowth", label = "Evaluation Window:", 
                             value = 20, min = 10, max = 50, step = 5),
                 sliderInput(inputId = "topGrowthNum", label = "Number of Top Stocks", 
                             value = 4, min = 2, max = 30, step = 1),
                 numericInput(inputId = "intializedValue", label = "Initial Value", value = 1),
                 numericInput(inputId = "howMuchtoInvest", label = "Initial Investment", value = 1e6),
                 submitButton("Submit", width = "100%")
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Quant: Growth Strategy",
                            h3("Results from OLS Regression"),
                            helpText("The estimated intercept is alpha. The t value to test whether the estimate is statistically significant."),
                            verbatimTextOutput("linearModel"),
                            h3("Performance"),
                            helpText("Paths for Benchmark and Proposed Portfolio by Growth Strategy starting from initial value (default $1 and can be changed by user)."),
                            dygraphOutput("Visualization"),
                            h3("Execution"),
                            helpText("The holdings and amount of shares the algorithm suggests in portfolio right now according to initial investment (default $1M and can be changed by user)."),
                            dataTableOutput("Execution")
                   ),
                   tabPanel("Tab 2", "This panel is intentionally left blank")
                 )
               )),
      conditionalPanel(
        condition="input.goButton > 0 | $('html').hasClass('shiny-busy')",
        tags$div(
          c("Calculating... Please wait... Patience is the key to success.",
            "Calculating... Please wait... Trees that are slow to grow bear the best fruit.",
            "Calculating... Please wait... He that can have patience can have what he will.",
            "Calculating... Please wait... Patience is bitter, but its fruit is sweet",
            "Calculating... Please wait... The two most powerful warriors are patience and time." )[sample(5,1)] ))
    )
  ),
  # Server
  server = function(input, output) {
    # Timing:
    ## YOUR CODE HERE ##
    timing <- reactive({
      AdvBuySellAlgorithm(
        target = input$ticker,
        buyCoef = input$coef[1],
        sellCoef = input$coef[2],
        height = input$height,
        movingAvEnv = c(as.numeric(as.character(unlist(strsplit(input$movingAvEnv, ", "))))) )})
    output$timing_FinalPlot <- renderDygraph({ timing()$FinalPlot })
    output$timing_buysellTable <- renderDataTable({ data.frame(cbind( timing()$buysellTable)) })
    output$timing_Statistics <- renderDataTable({ data.frame(timing()$Statistics) })
    ## END YOUR CODE ##
    
    # Quant: Growth Strategy
    results <- reactive({QuantGrowthStrategy(
      # Get Data
      symbols = c(unlist(strsplit(input$text, ", "))),
      pastNforGrowth = input$pastNforGrowth,
      topGrowthNum = input$topGrowthNum,
      intializedValue = input$intializedValue,
      howMuchtoInvest = input$howMuchtoInvest )})
    output$linearModel <- renderPrint({ results()$Linear_Model })
    output$Visualization <- renderDygraph({ results()$Visualization })
    output$Execution <- renderDataTable({
      data.frame(
        Stocks = results()$ExeStocks,
        Shares = results()$ExeShsEqWeight ) })
  }
)

#### END SCRIPT ####