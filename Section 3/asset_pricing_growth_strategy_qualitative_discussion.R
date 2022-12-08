# From Last Time
tmp <- YinsCapital::QuantGrowthStrategy(
  # Get Data
  symbols = c(
    # Market
    "SPY",
    # Tech
    "AAPL", "MSFT", "AMZN", "FB",
    # Financials
    "BAC", "GS", "JPM", "V",
    # Industrials
    "LMT", "RTN", "BA", "CAT",
    # Consumers
    "WMT", "TGT", "KO"),
  pastNforGrowth = 15,
  topGrowthNum = 4,
  intializedValue = 1,
  howMuchtoInvest = 1e6 )
tmp$Linear_Model$coefficients
tmp$Visualization
tail(tmp$Name_of_Stock_Held)
tmp$ExeShsEqWeight

# Start Investigation: pastNforGrowth
beginT <- Sys.time()
resultList <- list()
tuningSpace <- seq(10, 20, 5)
for (i in 1:length(tuningSpace)) {
  tmp <- YinsCapital::QuantGrowthStrategy( pastNforGrowth = tuningSpace[i] )
  resultList[i] <- list(tmp)
  print(paste0("Finished with tuning value N=", tuningSpace[i], "."))
} # End Investigation
endT <- Sys.time(); print(endT - beginT)

# Check
names(resultList) <- c(paste0("N=", tuningSpace))
lapply(1:length(tuningSpace), function(x) resultList[[x]]$Linear_Model$coefficients)