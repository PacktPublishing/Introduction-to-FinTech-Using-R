library(quantmod)
getSymbols("AAPL")
price <- AAPL[, 4]
returnDataDaily <- quantmod::dailyReturn(price)
returnDataWeekly <- quantmod::weeklyReturn(price)
returnDataMonthly <- quantmod::monthlyReturn(price)

# Report
DT::datatable(data.frame(
  rbind(
    DailyData = round(c("Ave" = mean(returnDataDaily), "SD" = sd(returnDataDaily)), 3),
    WeeklyData = round(c(mean(returnDataWeekly), sd(returnDataWeekly)), 3),
    MonthlyData = round(c(mean(returnDataMonthly), sd(returnDataMonthly)), 3)
  ) ))

# Compare
past_N_days <- 10
mean(tail(returnDataDaily, past_N_days))
sd(tail(returnDataDaily, past_N_days))
