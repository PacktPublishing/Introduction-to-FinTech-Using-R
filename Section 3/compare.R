# Compare

# Experiment
tmp1 <- YinsCapital::QuantGrowthStrategy()
tmp2 <- YinsCapital::QuantLMDrivenStrategy()

tmp1$Visualization
tmp2$Visualization

# Save
save.image("C:/Users/eagle/OneDrive/YINS CAPITAL, LLC/15. YouTube Channel/Videos/Yin's Q Branch/2019-20-23/global.RData")

# Future
load("C:/Users/eagle/OneDrive/YINS CAPITAL, LLC/15. YouTube Channel/Videos/Yin's Q Branch/2019-20-23/data/global.RData")

library(dygraphs)
tmp1$Visualization
tmp2$Visualization
