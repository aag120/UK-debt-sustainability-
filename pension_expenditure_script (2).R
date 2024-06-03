library(tidyverse)
library(ggplot2)
pension <- read.csv("pensions.csv")

pension_lm <- lm(DWP_Expenditure ~ Year, data = pension) 
summary(pension_lm)

graph_test <- ggplot(data = pension, aes(x=Year, y=DWP_Expenditure)) + geom_point() + geom_smooth(method="lm")
graph_test

future_years <- data.frame(Year = 2024:2032)
predictions_pension <- predict(pension_lm, newdata = future_years, interval = "prediction")
#Convert to dataframe
predictions_pension <- as.data.frame(predictions_pension)

#add a column for years
predictions_pension <- predictions_pension %>% mutate(Year = 2024:2032)

graph_pension <- ggplot(data = predictions_pension, aes(x=Year)) + geom_line(aes(y=fit)) +
  geom_line(aes(y = lwr), color = "red") + 
  geom_line(aes(y = upr), color = "green")
graph_pension
summary