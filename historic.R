library(tidyverse)
install.packages("Metrics")
library(Metrics)
lib
test <- read.csv("stats_test.csv")

test$actual <- as.numeric(test$actual)
rmse <- rmse(test$actual, test$predicted)
rmse


lm1 <- lm(actual ~ predicted, data = test)
summary(lm1)

graph_test <- ggplot(data = test, aes(x=predicted, y=actual)) + geom_point() + geom_smooth(method="lm")
graph_test

graph_act <- ggplot(data = test, aes(x=Year)) + geom_line(aes(y=predicted, color = "red")) + geom_line(aes(y=actual, color = "blue"))
graph_act

graph_act2 <- ggplot(data = test, aes(x = Year)) +
  geom_line(aes(y = predicted, color = "Predicted")) +
  geom_line(aes(y = actual, color = "Actual")) +
  scale_color_manual(values = c("Predicted" = "blue", "Actual" = "red")) +
  labs(y = "Debt to GDP Ratio", color = "Legend") +
  theme_minimal() + 
  scale_x_continuous(breaks = seq(min(test$Year), max(test$Year), by = 1))

print(graph_act2)
