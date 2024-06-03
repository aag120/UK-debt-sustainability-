# Install and load necessary packages
install.packages("minpack.lm", type = "binary")
library(minpack.lm)

# Data
years <- c(2015, 2025, 2035)  # Midpoints of the given decades
costs <- c(7.83, 36.5, 185)  # Total costs in billion USD

# Logistic function
logistic <- function(t, L, k, t0) {
  L / (1 + exp(-k * (t - t0)))
}

# Fit the curve using non-linear least squares
start_params <- list(L = 2000, k = 0.2, t0 = 2025)
fit <- nlsLM(costs ~ logistic(years, L, k, t0), start = start_params)

# Extract parameters
params <- coef(fit)
L <- params["L"]
k <- params["k"]
t0 <- params["t0"]

# Predict future costs
future_years <- 2045
future_costs <- logistic(future_years, L, k, t0)
future_costs

# Print the estimated cost for 2040-2049
cat(sprintf("Estimated cost for 2040-2049: $%.2f billion\n", future_costs))

# Plot the data and the fitted curve
plot(years, costs, col = "red", pch = 16, xlim = c(2010, 2050), ylim = c(0,1000), xlab = "Year", ylab = "Cost (billion GBP)", main = "Cost Estimation")
curve(logistic(x, L, k, t0), from = 2010, to = 2050, add = TRUE, col = "blue")
points(future_years, future_costs, col = "blue", pch = 16)
legend("topleft", legend = c("Obtained Data", "Fitted logistic curve", "2040-2049 estimate"),
       col = c("red", "blue", "blue"), pch = c(16, NA, 16), lty = c(NA, 1, NA))

