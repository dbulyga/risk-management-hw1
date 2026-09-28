# 1. Встановлення та підключення пакетів
packages <- c("quantmod", "PerformanceAnalytics", "ggplot2")
new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if (length(new_packages) > 0) install.packages(new_packages)

library(quantmod)
library(PerformanceAnalytics)
library(ggplot2)

end_date <- Sys.Date()
start_date <- end_date - (3 * 365)

tickers <- c("AAPL", "^GSPC")

getSymbols(tickers, src = "yahoo", from = start_date, to = end_date)

# 2. Розрахунок логарифмічних дохідностей
prices_aapl <- Ad(AAPL)
prices_sp500 <- Ad(GSPC)

returns_aapl <- na.omit(CalculateReturns(prices_aapl, method = "log"))
returns_sp500 <- na.omit(CalculateReturns(prices_sp500, method = "log"))

# 3. Оцінка метрик ризику (Value at Risk & Expected Shortfall)
var_hist_95_aapl <- VaR(returns_aapl, p = 0.95, method = "historical")
var_hist_99_aapl <- VaR(returns_aapl, p = 0.99, method = "historical")

var_norm_95_aapl <- VaR(returns_aapl, p = 0.95, method = "gaussian")
var_norm_99_aapl <- VaR(returns_aapl, p = 0.99, method = "gaussian")

es_hist_95_aapl <- ES(returns_aapl, p = 0.95, method = "historical")

var_hist_95_sp <- VaR(returns_sp500, p = 0.95, method = "historical")
var_hist_99_sp <- VaR(returns_sp500, p = 0.99, method = "historical")

var_norm_95_sp <- VaR(returns_sp500, p = 0.95, method = "gaussian")
var_norm_99_sp <- VaR(returns_sp500, p = 0.99, method = "gaussian")

es_hist_95_sp <- ES(returns_sp500, p = 0.95, method = "historical")

cat("Historical VaR (95%):", round(var_hist_95_aapl * 100, 2), "%\n")
cat("Historical VaR (99%):", round(var_hist_99_aapl * 100, 2), "%\n")
cat("Parametric VaR (95%):", round(var_norm_95_aapl * 100, 2), "%\n")
cat("Parametric VaR (99%):", round(var_norm_99_aapl * 100, 2), "%\n")
cat("Expected Shortfall (95%):", round(es_hist_95_aapl * 100, 2), "%\n")

cat("\n============== РЕЗУЛЬТАТИ ОЦІНКИ РИЗИКУ (S&P 500) ==============\n")
cat("Historical VaR (95%):", round(var_hist_95_sp * 100, 2), "%\n")
cat("Historical VaR (99%):", round(var_hist_99_sp * 100, 2), "%\n")
cat("Parametric VaR (95%):", round(var_norm_95_sp * 100, 2), "%\n")
cat("Parametric VaR (99%):", round(var_norm_99_sp * 100, 2), "%\n")
cat("Expected Shortfall (95%):", round(es_hist_95_sp * 100, 2), "%\n")


# 4. Візуалізація
png("data/var_chart.png", width = 900, height = 650, res = 100)

chart.Histogram(returns_aapl,
                main = "Розподіл щоденних дохідностей та рівні VaR (AAPL)",
                xlab = "Щоденна логарифмічна дохідність",
                ylab = "Щільність",
                methods = c("add.density", "add.normal"))

abline(v = as.numeric(var_hist_95_aapl), col = "darkorange", lwd = 2.5, lty = 2)
abline(v = as.numeric(var_hist_99_aapl), col = "red", lwd = 2.5, lty = 3)

legend("topleft",
       legend = c("Щільність (Kernel)", "Нормальний розподіл", 
                  paste0("Historical VaR 95% (", round(var_hist_95_aapl * 100, 2), "%)"),
                  paste0("Historical VaR 99% (", round(var_hist_99_aapl * 100, 2), "%)")),
       col = c("black", "blue", "darkorange", "red"),
       lty = c(1, 1, 2, 3),
       lwd = c(1.5, 1.5, 2.5, 2.5),
       bty = "n",
       cex = 0.85)

dev.off()

cat("\n[OK] Графік успішно створено та збережено: data/var_chart.png\n")