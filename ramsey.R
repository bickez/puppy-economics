library(tseries)

daily.prices <- get.hist.quote('^GSPC', quote = 'AdjClose', start = '1950-1-2')
plot(daily.prices, ylab = 'Price', xlab = 'Year', main = 'S&P 500 Daily Prices')

daily.returns <- diff(log(daily.prices))
plot(daily.returns, ylab = 'Return', xlab = 'Year', main = 'S&P 500 Daily Returns')

mu <- mean(daily.returns)
mu
mu * 252 # annualized average return
sigma <- sd(daily.returns)
sigma
sigma * sqrt(252) # annualized volatility

x <- seq(-0.45, 0.5, length = 100)
y <- dnorm(x, mu*252, sigma*sqrt(252))
plot(x, y, type = 'l', xlab = 'Annual Return', ylab = 'Density', 
     main = 'Normal Approx. to S&P 500 Annual Returns')
abline(v = mu*252, lty = 2, col = 'darkgoldenrod')

mean(diff(log(daily.prices), lag = 251)) # rolling annual returns (similar answer to mu*252)


