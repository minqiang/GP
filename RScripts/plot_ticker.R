plot_ticker <- function(ticker, years=1) {
  downloadtickeryears(ticker, years)
  filename <- paste("./daily_data/", ticker, "_daily.csv", sep="")
  data <- read.csv(filename)
  data$Date <- as.Date(data$Date)
  plot(data$Date, data$Adj.Close)
  data
  
}

