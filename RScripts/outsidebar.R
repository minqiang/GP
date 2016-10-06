outsidebar <- function(ticker, frequency) {
  num_bars = 0
  volume_factor <- 1.10
  
  if (frequency == 'weekly')
    filename <- paste("./weekly_data/", ticker, "_weekly.csv", sep="")
  else if (frequency == 'daily')
    filename <- paste("./daily_data/", ticker, "_daily.csv", sep="")
  else
    stop()
  
  
  data <- read.csv(filename)
  data <- data[order(data$Date),]
  
  for (i in (nrow(data)-1):(nrow(data)-1)) {
    cond1 <- data[i,"High"] > data[i-1,"High"]
    cond2 <- data[i,"Low"] < data[i-1,"Low"]
    cond3 <- data[i,"Close"] > data[i,"Open"]
    volume <-  as.numeric(data[i,"Volume"])
    prev_volume <- as.numeric(data[i-1,"Volume"])
    cond4 <- volume > prev_volume * volume_factor
    
    is_outside_bar <- cond1 && cond2 && cond3 && cond4
    if ( is_outside_bar ) {
      num_bars <- num_bars + 1
    }
    
  }
  
  if ( is_outside_bar ) {
    cat(sprintf("%s \t %d \t %f \t %d\n", ticker, num_bars, num_bars/nrow(data), is_outside_bar))
  }
}

spx500list <- read.csv("spx500list.csv")
frequency <- "weekly"
for (i in 1:nrow(spx500list)) {
  ticker <- as.character(spx500list[i,"ticker"])
  outsidebar(ticker, frequency)
}