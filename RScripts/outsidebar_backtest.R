backtest <- function(ticker, frequency) {
  
  num_bars = 0
  volume_factor <- 1.10
  period <- 15
  bardata <- data.frame()
  
  if (frequency == 'weekly')
    filename <- paste("./weekly_data/", ticker, "_weekly.csv", sep="")
  else if (frequency == 'daily')
    filename <- paste("./daily_data/", ticker, "_daily.csv", sep="")
  else
    stop()
  
  
  data <- read.csv(filename)
  data <- data[order(data$Date),]
  
  for (i in 2:nrow(data)) {
    cond1 <- data[i,"High"] > data[i-1,"High"]
    cond2 <- data[i,"Low"] < data[i-1,"Low"]
    cond3 <- data[i,"Close"] > data[i,"Open"]
    volume <-  as.numeric(data[i,"Volume"])
    prev_volume <- as.numeric(data[i-1,"Volume"])
    cond4 <- volume > prev_volume * volume_factor
    
    is_outside_bar <- cond1 && cond2 && cond3 && cond4
    if ( is_outside_bar ) {
       num_bars <- num_bars + 1
       closeprice <- data[i,"Close"]
       endNo <- min(period, nrow(data)-i)
       
       if (endNo>0) {
          for (j in 1:endNo) {
             BarNo <- j
             HighReturn <- (data[i+j,"High"]-closeprice)/closeprice
             CloseReturn <- (data[i+j,"Close"]-closeprice)/closeprice
             LowReturn <- (data[i+j,"Low"]-closeprice)/closeprice
             newrow <- cbind(data[i+j,], BarNo, HighReturn, CloseReturn, LowReturn)
             bardata <- rbind(bardata, newrow)
          }
       }
    } 
  }
  bardata
}
 high_to_close <- aggregate(HighReturn ~ BarNo, data = bardata, FUN=function(x) c(mean =mean(x), 
                                                                           max = max(x),
                                                                           min = min(x),
                                                                           sd = sd(x),
                                                                           Pvalue = t.test(x)$p.value))
                                                                          
                            
                             
 
high_to_close <- formatchange(high_to_close)
 
close_to_close <- aggregate(CloseReturn ~ BarNo, data = bardata, FUN=function(x) c(mean =mean(x), 
                                                                                max = max(x),
                                                                                min = min(x),
                                                                                sd = sd(x),
                                                                                Pvalue = t.test(x)$p.value) )

close_to_close <- formatchange(close_to_close)


low_to_close <- aggregate(LowReturn ~ BarNo, data = bardata, FUN=function(x) c(mean =mean(x), 
                                                                                max = max(x),
                                                                                min = min(x),
                                                                                sd = sd(x),
                                                                                Pvalue = t.test(x)$p.value) )
low_to_close <- formatchange(low_to_close)



formatchange <- function(x) {
  x <- cbind(x[-ncol(x)], x[[ncol(x)]])
}


  
  
  
