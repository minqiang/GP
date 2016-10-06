downloadtickeryears <- function(ticker, years) {
  today <- Sys.Date()
  today_day <- format(today, "%d")
  today_month <- as.character( as.numeric(format(today, "%m"))-1 )
  today_year <- format(today, "%Y")
  
  start <- today - 365*years;
  start_day <- format(start, "%d")
  start_month <- as.character( as.numeric(format(start, "%m"))-1 )
  start_year <- format(start, "%Y")
  
  if (!file.exists("daily_data")) {
    dir.create("daily_data")
  }
  
  fileUrl <- paste("http://ichart.yahoo.com/table.csv?s=", ticker, sep="")
  fileUrl <- paste(fileUrl, "&a=", start_month, sep="")
  fileUrl <- paste(fileUrl, "&b=", start_day, sep="")
  fileUrl <- paste(fileUrl, "&c=", start_year, sep="")
  fileUrl <- paste(fileUrl, "&d=", today_month, sep="")
  fileUrl <- paste(fileUrl, "&e=", today_day, sep="")
  fileUrl <- paste(fileUrl, "&f=", today_year, sep="")
  
  # download daily data
  download.file(fileUrl, destfile="./daily_data/temp.csv")
  temp <- read.csv("./daily_data/temp.csv")
  
  filename <- paste("./daily_data/", ticker, "_daily.csv", sep="")
  write.csv(temp, file=filename, row.names=FALSE)
  
  
  file.remove("./daily_data/temp.csv")
}