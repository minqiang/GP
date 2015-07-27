
downloadData <- function(ticker) {
  today <- Sys.Date()
  today_day <- format(today, "%d")
  today_month <- as.character( as.numeric(format(today, "%m"))-1 )
  today_year <- format(today, "%Y")
  
  start <- today - 31;
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
  if (file.exists(filename)) {
    prev <- read.csv(filename)
    combined <- merge(temp, prev, all=TRUE)
    write.csv(combined, file=filename, row.names=FALSE)
  }
  else {
    write.csv(temp, file=filename, row.names=FALSE)
  }
  
  # download weekly data
  if (!file.exists("weekly_data")) {
    dir.create("weekly_data")
  }
  
  fileUrl <- paste(fileUrl, "&g=w", sep="")
  download.file(fileUrl, destfile="./weekly_data/temp.csv")
  temp <- read.csv("./weekly_data/temp.csv")
  filename <- paste("./weekly_data/", ticker, "_weekly.csv", sep="")
  if (file.exists(filename)) {
    prev <- read.csv(filename)
    combined <- merge(temp, prev, all=TRUE)
    write.csv(combined, file=filename, row.names=FALSE)
  }
  else {
    write.csv(temp, file=filename, row.names=FALSE)
  }
  
  file.remove("./daily_data/temp.csv")
  file.remove("./weekly_data/temp.csv")
}

spx500list <- read.csv("spx500list.csv")
for (i in 1:nrow(spx500list))  {
  ticker <- as.character(spx500list[i,1])
  downloadData(ticker)
}
  


