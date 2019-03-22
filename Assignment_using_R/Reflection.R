library(stringr)
library(lubridate)
library(scales)

#importing history data into R and giving appropiate column names
dfHistory = read.delim("history_database",sep = ":", stringsAsFactors = F,header = F)
colnames(dfHistory) = c("Time","Command")


dfHistory$Time <- strtrim(dfHistory$Time,10)
dfHistory$Time <- as.numeric(dfHistory$Time)
dfHistory$Time <- as_datetime(dfHistory$Time)

#searching for the following commands
dfHistory$CommandName <- ifelse(grepl("write", dfHistory$Command, ignore.case = T),"Write", 
                                ifelse(grepl("read", dfHistory$Command, ignore.case = T),"Read",
                                       ifelse(grepl("ggplot", dfHistory$Command, ignore.case = T),"ggplot",
                                              ifelse(grepl("qplot", dfHistory$Command, ignore.case = T),"qplot",
                                                     NA ))))

#comparison read vs write
history.df1 <- filter(dfHistory,!is.na(CommandName),CommandName == c("Write","Read"))
ggplot(data = history.df1, aes(x=CommandName)) + geom_bar(fill = "seagreen3") + labs(title = "Read vs Write", x = "Command Name", y = "Count")


#comparison ggplot vs qplot
history.df2 <- filter(dfHistory,!is.na(CommandName),CommandName==c("ggplot","qplot"))
ggplot(data = history.df2, aes(x = CommandName)) + geom_bar(fill = "seagreen3") + labs(title = "ggplot vs qplot",x = "Command Name", y = "Count")


dfHistory$Date <- as.Date(dfHistory$Time,"%d/%m")
dfHistory$Time <- format(dfHistory$Time,"%H:%M:%S")

dfHistory <- subset(dfHistory,Date != "1970-01-01")


Count <- rle( sort( dfHistory$Time ) )

dfHistory$Count <- Count[[1]][ match( dfHistory$Time , Count[[2]] ) ]
dfHistory$Time <- format(dfHistory$Time, format = "%H:%M:%S")
dfHistory$Time <- as.POSIXct(dfHistory$Time, format = "%H:%M:%S")

#plotting for frequency of commands used over time
ggplot(data = dfHistory, aes(dfHistory$Time, y = dfHistory$Count)) + geom_smooth() + scale_x_datetime(labels = date_format("%H-%M")) + labs(x = "Time",y = "Count")

