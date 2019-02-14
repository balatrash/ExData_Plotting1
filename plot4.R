plot4 <- function(){

	##Get the desired data only for better performance and making sure
	##memory is enough by using fread() from the data.table package

	library(data.table)
	file = "household_power_consumption.txt"
	
	##get the skip and nrows using the following logic
	##skip = 1st occurance of 1/2/2007 since we are the skipping the header as well
	##nrows = number of occrances of 1/2/2007 and 2/2/2007
	
	datesdf <- fread(file, header = TRUE, select = 1)
	dates <- datesdf[[1]]
	toskip <- which(dates == "1/2/2007")[1]
	nbrows <- length(dates[dates == "1/2/2007" | dates == "2/2/2007"])

	##read all the columns with the selected rows and set the column names

	data <- fread(file, header = FALSE, skip = toskip, nrows = nbrows)
	headers <- fread(file, header = FALSE, nrows = 1)
	colnames(data) <- as.character(headers[1,])
	
	##format strings to match a date when using as.Date()
	data$Date <- gsub("1/","01/",data$Date)
	data$Date <- gsub("2/","02/",data$Date)
	dt <- paste(data$Date, data$Time, sep = " ")

	dt <- strptime(dt, "%d/%m/%Y %H:%M:%S")
	data <- cbind(data,dt)

	##plot in png file of a 2x2 matrix of graphs and close device
	png("plot4.png",width = 480, height = 480)
	par(mfcol = c(2,2))

	plot(data$Global_active_power ~ data$dt,type="l",ylab = "Global Active Power", xlab = "")

	plot(data$Sub_metering_1~data$dt,type = "l",col = "black",
		ylab = "Energy sub metering" , xlab = "")
	lines(data$Sub_metering_2~data$dt,type = "l",col = "red")
	lines(data$Sub_metering_3~data$dt,type = "l",col = "blue")
	legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
		col = c("black","red","blue"), lty = 1)	

	plot(data$Voltage ~ data$dt,type="l",ylab = "Voltage", xlab = "datetime")
	
	plot(data$Global_reactive_power ~ data$dt,type="l",ylab = "Global_reactive_power", xlab = "datetime")


	dev.off()

}