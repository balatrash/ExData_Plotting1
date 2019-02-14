plot1 <- function(){

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
	
	##plot histogram on a png

	png("plot1.png", width = 480, height = 480)
	hist(data$Global_active_power, col = "red", main = "Global Active Power", 
			xlab = "Global Active Power (kilowatts)")
	dev.off()
	
}