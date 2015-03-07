
# Generates plots for Global Active Power, Voltage, Energy sub metering and Global reactive power
# from the data of nameFile and dates specified in dates vector 
plot4 <- function(namefile, dates){
    
    # Set language to English
    Sys.setlocale("LC_TIME", "English")
    
    # Call to loadData function that loads the data used to make the plot
    data <- loadData(namefile, dates)
    
    # Define column Time as dateTime
    data[, 2] <- paste(as.character(data[, 1]), substring(data[, 2], 12, 19), sep = " ")
    
    data$Time <- strptime(data$Time, tz = "EST5EDT", format="%Y-%m-%d %H:%M:%S")
    
    # Define the order of plots, size of margins, outer margins and font size
    par(mfrow = c(2, 2), mar = c(5, 5, 2, 2), cex = 0.55)
    
    
    # Create the 4 plots required on screen device
    with(data, {
        plot(Time, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
        
        plot(Time, Voltage, type = "l", xlab = "datetime", ylab = "Voltage")
        
        plot(Time, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
        lines(Time, Sub_metering_2, type = "l", xlab = "", ylab = "Energy sub metering", col = "red")
        lines(Time, Sub_metering_3, type = "l", xlab = "", ylab = "Energy sub metering", col = "blue")
        legend("topright", pch = 151, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), bty = "n")
        
        plot(Time, Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global_reactive_power")
    })
    
    # Copy the plot to a png file
    dev.copy(png, file = "plot4.png")
    
    # Close the png device
    dev.off()
    
}


# Loads the data of the nameFile file for the dates specified in the dates vector
loadData <- function(nameFile, dates){
    
    lengthRowi <- numeric()
    
    # Count the number of occurrences of each date
    for(i in dates){
        
        textiDate <- paste0("\\<", i, "\\>")
        lengthRowi <- c(lengthRowi, length(grep(textiDate, readLines(nameFile))))
        
    }
    
    # Found the initial row to load
    initRow <- grep(paste0("\\<", dates[1], "\\>"), readLines(nameFile))[1]
    
    # Define specific classes for date and time according to the format used in the file
    setAs("character","myDate", function(from) as.Date(from, format="%e/%m/%Y") )
    setAs("character","myTime", function(from) strptime(from, format="%H:%M:%S") )
    
    # Load the data using the parameters calculated above 
    data <- read.table(file = nameFile, header = TRUE, sep = ";", col.names = colnames(read.table(nameFile, nrow = 1, header = TRUE, sep = ";")), na.strings = "?", colClasses = c("myDate", "myTime", rep("numeric", 7)), skip = initRow - 2, nrows = sum(lengthRowi))
    
    # Return the data
    data
    
}
