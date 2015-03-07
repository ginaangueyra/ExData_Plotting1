
# Generates lines plot of dateTime Vs Energy sub metering 1, 2 and 3 from the data 
# of nameFile and dates specified in dates vector 
plot3 <- function(namefile, dates){
    
    # Set language to English
    Sys.setlocale("LC_TIME", "English")
    
    # Call to loadData function that loads the data used to make the plot
    data <- loadData(namefile, dates)
    
    # Define column Time as dateTime
    data[, 2] <- paste(as.character(data[, 1]), substring(data[, 2], 12, 19), sep = " ")
    
    data$Time <- strptime(data$Time, tz = "EST5EDT", format="%Y-%m-%d %H:%M:%S")
    
    # Define the size of margins
    par(mar = c(3, 5, 2, 2))
    
    
    # Create plots of Time Vs 3 columns on screen device
    with(data, {
        plot(Time, Sub_metering_1, type = "l", xlab = "", ylab = "Energy sub metering")
        lines(Time, Sub_metering_2, type = "l", xlab = "", ylab = "Energy sub metering", col = "red")
        lines(Time, Sub_metering_3, type = "l", xlab = "", ylab = "Energy sub metering", col = "blue")
        legend("topright", pch = 151,col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
    })
    
    # Copy the plot to a png file
    dev.copy(png, file = "plot3.png")
    
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
