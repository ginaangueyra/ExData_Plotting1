
# Generates the histogram of Global Active Power from the data of nameFile for the 
# dates specified in the dates vector 
plot1 <- function(namefile, dates){
    
    # Call to loadData function that loads the data used to make the plot
    data <- loadData(namefile, dates)
    
    # Define the size of margins
    par(mar = c(5, 5, 2, 2))
    
    # Create plot on screen device
    hist(data$Global_active_power, col = "red", xlab = "Global Active Power (kilowatts)", 
         ylab = "Frequency", main = "Global Active Power")
    
    # Copy the plot to a png file
    dev.copy(png, file = "plot1.png")
    
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
