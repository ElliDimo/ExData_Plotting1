plot2 <- function(){
    
    #reading data separately for the 2 days and rowbinding them
    data1<-read.table('household_power_consumption.txt',header=T,sep=';')[which(read.table('household_power_consumption.txt',header=T,sep=';',na.strings = '?')$Date==as.character('1/2/2007')),]
    data2<-read.table('household_power_consumption.txt',header=T,sep=';')[which(read.table('household_power_consumption.txt',header=T,sep=';',na.strings = '?')$Date==as.character('2/2/2007')),]
    data<-rbind(data1,data2)
    # all elements of data frame are factors by now
    
    # combining date and time to 1 column (Date_Time)
    data$Date<-strptime(data$Date,format = '%d/%m/%Y')
    data$Date<-as.Date(data$Date)
    data$Time<-as.character(data$Time)
    data$Date_Time <- paste(data$Date,data$Time,sep=" ")
    data$Date_Time <- strptime(data$Date_Time,format = "%Y-%m-%d %H:%M:%S")
    
    # converting factors to numeric! 
    data$Global_active_power<-as.numeric(as.character(data$Global_active_power))

    # making 2nd plot
    par(mfrow = c(1,1))
    with(data, plot(Date_Time,Global_active_power,type = 'l',xlab='',ylab='Global Active Power (kilowatts)')) 
    dev.copy(png, file = "plot2.png") 
    dev.off() 
    
}