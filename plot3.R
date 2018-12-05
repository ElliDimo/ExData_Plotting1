plot3 <- function(){
    
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
    data$Sub_metering_1<-as.numeric(as.character(data$Sub_metering_1))
    data$Sub_metering_2<-as.numeric(as.character(data$Sub_metering_2))
    data$Sub_metering_3<-as.numeric(as.character(data$Sub_metering_3))
    
    # making 3rd plot
    par(mfrow = c(1,1))
    with(data, plot(Date_Time,Sub_metering_1,type = 'l',xlab='',ylab='Energy sub metering')) 
    lines(data$Date_Time, y = data$Sub_metering_2, type = "l",col='red')
    lines(data$Date_Time, y = data$Sub_metering_3, type = "l",col='blue')
    legend('topright',legend = c('Sub_metering_1','Sub_metering_2','Sub_metering_3'),col=c('black','red','blue'),lty=1,bty = 'o',pt.cex=0.7,xjust=1)
    dev.copy(png, file = "plot3.png") 
    dev.off() 
}   
