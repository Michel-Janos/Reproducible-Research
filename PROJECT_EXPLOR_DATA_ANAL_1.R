EPC <- read.table("EPC_PROJ.csv", header=T, sep=";", dec=";", stringsAsFactors =F)


# convertendo para numerico

EPC$Global_active_power <- sapply(EPC$Global_active_power , as.numeric) 
EPC$Global_reactive_power <- sapply(EPC$Global_reactive_power , as.numeric) 


library(dplyr)
library(ggplot2)
glimpse(EPC)
max(EPC$Global_active_power)
summary(EPC$Global_active_power)

# hist(EPC$Global_active_power)
# 
# qplot(data=EPC,x=Global_active_power,binwidth = .5,
#       color="black",fill="red",main="Global Active Power", xlab = "Global Active Power (kilowatts)", ylab ="Frequency")
# 
# a <- ggplot(EPC_bind, aes(Global_active_power))
#    
# a + geom_bar(stat="identity")
# a
# 
# qplot(Global_active_power, data=EPC_bind, geom="histogram", fill="black", binwidth = .5, xlab = "Global Active Power (kilowatts)",
#       ylab ="Frequency",main="Global Active Power")
#############################################################################
#Plot 1


plot1 <- ggplot(EPC_bind, aes(x=Global_active_power)) +
  geom_histogram(binwidth=.5, colour="black", fill="red") +
ggtitle("Global Active Power (kilowatts)") +
  
  xlab("Global Active Power (kilowatts)") +  ylab("Frequency")
plot1

############################################################################
# Merging date and Time in date_time

start <- as.POSIXct("2007-02-01")
interval <- 60 # segundos
end <- start + as.difftime(2, units="days")
date_time <- seq(from=start, by=interval, to=end-1)

# tail(date_time)
# class(date_time)

EPC_bind <- cbind(EPC, date_time)
tail(EPC_bind)
#as.POSIXct(paste(times, x), format="%Y-%m-%d %H:%M:%S")



class
library(dplyr)
library(ggplot2)
#glimpse(EPC)

##################################################
#Plot 2




 a <- ggplot(EPC_bind, aes(date_time , Global_active_power))
 plot2 <- a  + geom_step(direction = "hv") +   # direction of stairs: 'vh' for vertical then horizontal, or 'hv' for horizontal then vertical
  ggtitle(" ")+
  ylab("Global Active Power (kilowatts)") +  xlab("Month - day - hour")

plot2

#################################################################################

#Plot 3


plot3 <- ggplot(EPC_bind, aes(EPC_bind$date_time)) + 
  geom_line(aes(y = Sub_metering_1 , colour = "Sub_metering_1")) + 
  geom_line(aes(y = Sub_metering_2, colour = "Sub_metering_2"))+
  geom_line(aes(y = Sub_metering_3, colour = "Sub_metering_3"))+
  ggtitle(" ")+   # no main title
  ylab("Energy sub mettering") +  xlab("Month - day - hour")+
  theme(legend.justification = 'right', legend.position=c(1,.9)) +  # position of legends
  theme(legend.title=element_blank()) # remove legent title

plot3



# tail(EPC_bind)
# summary(EPC_bind)
# summary(g)


######################################################################


#Plot 4


# g1 <- ggplot(EPC_bind, aes(EPC_bind$date_time)) + 
#   geom_line(aes(y = Global_active_power , colour = "var0"))
# 
# g1
# g1 + facet_grid(EPC_bind$Global_active_power)


library(ggplot2)
library(grid)
library(gridExtra)
#summary(EPC)
##########################################################
#PLOT 4



p1 <- ggplot(EPC_bind, aes(EPC_bind$date_time))+ 
  geom_line(aes(y = Global_active_power) )+
  ggtitle(" ")+   # no main title
  ylab("Global Active Power") +  xlab("Month - day - hour")
p1





p2 <- ggplot(EPC_bind, aes(EPC_bind$date_time))+ 
  geom_line(aes(y = Voltage ))+
  ggtitle(" ")+   # no main title
  ylab("VOltager") +  xlab("Month - day - hour")
  
p2

  
p3 <- plot3

p3

p4 <- ggplot(EPC_bind, aes(EPC_bind$date_time))+ 
  geom_line(aes(y = Global_reactive_power ))+
  ggtitle(" ")+   # no main title
  ylab("Global Reactive Power") +  xlab("Month - day - hour")

p4

# Multiple plot function
# 
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
# 
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout)))) 
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(p1, p3, p2,p4, cols=2)
