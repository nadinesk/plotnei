
#use dplyr package
library(dplyr)

#set working directory
setwd("")

#read in data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#group NEI data by year and total Emissions by year 
s01 <- NEI %>%
  group_by(year) %>%
  summarize(total = sum(Emissions)) 

#create plot
plot1 <- with(s01, plot(year, total, pch=20, ylab="Emissions (tons)", col="red",
                        main = "Total PM2.5 Emissions by Year, U.S., 1999-2008"))
#create abline
tr <- lm(total ~ year, s01)
abline(tr, lwd=2, col="blue")

#save plot1.png
dev.copy(png, file = "plot1.png", width=600, height=600)  
dev.off()  
