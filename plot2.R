
#use dplyr package
library(dplyr)

#set working directory
setwd("/Users/nadinekhattak/Desktop/edaweek4/New Folder 2")

#read in files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#filter data for Baltimore City (fips=24510), group data by year, get Emissions total by year

b01 <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarize(total = sum(Emissions))

#create plot
plot2 <- with(b01, plot(year, total, pch=20, ylab = "Emissions (tons)", col="purple", 
                        main = "Total PM2.5 Emissions by Year, Baltimore City, 1999-2008"))
#create plot lm line
tr_b <- lm(total ~ year, b01)
abline(tr_b, lwd=2, col="brown")

#save plot2
dev.copy(png, file = "plot2.png", width=600, height=600)
dev.off()

