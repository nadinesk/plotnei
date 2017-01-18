

#use dplyr and ggplot2 package
library(dplyr)
library(ggplot2)

#set working directory
setwd("/Users/nadinekhattak/Desktop/edaweek4/New Folder 2")

#read in files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

test <- subset(NEI, type=="NONPOINT" & fips == "24510" & year == "1999")

sum(test$Emissions)
1509.5/36
table(test$SCC)


#filter NEI data for Baltimore City, group by year, and average by year
c01 <- NEI %>%
  filter(fips == "24510") %>% 
  group_by(year, type) %>%
  summarize(avg = mean(Emissions))

c01

#create plot
g01 <- ggplot(c01, aes(year, avg))
p01 <- g01 + geom_point(size=2, color = "purple") + facet_grid(. ~ type) + 
  geom_smooth(aes(year, avg), method = "lm", color="brown") + ylab("Emissions (tons)") +
  labs(title = "Average PM2.5 Emissions by Year and Type, Baltimore City, 1999-2008") 

#print plot
p01

#save plot3
dev.copy(png, file = "plot3.png", width=800)  
dev.off()
