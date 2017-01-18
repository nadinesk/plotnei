rm(list = ls())
#set working directory
setwd("")

#use dplyr and ggplot2 package
library(dplyr)
library(ggplot2)


#read in files
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#conatenate columns with descriptions
SCC$concat <- with(SCC, paste(Short.Name, EI.Sector, Option.Group, Option.Set, 
                              SCC.Level.One, SCC.Level.Two, SCC.Level.Three, SCC.Level.Four, 
                              sep="-"))

#indicate columns that have the word "coal" in them
SCC$coal <- data.frame(grepl("coal", SCC$concat, ignore.case = TRUE))

#subset data to include only rows that include the word "coal" in the concatented column
scc_coal <- subset(SCC, coal == TRUE)

#get only SCC codes for subsetted data (that include coal combustion)
scc_coal_01 <- scc_coal$SCC

#subset NEI data to include only data with coal combustion, using SCC codes from above step
nei01 <- subset(NEI, SCC %in% scc_coal_01 )

#merge NEI subsetted coal combustion data with SCC data 
nei02 <- merge(nei01, scc_coal, by="SCC")

#group new NEI coal data by year, and average Emissions by year
nei03 <- nei02 %>% 
  group_by(year) %>%
  summarize(avg = mean(Emissions))

#create plot emissions from coal combustion by year
gCC <- ggplot(nei03, aes(year, avg))
pCC <- gCC + geom_point(size=2, color = "brown") + geom_smooth(aes(year, avg), method = "lm", color="blue") + ylab("Emissions (tons)") + 
  labs(title = "Average PM2.5 Emissions from Coal Combustion by Year, U.S., 1999-2008")
pCC

dev.copy(png, file = "plot4.png", width=600, height=600)  
dev.off()
