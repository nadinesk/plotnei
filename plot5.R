

#set working directory
setwd("/Users/nadinekhattak/Desktop/edaweek4/New Folder 2")

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

#indicate columns that have the word "motor" or "motorcycle" in them, but not rocket motor (add space after "motor ") 
SCC$mot_veh <- data.frame(grepl("motor |motorcycle", SCC$concat, ignore.case = TRUE))

#subset data to include only rows that include the word "motor " or "motorcycle" in the concatented column
scc_mot_veh <- subset(SCC, mot_veh == TRUE)

#get only SCC codes for subsetted data (that include "motor " or "motorcycle")
scc_mot_veh_01 <- scc_mot_veh$SCC

#subset data to get only Baltimore City and data that includes "motor " or "motorcycle"
neiMV01 <- subset(NEI, fips == "24510" & SCC %in% scc_mot_veh_01 )

#merge NEI subsetted motor vehicle data with SCC data 
neiMV02 <- merge(neiMV01, scc_mot_veh, by="SCC")

#group subsetted motor vehilcle, Baltimore City data, and then total emissions by year also
neiMV03 <- neiMV02 %>% 
  group_by(year) %>%
  summarize(avg = mean(Emissions))

#create plot
gMV <- ggplot(neiMV03, aes(year, avg))
pMV <- gMV + geom_point(size=4, color="purple") + geom_smooth(aes(year, avg),method = "lm") + ylab("Emissions (tons)") + 
  labs(title = "Average PM2.5 Emissions from Motor Vehicles by Year, Baltimore City, 1999-2008")
pMV

#save plot
dev.copy(png, file = "plot5.png", width = 600)  
dev.off()
