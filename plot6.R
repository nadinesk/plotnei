

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

#indicate columns that have the word "motor" or "motorcycle" in them, but not rocket motor (add space after "motor ") 
SCC$mot_veh <- data.frame(grepl("motor |motorcycle", SCC$concat, ignore.case = TRUE))

#subset data to include only rows that include the word "motor " or "motorcycle" in the concatented column
scc_mot_veh <- subset(SCC, mot_veh == TRUE)

#get only SCC codes for subsetted data (that include "motor " or "motorcycle")
scc_mot_veh_01 <- scc_mot_veh$SCC

#subset data to get only Baltimore City and LA County and data that includes "motor " or "motorcycle"
neiMVBCLA01 <- subset(NEI, (fips == "24510" | fips == "06037") & SCC %in% scc_mot_veh_01 )

#merge NEI subsetted motor vehicle data with SCC data 
neiMVBCLA02 <- merge(neiMVBCLA01, scc_mot_veh, by="SCC")

#group data by Baltimore City or LA County and year, total emissions by those groups, replace names of fips codes to names of locations 
neiMVBCLA03 <- neiMVBCLA02 %>% 
  group_by(fips, year) %>%
  summarize(avg = mean(Emissions))%>%
  as.data.frame()%>%
  mutate(fips=replace(fips, fips =="06037", "Los Angeles County")) %>%
  mutate(fips=replace(fips, fips =="24510", "Baltimore City")) 

#create plot
gMVBCLA <- ggplot(neiMVBCLA03, aes(year, avg))
pMVBCLA <- gMVBCLA + geom_point(aes(color=fips)) + geom_smooth(aes(year, avg), method = "lm") + facet_grid(. ~ fips) + 
  labs(title = "Average PM2.5 Emissions from Motor Vehicles by Year, Baltimore City and L.A. County, 1999-2008") + ylab("Emissions (tons)") + 
  theme(legend.position="none") 
pMVBCLA

#save plot
dev.copy(png, file = "plot6.png")  
dev.off()
