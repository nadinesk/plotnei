
library(dplyr)

setwd("D:/New folder")

####plot1###########
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


head(tbl_df(NEI))
head(tbl_df(SCC))

s01 <- NEI %>%
        na.omit() %>%
        group_by(year) %>%
        summarize(total = sum(Emissions)) %>%
        rename(Total_Tons_Emissions = total)
s01

plot1 <- with(s01, plot(year, Total_Tons_Emissions, pch=20, 
                        main = "Total Tons of PM2.5 Emissions by Year"))

tr <- lm(Total_Tons_Emissions ~ year, s01)
abline(tr, lwd=2)

dev.copy(png, file = "plot1.png")  
dev.off()  

####plot2###########
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

?filter

b01 <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year) %>%
  summarize(total = sum(Emissions))%>%
  rename(Total_Tons_Emissions = total)
  
plot2 <- with(b01, plot(year, Total_Tons_Emissions, pch=20,
                        main = "Total Tons of PM2.5 Emissions by Year in Baltimore City, Maryland"))


tr_b <- lm(Total_Tons_Emissions ~ year, b01)
abline(tr_b, lwd=2)

dev.copy(png, file = "plot2.png")
dev.off()


####plot3#########
library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

head(tbl_df(NEI))

sum(table(NEI$type))


c01 <- NEI %>%
  filter(fips == "24510") %>%
  group_by(year, type) %>%
  summarize(total = sum(Emissions))%>%
  rename(Total_Tons_Emissions = total)

c02 <- NEI %>%
  filter(fips == "24510")


c03 <- NEI %>%
  filter(fips == "24510") %>% 
  group_by(year, type) %>%
  summarize(total = sum(Emissions))%>%
  rename(Total_Tons_Emissions = total)

c03

yrg01 <- range(c03$Total_Tons_Emissions)
g <- ggplot(c02, aes(year, Emissions))
p <- g + geom_point() + facet_grid(. ~ type)
                                   
g01 <- ggplot(c03, aes(year, Total_Tons_Emissions))
p01 <- g01 + geom_point() + facet_grid(. ~ type) + geom_smooth(method = "lm") + 
  labs(title = "Total Tons of Emissions in Baltimore City by Type, 1999-2008") + ylim(yrg01)

p01

dev.copy(png, file = "plot3.png")  
dev.off()


###plot4###

library(tidyr)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

str(SCC)

names(SCC)

SCC$concat <- with(SCC, paste(Short.Name, EI.Sector, Option.Group, Option.Set, 
                        SCC.Level.One, SCC.Level.Two, SCC.Level.Three, SCC.Level.Four, 
                         sep="-"))

SCC$coal <- data.frame(grepl("coal", SCC$concat, ignore.case = TRUE))

table(SCC$coal)

scc_coal <- subset(SCC, coal == TRUE)

scc_coal_01 <- scc_coal$SCC

head(NEI)

nei01 <- subset(NEI, SCC %in% scc_coal_01 )

nei02 <- merge(nei01, scc_coal, by="SCC")

table(nei01$SCC)

table(scc_coal_01)


nei03 <- nei02 %>% 
          group_by(year) %>%
          summarize(total = sum(Emissions))%>%
          rename(Total_Tons_Emissions = total)

nei03

gCC <- ggplot(nei03, aes(year, Total_Tons_Emissions))
pCC <- gCC + geom_point() + geom_smooth(method = "lm") + 
  labs(title = "Coal Combustion-related Emissions in Tons, United States, 1999-2008")
pCC

dev.copy(png, file = "plot4.png")  
dev.off()

#####plot5#########

library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC$concat <- with(SCC, paste(Short.Name, EI.Sector, Option.Group, Option.Set, 
                              SCC.Level.One, SCC.Level.Two, SCC.Level.Three, SCC.Level.Four, 
                              sep="-"))


SCC$mot_veh <- data.frame(grepl("motor |motorcycle", SCC$concat, ignore.case = TRUE))
table(SCC$mot_veh)

scc_mot_veh <- subset(SCC, mot_veh == TRUE)

scc_mot_veh_01 <- scc_mot_veh$SCC


test <- data.frame(scc_mot_veh$SCC)
neiMV01 <- subset(NEI, fips == "24510" & SCC %in% scc_mot_veh_01 )

str(neiMV01)

neiMV02 <- merge(neiMV01, scc_mot_veh, by="SCC")

table(neiMV02$concat)

sum(neiMV02$Emissions)

neiMV03 <- neiMV02 %>% 
  group_by(year) %>%
  summarize(total = sum(Emissions))%>%
  rename(Total_Tons_Emissions = total)

gMV <- ggplot(neiMV03, aes(year, Total_Tons_Emissions))
pMV <- gMV + geom_point() + geom_smooth(method = "lm") + 
  labs(title = "Motor Vehicle Source Emissions in Tons, Baltimore City, 1999-2008")
pMV


dev.copy(png, file = "plot5.png")  
dev.off()

#############plot6#############

library(ggplot2)

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

SCC$concat <- with(SCC, paste(Short.Name, EI.Sector, Option.Group, Option.Set, 
                              SCC.Level.One, SCC.Level.Two, SCC.Level.Three, SCC.Level.Four, 
                              sep="-"))

SCC$mot_veh <- data.frame(grepl("motor |motorcycle", SCC$concat, ignore.case = TRUE))
table(SCC$mot_veh)

scc_mot_veh <- subset(SCC, mot_veh == TRUE)

scc_mot_veh_01 <- scc_mot_veh$SCC

neiMVBCLA01 <- subset(NEI, (fips == "24510" | fips == "06037") & SCC %in% scc_mot_veh_01 )

str(neiMVBCLA01)

table(neiMVBCLA01$fips)

neiMVBCLA02 <- merge(neiMVBCLA01, scc_mot_veh, by="SCC")

sum(neiMVBCLA02$Emissions)
str(neiMVBCLA02)
rm(neiMVBCLA03)

neiMVBCLA03 <- neiMVBCLA02 %>% 
  group_by(fips, year) %>%
  summarize(total = sum(Emissions))%>%
  rename(Total_Tons_Emissions = total, Location = fips)%>%
  as.data.frame()%>%
  mutate(Location=replace(Location, Location =="06037", "Los Angeles County")) %>%
  mutate(Location=replace(Location, Location =="24510", "Baltimore City")) 



neiMVBCLA03
str(neiMVBCLA03)



gMVBCLA <- ggplot(neiMVBCLA03, aes(year, Total_Tons_Emissions))
pMVBCLA <- gMVBCLA + geom_point(aes(color=Location)) + geom_smooth(aes(year, Total_Tons_Emissions), method = "lm") + facet_grid(. ~ Location) + 
  labs(title = "Motor Vehicle Source Emissions in Tons, Baltimore City and Los Angeles County, 1999-2008") 
pMVBCLA



dev.copy(png, file = "plot6.png")  
dev.off()

#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

