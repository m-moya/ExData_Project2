setwd("~/github/ExData_Project2")

# load required libraries
library(dplyr)

# read data from files
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# filter NEI for baltimore city
NEI_Baltimore <- filter(NEI, fips=="24510")
rm(NEI)

# create filters to get motor vehicles
# first filter is to look for mobile in the EI.Sector variable in the SCC data frame
mobile_filter <- unique(SCC$EI.Sector[grep("[Mm]obile", SCC$EI.Sector)])
SCC_motor_vehicle <- filter(SCC, EI.Sector%in%mobile_filter)

# the secod filter is to look for "Onroad" int the Data.Category field of the SCC
# data frame. This narrows the SCC codes 
SCC_motor_vehicle <- filter(SCC_motor_vehicle, Data.Category=="Onroad")

# Use the filter created above ot filter the NEI data frame which was pre-filterd for Baltimore
NEI_Baltimore_motor <- filter(NEI_Baltimore, SCC%in%as.character(SCC_motor_vehicle$SCC))

# use dplyr to group and summarize the data
Baltimore_motor_emissions_by_year <- summarise(group_by(NEI_Baltimore_motor,year), total_emissions = sum(Emissions))

# plot the data using the base plot system
plot(Baltimore_motor_emissions_by_year$year, Baltimore_motor_emissions_by_year$total_emissions, ylim=c(0,400),
     ylab="Total pm2.5 Emissions",xlab="Year",main="pm2.5 Emissions in Baltimore City by Year\nRelated to Motor Vehicles")

abline(lm(Baltimore_motor_emissions_by_year$total_emissions~as.integer(Baltimore_motor_emissions_by_year$year)), col="red", lwd=1.5)
legend(x="topright", legend=c("Emissions","Regression Line"), pch=c(5,NA), 
       lwd=c(NA,1.5), col=c("blue", "red"), cex=0.8)

# save the plot to a png file
dev.copy(png, "plot5.png")
dev.off()