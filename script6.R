setwd("~/github/ExData_Project2")

# load required libraries
library(dplyr)
library(ggplot2)

# read data from files
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Baltimore City (fips="24510"), Los Angeles County(fips ="06037")
# filter NEI for Baltimore City and Los Angeles
NEI_Two_Places <- filter(NEI, fips %in% c("24510","06037"))

# remove NEI
rm(NEI)

# create filters to get motor vehicles
# first filter is to look for mobile in the EI.Sector variable in the SCC data frame
mobile_filter <- unique(SCC$EI.Sector[grep("[Mm]obile", SCC$EI.Sector)]) 
mobile_filter <- unique(SCC$EI.Sector[grep("[Mm]obile", SCC$EI.Sector)])
SCC_motor_vehicle <- filter(SCC, EI.Sector%in%mobile_filter)

# the secod filter is to look for "Onroad" int the Data.Category field of the SCC
# data frame. This narrows the SCC codes 
SCC_motor_vehicle <- filter(SCC_motor_vehicle, Data.Category=="Onroad")

# Use the filter created above ot filter the NEI data frame which was pre-filterd for Baltimore 
# and Los Angeles
NEI_Two_motor <- filter(NEI_Two_Places, SCC%in%as.character(SCC_motor_vehicle$SCC))

# remove SCC and NEI_Two_Places to clear memory
rm(SCC, NEI_Two_Places)

# create location variable to hold the location text instead of showing fips in graph
NEI_Two_motor$location <- "blank"
for(i in seq(1:length(NEI_Two_motor$fips))) {
  if (NEI_Two_motor$fips[i]=="24510") {
    NEI_Two_motor$location[i] <- "Baltimore"    
  } else if (NEI_Two_motor$fips[i]=="06037" ) {
    NEI_Two_motor$location[i] <- "Los Angeles"  
  }
}

# use dplyr to group and summarize the data
Two_Places_motor_emissions_by_year <- summarise(group_by(NEI_Two_motor,year,location), total_emissions = sum(Emissions))

# show results to get the scaling factor used below
Two_Places_motor_emissions_by_year

# create a new variable scaled_emissions and scal results for each location
# by value in 1999
Two_Places_motor_emissions_by_year$scaled_emissions <- 0

Two_Places_motor_emissions_by_year$scaled_emissions[Two_Places_motor_emissions_by_year$location=="Baltimore"] <- 
  Two_Places_motor_emissions_by_year$total_emissions[Two_Places_motor_emissions_by_year$location=="Baltimore"]/346.82

Two_Places_motor_emissions_by_year$scaled_emissions[Two_Places_motor_emissions_by_year$location=="Los Angeles"] <- 
  Two_Places_motor_emissions_by_year$total_emissions[Two_Places_motor_emissions_by_year$location=="Los Angeles"]/3931.12

# plot the data using the ggplot2 system
plot <- ggplot(Two_Places_motor_emissions_by_year, aes(x = factor(year), 
              y = scaled_emissions, fill=factor(location))) +
      geom_bar(stat = "identity", position="dodge") +
      scale_size_area() + 
      xlab("Year") +
      ylab("Scaled Total Emissions")  +
      scale_fill_discrete(name="Location")
plot + ggtitle("Comparison of Change in Emissions\nin Baltimore City and Los Angeles\nFor Motor Vehicle Sources")

# save the plot to a png file
ggsave(plot, file="plot6.png", width=6, height = 4)