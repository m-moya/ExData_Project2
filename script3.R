setwd("~/github/ExData_Project2")

library(ggplot2)
library(dplyr)

# READ DATA FROM FILES
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

NEI_Baltimore <- filter(NEI, fips=="24510")

# using only start year and end year to make plot easier to interpret
# the two years should be enough to answer the question about changes
# from 1999 to 2008
NEI_Baltimore_2yr <- filter(NEI_Baltimore, year==c(1999,2008))

# group by year and top so that bar chart could be split by 
# year on x-axis and year for fills
group_year_type <- group_by(NEI_Baltimore_2yr, year, type) 

# calculate the total emissions
# total used over average arbitrarily
total_emissions_type_year <- summarise(group_year_type, total_emissions=sum(Emissions))

# make bar chart showing changes of total emissions from 1999 to 2008 by type
plot <- ggplot(total_emissions_type_year, aes(x = factor(type), y = total_emissions, fill=factor(year))) +
      geom_bar(stat = "identity", position="dodge") +
      scale_size_area() + 
      xlab("Type") +
      ylab("Total pm25 Emissions")  +
      scale_fill_discrete(name="Year")
plot + ggtitle("Change in Total Emissions in Baltimore City by Type")

# save plot to png file
ggsave(plot, file="plot3.png", width=3, height =2, scale=2)