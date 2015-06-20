setwd("~/github/ExData_Project2")

#load required libraries
library(dplyr)

# read data from files
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# filter SCC table using EI.Sector variable
# looked at other variables, but the EI.Sector variable looked like the right one to use
# created a filter vector from SCC table to apply to NEI data frame
SCC_Filter <- as.character(SCC[SCC$EI.Sector %in% SCC$EI.Sector[grep("Coal",SCC$EI.Sector)], "SCC"])

# apply filter to NEI data frame
NEI_Coal_Combustion <- NEI[NEI$SCC %in% SCC_Filter,]

# use dplyr to perform grouping and summarising
group_year <- group_by(NEI_Coal_Combustion, year)

# scaled emission by 100000
emissions_by_year <- summarise(group_year, total_emissions=sum(Emissions)/100000)

# make plot using base plot
plot(emissions_by_year$year, emissions_by_year$total_emissions, ylim=c(0,6), cex=0.6,
     ylab="Emissions (100 thousands)",xlab="Year",pch=5,main="pm2.5 emissions in US \nfor Coal Cumbustion by Year")
abline(lm(emissions_by_year$total_emissions~as.integer(emissions_by_year$year)), col="red", lwd=1.5)
legend(x="bottomleft", legend=c("Emissions","Regression Line"), pch=c(5,NA), 
       lwd=c(NA,1.5), col=c("blue", "red"), cex=0.8)

# save plot to png file
dev.copy(png, "plot4.png")
dev.off()
