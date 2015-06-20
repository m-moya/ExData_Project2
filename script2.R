setwd("~/github/ExData_Project2")

# Read data from files
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# Filter data with Baltimore fips code
NEI_Baltimore <- NEI[NEI$fips=="24510",]

# calculate total pm25 emission readings
# total was chosen over mean arbitrarily
# both show downard trend in emissions
total_pm25_Baltimore <- tapply(NEI_Baltimore$Emissions, as.factor(NEI_Baltimore$year), sum)

# separate years for x-axis of plot
years <- names(total_pm25_Baltimore)

# use base plot to plot as scatter plot with a linear regression line to show trend
plot(years, total_pm25_Baltimore, ylim=c(0,3500), xlab="Year", ylab="Total pm25",
     main="Total pm25 Emissions by Year for Baltimore", pch=15, col="blue")
abline(lm(total_pm25_Baltimore~as.integer(years)), col="red", lwd=1.5)
legend(x="bottomleft", legend=c("Emissions","Regression Line"), pch=c(15,NA), lwd=c(NA,1.5), col=c("blue", "red"))
       
# save plot to png file
dev.copy(png, "plot2.png")
dev.off()
