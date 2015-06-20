setwd("~/github/ExData_Project2")

# Read data from files
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

# calculate total pm25 emission readings
# total was chosen over mean arbitrarily
# both show downard trend in emissions
# scaled emission result by 1000000 to reduce number of digits in plot
total_pm25 <- tapply(NEI$Emissions, as.factor(NEI$year), sum)/1000000

# separate years for x-axis of plot
years <- names(total_pm25)

# use base plot to plot as scatter plot with a linear regression line to show trend
plot(years, total_pm25, ylim=c(0,10), xlab="Year", ylab="Total pm25 (millions)",
     main="Total U.S. pm25 Emissions by Year", pch=5, col="blue")
abline(lm(total_pm25~as.integer(years)), col="red", lwd=1.5)
legend(x="bottomleft", legend=c("Emissions","Regression Line"), pch=c(5,NA), 
       lwd=c(NA,1.5), col=c("blue", "red"), cex=0.8)

# copy plot 1 to png file
dev.copy(png, "plot1.png")
dev.off()