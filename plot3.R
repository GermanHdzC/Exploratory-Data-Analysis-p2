## Question:
## Of the four types of sources indicated by the type (point, nonpoint, 
## onroad, nonroad) variable, which of these four sources have seen decreases in emissions 
## from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? 
## Use the ggplot2 plotting system to make a plot answer this question.


library(ggplot2)
 
# Read PM2.5 emissions dataset
emissions.df <- readRDS('summarySCC_PM25.rds')
 
# Read emission source classification dataset
scc.df <- readRDS('Source_Classification_Code.rds')
 
 
# Get the emissions in Baltimore City
baltimore.emissions.df = emissions.df[emissions.df$fips == '24510', ]
 
# Get the total emissions in Baltimore, grouped by type and year
baltimore.emission.totals.df <- aggregate(Emissions ~ type + year, baltimore.emissions.df, FUN = sum)
 
 
# Open png device
png(filename='plot3.png', width=1200, height=640, units='px')
 
# Print numeric values in fixed notation
options(scipen=10)
 
plot3 <- qplot(year, Emissions, data=baltimore.emission.totals.df, facets = . ~ type) +  
    geom_line() +
    xlab("Year") +
    ylab('PM2.5 Emissions (tons)') +
    ggtitle('Total PM2.5 Emissions in Baltimore City')

print(plot3)

# Close png device
dev.off()
