## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
## Using the base plotting system, make a plot showing the total PM2.5 emission
## from all sources for each of the years 1999, 2002, 2005, and 2008.

setwd("C:/Users/actge/Documents/R/exdata_data_NEI_data")
# Read PM2.5 emissions dataset
emissions.df <- readRDS('summarySCC_PM25.rds')

# Read emission source classification dataset
scc.df <- readRDS('Source_Classification_Code.rds')


# Get the total emissions by year
emission.totals.by.year <- aggregate(Emissions ~ year, data = emissions.df, FUN = sum)


# Open png device
png(filename='plot1.png', width=480, height=480, units='px')

# Print numeric values in fixed notation
options(scipen=10)

with(emission.totals.by.year, {
  plot(year, Emissions, type = 'b',  
       xlab="Year",
       ylab='PM2.5 Emissions (tons)',
       main='Total PM2.5 Emissions in U.S.A.')
})

# Close png device
dev.off()
