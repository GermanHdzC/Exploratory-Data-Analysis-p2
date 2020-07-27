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


###################################


## Question:
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
## from 1999 to 2008? Use the base plotting system to make a plot answering this question.


# Read PM2.5 emissions dataset
emissions.df <- readRDS('summarySCC_PM25.rds')

# Read emission source classification dataset
scc.df <- readRDS('Source_Classification_Code.rds')


# Get the emissions in Baltimore City
baltimore.emissions.df = emissions.df[emissions.df$fips == '24510', ]

# Get the total emissions in Baltimore City by year
baltimore.emission.totals.by.year <- aggregate(Emissions ~ year, data = baltimore.emissions.df, FUN = sum)


# Open png device
png(filename='plot2.png', width=480, height=480, units='px')

# Print numeric values in fixed notation
options(scipen=10)

with(baltimore.emission.totals.by.year, {
  plot(year, Emissions, type = 'b',
       xlab="Year",
       ylab='PM2.5 Emissions (tons)',
       main='Total PM2.5 Emissions in Baltimore City')
})

# Close png device
dev.off()


###########################

## Question:
## Of the four types of sources indicated by the type (point, nonpoint, 
## onroad, nonroad) variable, which of these four sources have seen decreases in emissions 
## from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? 
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



###################3


## Question:
## Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?


# Read PM2.5 emissions dataset
emissions.df <- readRDS('summarySCC_PM25.rds')

# Read emission source classification dataset
scc.df <- readRDS('Source_Classification_Code.rds')


# Get the coal combustion-related sources
# Use the 'Short.Name' field to find the coal combustion-related sources
scc.coal.df <- scc.df[grep('coal', scc.df$Short.Name), ]

# Get the emissions from coal combustion-related sources
coal.emissions.df <- emissions.df[emissions.df$SCC %in% scc.coal.df$SCC, ]
# Get the total emissions from coal combustion-related sources by year
coal.emissions.by.year.df <- aggregate(Emissions ~ year, data = coal.emissions.df, FUN = sum)


# Open png device
png(filename='plot4.png', width=480, height=480, units='px')

# Print numeric values in fixed notation
options(scipen=10)

with(coal.emissions.by.year.df, {
  plot(year, Emissions, type = 'b',
       xlab="Year",
       ylab='PM2.5 Emissions (tons)',
       main='PM2.5 Emissions from coal combustion sources in U.S.A.')
})

# Close png device
dev.off()



##################

## Question:
## How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City? 


# Read PM2.5 emissions dataset
emissions.df <- readRDS('summarySCC_PM25.rds')

# Read emission source classification dataset
scc.df <- readRDS('Source_Classification_Code.rds')


# Get the emissions from motor vehicle sources (type = ON-ROAD) in Baltimore City (fips code: '24510')
baltimore.motor.emissions.df <- emissions.df[emissions.df$type == 'ON-ROAD' & emissions.df$fips == '24510', ]

# Get the total Baltimore emissions from motor vehicle sources by year
baltimore.motor.emissions.by.year.df <- aggregate(Emissions ~ year, data = baltimore.motor.emissions.df, FUN = sum)


# Open png device
png(filename='plot5.png', width=480, height=480, units='px')

# Print numeric values in fixed notation
options(scipen=10)

with(baltimore.motor.emissions.by.year.df, {
  plot(year, Emissions, type = 'b',
       xlab="Year",
       ylab='PM2.5 Emissions (tons)',
       main='PM2.5 Emissions from motor vehicle sources in Baltimore City')
})

# Close png device
dev.off()


#########################

## Question:
## Compare emissions from motor vehicle sources in Baltimore City with emissions
## from motor vehicle sources in Los Angeles County, California (fips == "06037"). 
## Which city has seen greater changes over time in motor vehicle emissions?


library(ggplot2)

# Read PM2.5 emissions dataset
emissions.df <- readRDS('summarySCC_PM25.rds')

# Read emission source classification dataset
scc.df <- readRDS('Source_Classification_Code.rds')


# Get the emissions from motor vehicle sources (type = ON-ROAD) in Baltimore City (fips code: '24510') and Los Angeles (fips code: '06037')
city.emissions.df <- emissions.df[emissions.df$type == 'ON-ROAD' & emissions.df$fips %in% c('24510', '06037'), ]

# Get the total emissions for Baltimore City and Los Angeles, grouped by year and city
city.emissions.by.year.df <- aggregate(Emissions ~ year + fips, data = city.emissions.df, FUN = sum)

# Add city names for use in the plot
city.emissions.by.year.df$city = city.emissions.by.year.df$fips
city.emissions.by.year.df[which(city.emissions.by.year.df$fips == '24510'), 'city'] = 'Baltimore City'
city.emissions.by.year.df[which(city.emissions.by.year.df$fips == '06037'), 'city'] = 'Los Angeles'

# Open png device
png(filename='plot6.png', width=640, height=480, units='px')

# Print numeric values in fixed notation
options(scipen=10)

plot6 <- qplot(year, Emissions, data=city.emissions.by.year.df, facets = . ~ fips) +  
  geom_line() +
  xlab("Year") +
  ylab('PM2.5 Emissions (tons)') +
  ggtitle('Total PM2.5 Emissions in Los Angeles (06037) and Baltimore City (24510)')

print(plot6)

# Close png device
dev.off()