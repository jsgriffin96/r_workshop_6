#install.packages('tidyverse')
#install.packages('ggrepel')
#install.packages('ggthemes')
#install.packages('plotly')
#install.packages('lattice')
#install.packages('hrbrthemes')
#install.packages('GGally')

library(tidyverse)
library(ggrepel)
library(ggthemes)
library(scales)
library(plotly)
library(lattice)
library(GGally)

cars <- mtcars %>%
  as.tibble() %>%
  add_column(rownames(mtcars))
colnames(cars)[12] <- 'model'

cars
###Using built in plotting functions
hist(cars$disp, breaks = 10)

maxTemps <- c(75, 80, 83)
days <- c('Mon','Tues','Wed')
barplot(maxTemps, names.arg = days)

###Lattice package
xyplot(mpg ~ wt, cars) #xyplot(y~x,data)
histogram(cars$disp, breaks = 10)


###GGPLOT2 basic plots
ggplot(data = cars, aes(x = mpg, y = disp)) + #data
  geom_jitter(size=3, color='red') + #geom
  facet_grid(cols = vars(cyl)) + #coordinate
  coord_flip() + #coordinate
  theme_economist() + #coordinate
  labs(title = 'MPG vs Displacement', x = 'Miles Per Gallon', y = 'Displacement') #coordinate


###One continous variable
hist(cars$mpg)

ggplot(data = cars, aes(x = mpg)) +
  geom_histogram(binwidth = 5) +
  scale_x_continuous(name = "Miles Per Gallon",
                     breaks = seq(0, 35, 5),
                     limits = c(5, 35)) +
  scale_y_continuous(name = "Count")

###Categorical
barplot(table(cars$cyl))

ggplot(data = cars, aes(x = cyl)) +
  geom_bar()

###Two Variables
#x=continous, y=continous
cars %>% 
  select(disp, hp, mpg) %>%
  filter(mpg > 15) %>%
  ggplot(aes(x = disp, y = hp)) + 
  geom_point() +
  geom_smooth()


#x=categorical, y=continous
boxplot(data = cars, mpg~cyl)

ggplot(data=cars, aes(x=cyl, y=mpg))+
  geom_bar(stat = 'identity')

ggplot(data=cars, aes(x=cyl, y=mpg, group=cyl))+
  geom_boxplot()

#exploring data with visualization
#library(GGally)
ggpairs(cars %>% select(cyl, mpg, disp, wt, qsec))
ggcorr(cars %>% select(!model))

#interactive plots
#library(plotly)
plot <- ggplot(data = cars, aes(x = hp, y = qsec)) +
  geom_jitter()

ggplotly(plot)

#labeling
#library(ggrepel)
plot + geom_label(aes(label=model))
plot + geom_label_repel(aes(label = model))

#saving plots
ggsave(plot, filename = "mygraph.png")



####OLD#####
#histogram (one continous variable)
ggplot(data = cars, aes(x = disp)) +  #data
  geom_histogram(binwidth = 30)       #geom

#scatterplot (two continous variables, reveals correlations)
ggplot(data = cars, aes(x = disp, y = mpg)) + #data
  geom_jitter() + #geom
  coord_flip() #coordinate system

#barplot (one variable, discrete/ categorical)
ggplot(data = cars, aes(x = cyl)) +
  geom_bar() +
  coord_cartesian(xlim = c(0, 10))

#boxplot (summary of continuous variable, grouped)
ggplot(data = cars, aes(x = cyl, y = mpg, group=cyl)) +
  geom_boxplot()

#chaining plots
ggplot(data = cars, aes(x = disp, y = mpg)) +
  geom_jitter() +
  geom_smooth(method=loess)

##asthetics
#points
ggplot(data = cars, aes(x = hp, y = qsec)) +
  geom_jitter(shape = 2, colour = 'green')  #https://rkabacoff.github.io/datavis/datavis_files/figure-html/shapes1-1.png

#title
ggplot(data = cars, aes(x = hp, y = qsec)) +
  geom_jitter() +
  ggtitle("Horsepower \nvs \nQuarter Second Time")

#axes
ggplot(data = cars, aes(x = hp, y = qsec)) +
  geom_jitter() +
  scale_x_continuous(name = "Horse Power",
                     breaks = seq(0, 350, 50),
                     limits = c(0, 350)) +
  scale_y_continuous(name = "1/4 Mile Time",
                     breaks = seq(10, 25, 1),
                     limits = c(10, 25)) #,label=percent

#themes
ggplot(data = cars, aes(x = hp, y = qsec)) +
  geom_jitter() +
  theme_economist()

#labeling points
cars %>%
  filter(mpg > 20) %>%
  mutate(PtW = hp/wt) %>%
  ggplot(aes(x=hp, y=qsec )) + 
  geom_point() +
  geom_label(aes(label=model))
  #geom_label_repel(aes(label=model))
  

#interactive plots
plot <- ggplot(data = cars, aes(x = hp, y = qsec)) +
  geom_jitter()

ggplotly(plot)

#saving plots
ggsave(plot, filename = "mygraph.png")






































########OLD#########
### Importing Data
data.df <- read_csv('https://data.ca.gov/dataset/1b31c08e-b1a7-4459-8aef-41cfff61fc5e/resource/5c42e9f6-e172-4db4-9a51-ca1256b03a26/download/fleet-asset-management-system-open-data-2015-2019.csv')
summary(data.df)
#data dictionary https://data.ca.gov/dataset/california-state-fleet/resource/bf852ee6-49c5-42e5-9b2c-f531b9143b85 

### Dealing with missing data
## we need to replace "No Data", "Null" to NA
data.df[data.df == 'No Data'] <- NA
data.df[data.df == 'Null'] <- NA

### Dealing with numbers stored as characters
columnsToConvert <- c(
  "Model_Year",
  "Payload_Rating",
  "Shipping_Weight",
  "Purchase_Price",
  "Disposition_Mileage",
  "Disposition_Sold_Amount",
  "Total_Miles"
)

for (colName in columnsToConvert) {
  data.df[colName] <- parse_number(data.df[[colName]])
}

summary(data.df)

negative_miles <- data.df %>% #why do these vehicles have negative miles?
  filter(Total_Miles < 0)

#lets get rid of the vehciles with negative miles
data.df <- anti_join(data.df, negative_miles, by='Total_Miles') #anti-join is more SQL based

#drop anything made before 1900
data.df <- data.df %>% filter(!is.na(Model_Year), Model_Year > 1000)
