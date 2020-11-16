#install.packages('tidyverse')
#install.packages('ggrepel')
#install.packages('ggthemes')
#install.packages('scales')
#install.packages('plotly')
#install.packages('lattice')
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
###Using a built in plotting function
hist(cars$disp, breaks = 10)
#barplot will be discussed later

###Lattice package
xyplot(mpg ~ wt, cars) #xyplot(y~x,data)
histogram(cars$disp, breaks = 10)

###GGPLOT2 basic plots
ggplot(data = cars, aes(x = mpg, y = disp)) + #data
  geom_point(size=3, color='red') + #geom
  facet_grid(cols = vars(cyl)) + #coordinate
  coord_flip() + #coordinate
  theme_economist() + #coordinate
  labs(title = 'MPG vs Displacement', x = 'Miles Per Gallon', y = 'Displacement') #coordinate

###One continous variable 
ggplot(data = cars, aes(x = mpg)) +
  geom_histogram(binwidth = 5)

###Categorical
barplot(table(cars$cyl))

ggplot(data = cars, aes(x = cyl)) +
  geom_bar()

###Two Variables
#x=continous, y=continous (done above)

#x=categorical, y=continous 
boxplot(data = cars, mpg~cyl) #move to appendix

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
  geom_point() + geom_smooth()

ggplotly(plot)

#labeling
#library(ggrepel)
plot + geom_label(aes(label=model))
plot + geom_label_repel(aes(label = model))

#saving plots
ggsave(plot, filename = "mygraph.png")



###Apendix###
#adjusting the x and y axis
ggplot(data = cars, aes(x = mpg)) +
  geom_histogram(binwidth = 5) +
  scale_x_continuous(name = "Miles Per Gallon",
                     breaks = seq(0, 35, 5),
                     limits = c(5, 35)) +
  scale_y_continuous(name = "Count")

