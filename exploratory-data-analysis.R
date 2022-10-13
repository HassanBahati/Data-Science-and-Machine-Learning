################## 4. EXPLORATORY DATA ANALYSIS ######################
### An iterative cycle of:
#1.Generating questions about your data.
#2.Searching for answers by visualizing, transforming, and modeling your data.
#3.Using what you learn to refine your questions and/or generate new questions.

## Packages to use
#dplyr and ggplot2 in tidyverse
library(tidyverse)
library(nycflights13)
library(dplyr)
ggplot2::diamonds

###### 4.1. Visualising the distribution of variables #####
# A bar chat to view a categorical variable (called a factor/character vector  in R)
# "cut" is the categorical variable
ggplot(data = diamonds) +
  geom_bar(mapping = aes(x = cut))
## The height of the bars displays how many observations occurred
## with each x value. You can compute these values manually with;
dplyr::count():
  diamonds %>%
  count(cut)

## To examine the distribution of a continuous variable, use a histogram
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), binwidth = 0.5)

## You can compute this by hand by combining dplyr::count() and
#ggplot2::cut_width():
diamonds %>%
  count(cut_width(carat, 0.5))

## However, it’s much easier to understand overlapping lines than bars
#by using geom_freqpoly() instead of geom_histogram()
ggplot(data = diamonds, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.5)

## You should also explore a variety of binwidths when working
#with histograms, as different binwidths can reveal different patterns.
#For example, here is how the preceding graph looks when we zoom in on
#the diamonds with a size of less than three carats and choose a smaller binwidth
smaller <- diamonds %>%
  filter(carat < 3)
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

##This can then be displayed as overlapping lines rather than bars for a better picture
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.5)

##If you wish to overlap multiple histograms use a smaller binwidth
ggplot(data = smaller, mapping = aes(x = carat, color = cut)) +
  geom_freqpoly(binwidth = 0.1)

###### 4.2. Generating questions about the data #####
### 4.2.1. Typical values #####
## To turn this information into useful questions, look for anything unexpected:
## Which values are the most common? Why?
## Which values are rare? Why? Does that match your expectations?
## Can you see any unusual patterns? What might explain them?
## For instance this histogram of the dataset "diamonds" raises questions
ggplot(data = smaller, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
## Questions:
#1. Why are there more diamonds at whole carats and common fractions of carats?
#2. Why are there more diamonds slightly to the right of each peak 
#than there are slightly to the left of each peak? (Is the data skewed? Check mean and median)
# 3. Why are there no diamonds bigger than 3 carats?

## There's a clear existence of subgroups within the data
## To understand these subgroups generate new questions.
#1. How are the observations within each cluster similar to each other?
#2. How are the observations in separate clusters different from each other?
#3. How can you explain or describe the clusters?
#4. Why might the appearance of clusters be misleading?

### 4.2.2. Unusual Values ###
## Outliers are observations that are unusual; data points that don’t seem to fit the pattern. 
#Sometimes outliers are data entry errors;
#other times outliers suggest important new science
#For instance looking at the y variable of the diamonds dataset by plotting a histogram
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)
#The only evidence of outliers is the unusually wide limits on the y-axis
#The are so many observations in the rare bins are so short that you can’t see them
#Zoom in to small values of the y-axis with coord_cartesian() function
ggplot(diamonds) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))
# There are 3 unusual values at 0, 32 and 59
# We can filter these using dplyr and place them in a dataframe called "outliers"
outliers <- diamonds %>%
  filter(y < 3 | y > 20) %>%
  arrange(y)
# Justify why you've removed or kept the outliers and explain this in your results.
#For instance, y variable measures one of the three dimensions of these diamonds,
#in mm. We know that diamonds can’t have a width of 0mm, so these values must be incorrect.
ggplot(outliers) +
  geom_histogram(mapping = aes(x = y), binwidth = 0.5) +
  coord_cartesian(ylim = c(0, 50))

#### 4.2.3 Removing Outliers####
## Visualise outliers using boxplots##
boxplot(diamonds)$out

#label outliers for better visualization using the “ggbetweenstats” in “ggstatsplot” package
install.packages("ggstatsplot")
library(ggstatsplot)
ggbetweenstats(diamonds,
               price, outlier.tagging = TRUE)


## Exercises
# 1. Explore the distribution of each of the x, y, and z variables in diamonds. 
#What do you learn?
# 2. Explore the distribution of price. Do you discover anything
#unusual or surprising? (Hint: carefully think about the bin width and make sure you try a wide range of values.)
# 3. How many diamonds are 0.99 carat? How many are 1 carat?
#What do you think is the cause of the difference?
#4.Compare and contrast coord_cartesian() versus xlim() or
#ylim() when zooming in on a histogram. What happens if you
#leave binwidth unset? What happens if you try and zoom so only half a bar shows?

#### 4.2.4 MISSING VALUES ####
#If you’ve encountered unusual values in your dataset, and simply
#want to move on to the rest of your analysis, you have two options:
#Drop the entire row with the strange values: But recall to make a new file
diamonds2 <- diamonds %>%
filter(between(y, 3, 20))
#However, if you have low-quality data, by time that you’ve applied this
#approach to every variable you might find that you don’t have
#any data left!

# Instead, replace the unusual values with missing values (NA). 
# The easiest way to do this is to use mutate() to replace the variable with a modified copy
#use the ifelse() function to replace unusual values with NA:
diamonds3 <- diamonds %>%
mutate(y = ifelse(y < 3 | y > 20, NA, y))
# ggplot2 doesn't plot missing values, but mentions their removal.
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point()
ggplot(data = diamonds3, mapping = aes(x = x, y = y)) +
  geom_point()

# To suppress that warning, set na.rm = TRUE:
ggplot(data = diamonds2, mapping = aes(x = x, y = y)) +
  geom_point(na.rm = TRUE)

# If you want to understand what makes observations with
# missing values different from observations with recorded values.
#You can do this by making a new variable with is.na():
nycflights13::flights %>%
  mutate(
    cancelled = is.na(dep_time),
    sched_hour = sched_dep_time %/% 100,
    sched_min = sched_dep_time %% 100,
    sched_dep_time = sched_hour + sched_min / 60
  ) %>%
  
  ggplot(mapping = aes(sched_dep_time)) +
  geom_freqpoly(
    mapping = aes(color = cancelled),
    binwidth = 1/4
  )

## Exercises
#1. What happens to missing values in a histogram? What happens
#to missing values in a bar chart? Why is there a difference?
#2. What does na.rm = TRUE do in mean() and sum()?


###4.2.5 Detecting and Removing Outliers Using the IQR method###
# IQR = Interquartile range
# A box that stretches from the 25th percentile of the distribution
# to the 75th percentile, a distance known as the interquartile range (IQR)
# An observation is an outlier if it is above the 75th or below the 25th percentile
# by a factor of 1.5 times the IQR
# Visualise outliers in the datset diamonds using a boxplot
boxplot(diamonds)
boxplot(diamonds)$out
#label outliers for better visualization using the “ggbetweenstats” in “ggstatsplot” package
install.packages("ggstatsplot")
library(ggstatsplot)
#Load the dataset
data("diamonds")
#Filter variables of interest
diamonds_price_cut <- filter(diamonds, diamonds$cut, diamonds$price)
#Create a boxplot of the two variables (price and cut)
boxplot(price~cut, data = diamonds)
#Create a boxplot that labels the outliers
ggbetweenstats(data = diamonds, 
               x = cut,
               y = price,
               outlier.tagging = TRUE)
                
ggbetweenstats(diamonds, cut, price, outlier.tagging = TRUE)


####### 4.3. COVARIATION #######
## Covariation is the tendency for the values of two or more variables to vary together in a related way
# Generate visuals to assess covariation

##4.3.1. Covariation between a continuous and categorical variable###
# For example, explore how the price of a diamond varies with its quality
ggplot(data = diamonds, mapping = aes(x = price)) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
# This isn't the right graph as it’s hard to see the difference 
# in distribution because the overall counts differ so much:
ggplot(diamonds) +
  geom_bar(mapping = aes(x = cut))
# Instead of displaying count, display density on the y-axis,
#which is the count standardized so that the area under each 
#frequency polygon is one:
ggplot(
  data = diamonds,
  mapping = aes(x = price, y = ..density..)
) +
  geom_freqpoly(mapping = aes(color = cut), binwidth = 500)
# it appears that fair diamonds (the lowest quality) have the highest average price

# Another alternative to display the distribution of a continuous 
#variable broken down by a categorical variable is the boxplot.
ggplot(data = diamonds, mapping = aes(x = cut, y = price)) +
  geom_boxplot()
# The plot supports that better quality diamonds are cheaper on average

# Cut is an ordered factor: fair is worse than good, which is worse
# Many categorical variables don’t have such an intrinsic order
# One way to do that is with the reorder() function
# For example, take the class variable in the mpg dataset.
ggplot(data = mpg, mapping = aes(x = class, y = hwy)) +
  geom_boxplot()
#To make the trend easier to see, we can reorder class based on the median value of hwy:
ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  )
# If you have long variable names, flip the chart 90o with coord_flip()
ggplot(data = mpg) +
  geom_boxplot(
    mapping = aes(
      x = reorder(class, hwy, FUN = median),
      y = hwy
    )
  ) +
  coord_flip()

##Exercises
#1. Use what you’ve learned to improve the visualization of the
#departure times of cancelled versus noncancelled flights.
#2. What variable in the diamonds dataset is most important for
#predicting the price of a diamond? How is that variable correla‐
#ted with cut? Why does the combination of those two relation‐
#ships lead to lower quality diamonds being more expensive?
#3. Install the ggstance package, and create a horizontal boxplot.
#How does this compare to using coord_flip()?
#4. One problem with boxplots is that they were developed in an
#era of much smaller datasets and tend to display a prohibitively
#large number of “outlying values.” One approach to remedy this
#problem is the letter value plot. Install the lvplot package, and
#try using geom_lv() to display the distribution of price versus
#cut. What do you learn? How do you interpret the plots?
#5. Compare and contrast geom_violin() with a faceted geom_his
#togram(), or a colored geom_freqpoly(). What are the pros and cons of each method?
#6. If you have a small dataset, it’s sometimes useful to use
#geom_jitter() to see the relationship between a continuous
#and categorical variable. The ggbeeswarm package provides a
#number of methods similar to geom_jitter(). List them and
#briefly describe what each one does.


#####4.3.2 Covariation between two categorical variables ######
#Using the built-in geom_count():
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = color))
# Covariation will appear as a strong correlation between specific x values and specific y values

#Another approach is to compute the count with dplyr:
diamonds %>%
  count(color, cut)
# Then visualize with geom_tile() and the fill aesthetic:
diamonds %>%
  count(color, cut) %>%
  ggplot(mapping = aes(x = color, y = cut)) +
  geom_tile(mapping = aes(fill = n))

## Exercises
#1. How could you rescale the count dataset to more clearly show
#the distribution of cut within color, or color within cut?
#2. Use geom_tile() together with dplyr to explore how average
#flight delays vary by destination and month of year. What makes
#the plot difficult to read? How could you improve it?
#3. Why is it slightly better to use aes(x = color, y = cut) rather
#than aes(x = cut, y = color) in the previous example?


####4.3.3. Covariation between continuous variables ###
# Using a scatterplot the with geom_point() function
# An exponential relationship between the carat size and price of a diamond:
ggplot(data = diamonds) +
  geom_point(mapping = aes(x = carat, y = price))
# Using the alpha aesthetic to add transparency to prevent overcrowding of plotted points
ggplot(data = diamonds) +
  geom_point(
    mapping = aes(x = carat, y = price),
    alpha = 1 / 100
  )
# But this doesn't create as much clarity.
# Therefore, use geom_bin2d() and geom_hex() to bin in two dimensions
#Install necessary packages
install.packages("hexbin")
install.packages("methods")
library(hexbin)
library(methods)
ggplot(data = diamonds) +
  geom_bin2d(mapping = aes(x = carat, y = price))
ggplot(data = diamonds) +
  geom_hex(mapping = aes(x = carat, y = price))
#Another option is to bin one continuous variable so it acts like a categorical variable:
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_width(carat, 0.1)))
# Another approach is to display approximately the same number of
# points in each bin using the cut_number() function
ggplot(data = diamonds, mapping = aes(x = carat, y = price)) +
  geom_boxplot(mapping = aes(group = cut_number(carat, 20)))

#Exercises
#1. Instead of summarizing the conditional distribution with a box‐
#plot, you could use a frequency polygon. What do you need to
#consider when using cut_width() versus cut_number()? How
#does that impact a visualization of the 2D distribution of carat
#and price?
#2. Visualize the distribution of carat, partitioned by price.
#3. How does the price distribution of very large diamonds com‐
#pare to small diamonds. Is it as you expect, or does it surprise you?
#4. Combine two of the techniques you’ve learned to visualize the
#combined distribution of cut, carat, and price.
#5. Two-dimensional plots reveal outliers that are not visible in
#one-dimensional plots. For example, some points in the following 
#plot have an unusual combination of x and y values, which makes the points outliers even though their x and y values
#appear normal when examined separately:
  ggplot(data = diamonds) +
  geom_point(mapping = aes(x = x, y = y)) +
  coord_cartesian(xlim = c(4, 11), ylim = c(4, 11))
#Why is a scatterplot a better display than a binned plot for this case?  
  

#### 4.4. DATA PATTERNS AND MODELS #####
# Discerning patterns gives clues about relationships between variables
# If you spot a pattern, asking the following;
#•Could this pattern be due to coincidence (i.e., random chance)?
#• How can you describe the relationship implied by the pattern?
#• How strong is the relationship implied by the pattern?
#• What other variables might affect the relationship?
#• Does the relationship change if you look at individual sub‐groups of the data?  


#Models are a tool for extracting patterns out of data:
# For instance, t’s hard to understand the relationship
#between cut and price, because cut and carat, and carat and price,
#are tightly related
#A model has to be generated that predicts
#price from carat and then computes the residuals labelled "mod"
#(the difference between the predicted value and the actual value):
install.packages("modelr")  
library(modelr)  
mod <- lm(log(price) ~ log(carat), data = diamonds)  
diamonds2 <- diamonds %>%
  add_residuals(mod) %>%
  mutate(resid = exp(resid))
ggplot(data = diamonds2) +
  geom_point(mapping = aes(x = carat, y = resid))

