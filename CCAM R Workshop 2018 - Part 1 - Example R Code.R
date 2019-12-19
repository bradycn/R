####### CCAM R WORKSHOP - OCTOBER 5, 2018 #######
################ R CODE - PART 1 ################

# Slide 10 - R code basics
a <- c(2, 4, 6) # c() concatenates data
b <- sqrt(a)
print(b)

# Slides 15-18 - importing and investigating data
housing=read.csv("FullertonHousing.csv")
library(tidyverse)
View(housing)

# Slides 21-22 - finding things in a data frame
head(housing$PROPERTY_TYPE)
housing[1:4, c(1,3,8,10)]  # rows 1-4, columns 1, 3, 8, and 10
housing$PRICE[1:7]  # rows 1-7, variable PRICE

# Slide 24 - create training and test set
library(caret)
set.seed(1005)
rows.train <- createDataPartition(
  y = housing$PROPERTY_TYPE,  #categorical variable we want to split by
  p = .8,  # 80% of each property type in the training set
  list = FALSE
)

housing.training <- housing[rows.train,]  # rows in the training set
housing.test  <- housing[-rows.train,]  # rows in the test set (everything not in the training set)

# Slides 25-26 - missing data
missing.data <- vapply(housing.training, anyNA, TRUE)  # reports TRUE if any values are missing and FALSE if all values are there
which(missing.data == TRUE)

missing.lotsize <- which(is.na(housing.training$LOT_SIZE)) # STEP 1: find the rows where the values are missing

housing.training$LOT_SIZE[missing.lotsize] <- 0 # STEP 2: impute 0 in those rows
View(housing.training)

# Write your answer to the challenge on Slide 27 below
missing.hoa=which(is.na(housing.training$HOA))
housing.training$HOA[missing.hoa]=0
View(housing.training)

# Slides 29-31 - transforming a variable
housing.training <- housing.training %>% mutate(PRICE = PRICE/1000)   # %>% only works with tidyverse
housing.training <- housing.training %>% mutate(ZIP = as.factor(ZIP))

# Slides 33-36 - creating a histogram
plot1 <- ggplot(data = housing.training, mapping = aes(x = PRICE))
plot1_hist <- plot1 + geom_histogram(center = 500, binwidth = 50)
plot1_labeled <- plot1_hist + labs(title = "Sale Price of Fullerton Houses", 
                                   x = "Price (Thousands of $)", 
                                   y = "Number of Houses")
print(plot1_labeled)  # creates a grayscale histogram

# Slides 37-38 - adding color to a histogram
plot1_color <- ggplot(data = housing.training, 
                      mapping = aes(x = PRICE, fill = PROPERTY_TYPE))
plot1_color_hist <- plot1_color + geom_histogram(center = 500, binwidth = 50)
plot1_color_labeled <- plot1_color_hist + labs(title = "Sale Price of Fullerton Houses", x = "Price (Thousands of $)", y = "Number of Houses")
print(plot1_color_labeled)

# Slides 41-42 - default ggplot2 themes
plot1_bw <- plot1_color_labeled + theme_bw()
print(plot1_bw)

plot1_minimal <- plot1_color_labeled + theme_minimal()
print(plot1_minimal)

# Slides 43-44 - changing colors in ggplot2
plot1_colorchange <- plot1_minimal + scale_fill_manual(name = "Property Type", # changes legend title
                                                       breaks = c("Condo/Co-op", "Townhouse", "Single Family Residential"), 
                                                       labels = c("Condo/Co-op"= "Condo", "Townhouse" = "Townhouse","Single Family Residential" = "Single Family"), 
                                                       values = c("Condo/Co-op" = "blue", "Townhouse" = "orange", "Single Family Residential" = "black"))
print(plot1_colorchange)

# Slides 45-46 - more theme changes
plot1_centered <- plot1_colorchange + theme(plot.title = element_text(hjust = 0.5))
plot1_legend_bottom <- plot1_centered + theme(legend.position = "bottom")
plot1_final<-plot1_colorchange+theme(plot.title = element_text(hjust = 0.5),legend.position = "bottom")
print(plot1_final)

# Slides 47-48 - plotting by group
facet_titles <- c("92831" = "92831", "92832" = "92832", "92833" = "92833", "92835" = "92835")
plot1_faceted <- plot1_final + facet_wrap(~ZIP, labeller = labeller(.cols = facet_titles))
print(plot1_faceted)

row_titles <- c("Condo/Co-op"= "Condo", "Townhouse" = "Townhouse","Single Family Residential" = "Single Family")
column_titles <- c("92831" = "92831", "92832" = "92832", "92833" = "92833", "92835" = "92835")
plot1_gridded <- plot1_final + facet_grid(PROPERTY_TYPE~ZIP, labeller = labeller(.rows = row_titles, .cols = column_titles))
print(plot1_gridded)

# Slides 50-54 - creating scatterplots
plot2 <- ggplot(data = housing.training, mapping = aes(x = SQUARE_FEET, y = PRICE)) + geom_jitter() + theme_minimal()
print(plot2) # basic jittered scatterplot

plot2_color <- ggplot(data = housing.training, mapping = aes(x = SQUARE_FEET, y = PRICE, color = PROPERTY_TYPE)) + geom_jitter() + theme_minimal()
print(plot2_color) # scatterplot with color

plot2_colorchange <- plot2_color + scale_color_manual(name = "Property Type", 
                                                      breaks = c("Condo/Co-op", "Townhouse", "Single Family Residential"), 
                                                      labels = c("Condo/Co-op"= "Condo", "Townhouse" = "Townhouse","Single Family Residential" = "Single Family"), 
                                                      values = c("Condo/Co-op" = "blue", "Townhouse" = "orange", "Single Family Residential" = "purple"))
print(plot2_colorchange)  # scatterplot with new colors

plot2_linreg <- plot2_colorchange + geom_smooth(color = "black", method = "lm", se = FALSE)
print(plot2_linreg) # scatterplot with linear trendline

plot2_final <- plot2_linreg + labs(x = "Square Footage", y = "Price (Thousands of $)", title = "Fullerton Houses Sold (Summer 2018)") + 
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + facet_wrap(~ZIP)
print(plot2_final)  # scatterplot with labels and facets

# Slides 56-57 - create the least-squares regression line
lm_sqft <- lm(PRICE ~ SQUARE_FEET, data = housing.training) # the actual model
summary(lm_sqft)  # summarize the model

# Slide 58 - add the least-squares regression line to the plot
intercept <- coef(lm_sqft)[1]
slope <- coef(lm_sqft)[2]
plot2_linreg2 <- plot2_colorchange + geom_abline(slope = slope, intercept = intercept)
print(plot2_linreg2)

#### IMPORTANT NOTE: YOU NEED THE VARIABLES CREATED FROM PART 1 IN YOUR ENVIRONMENT FOR PART 2 TO WORK
#### IT SHOULD WORK IF YOU DON'T CLOSE OUT OF R DURING THE BREAK, BUT JUST TO BE SAFE, SAVE YOUR ENVIRONMENT
#### TO SAVE, CLICK THE ENVIRONMENT TAB, THEN THE SAVE BUTTON
