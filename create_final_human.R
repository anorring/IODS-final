######################################################################################################################
### Author: Anni Norring                                                                                           ###
### Date: 13.12.2017 						                                                                                   ###
### Content: This script contains the R code for final assignment data wrangling.                                  ###
######################################################################################################################

# Access all the needed libraries:
library(dplyr)
library(stringr)

######################################################################################################################
###                                 DATA WRANGLING for Final Assignment                                            ###
######################################################################################################################

### NOTE: This script follows quite closely to the one I made for RStudio exercises 4 and 5. 
###       The main difference is that here I do not exclude any of the variables (i.e. after part 6 there is no
###       "excluding unneeded variables" part) and that I create a variable that takes value 0 if the mean years of 
###       schooling of a given country is less than average and value 1 if it is more than average. I also exclude 
###       the observations related to regions instead of countries in a more intuitive spot.


######################################################################################################################

### 1. Read the data into R:

## Read both sets of data into R:
hd <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gii <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# The data sets contain data in the human development index (hd) and the gender inequality index (gii).

######################################################################################################################

### 2. Explore the data:

# Now we can explore the dataframes a bit by using the familiar functions for looking at dimensions, structure, 
#   column names and first six rows of observations of the data.

dim(hd)
str(hd)
head(hd)
summary(hd)
colnames(hd)

# The hd dataframe has 195 observations on 8 variables. The dataframe and the first six rows appear OK and are
#   consistent with the tables on page http://hdr.undp.org/en/composite/HDI. From the summary we can see the crucial 
#   moments of the variables. The column names are quite long, and we will change them in the next part.

dim(gii)
str(gii)
head(gii)
summary(gii)
colnames(gii)

# The gii dataframe has 195 observations on 10 variables. The dataframe and the first six rows appear OK. However, they
#   are not exactly the same as on http://hdr.undp.org/en/composite/GII. Perhaps the data or the table has been updated?
#   From the summary we can see the crucial moments of the variables. The column names are quite long, and we will 
#   change them in the next part.

######################################################################################################################

### 3. Rename the variables:

# To make using the data a bit easier, we will rename the variables. 

# Let's start with the names of variables in the hd dataset: 

colnames(hd)[1] <- "HDIrank"
colnames(hd)[2] <- "Country"
colnames(hd)[3] <- "HDI"
colnames(hd)[4] <- "Life.Exp"
colnames(hd)[5] <- "Edu.Exp"
colnames(hd)[6] <- "Edu.Mean"
colnames(hd)[7] <- "GNI"
colnames(hd)[8] <- "diffGNIrankHDIrank"

# Check the new names:

colnames(hd)

# Then change the names of variables in the gii dataset:

colnames(gii)[1] <- "GIIrank"
colnames(gii)[2] <- "Country"
colnames(gii)[3] <- "GII"
colnames(gii)[4] <- "Mat.Mor"
colnames(gii)[5] <- "Ado.Birth"
colnames(gii)[6] <- "Parli.F"
colnames(gii)[7] <- "Edu2.F"
colnames(gii)[8] <- "Edu2.M"
colnames(gii)[9] <- "Labo.F"
colnames(gii)[10] <- "Labo.M"

# Check the new names:

colnames(gii)

######################################################################################################################

### 4. Create new variables:

# Next we will create two new variables from the gii dataset to account for different aspects of gender inequality. 
#   First one is the ratio of female and male populations with secondary education in each country. The second is a 
#   similar ratio for labor force participation. We will call these new variables Edu2.FM and Labo.FM respectively.

Edu2.FM <- gii$Edu2.F/gii$Edu2.M
Labo.FM <- gii$Labo.F/gii$Labo.M

# Include the new variables in the dataset:

gii$Edu2.FM <- Edu2.FM
gii$Labo.FM <- Labo.FM

# Check that everything is ok:
colnames(gii)
head(gii)

# Then we will create a new variable, that will further down the line be our dichotomous dependent variable. This 
#   variable will tell wheter a given country has higher than average mean years of schooling. We will call this 
#   variable Edu.high, and it will take value 1 if Edu.Mean is larger than average and value 0 if it is less than
#   average. 

# Start by calculating the mean of variable Edu.mean:

mean(hd$Edu.Mean)

# The result is 8.1 years.

# Next we want to define a new logical column Edu.high, which will be our new variable:

hd <- mutate(hd, Edu.high = Edu.Mean > 8.1)

# Print both Edu.Mean and Edu.high to see that everything is as it should be:

hd$Edu.Mean
hd$Edu.high

# We still want the variable to take values 0/1 instead of FALSE/TRUE. Let's use the ifelse()-function to define this.

hd$Edu.high <- ifelse(hd$Edu.high == FALSE, 0, 1)
hd$Edu.high

# Now we have created two new variables into GII data set and one new (the most important one) into HD data set.

######################################################################################################################

### 5. Join the two datasets:

# In this part we join two datasets using the variable country as the identifier.

# We will join the data sets using inner.join() function as in the DataCamp exercises. With this function we will end
#   up with a data set that contains only the countries that are included in both GII and HD data sets. 

# Choose country as the identifier to identify the countries present in both rankings:
join_by <- c("Country")

# Join the two datasets by the selected identifier:
human <- inner_join(hd, gii, by = join_by)

# Glimpse at the data to make sure that we have 195 observations and 20 variables:
glimpse(human)

# As we have country name as our identifier, it is a good idea to take a closer look at this specific variable. From
#   the print out we can see that there are also regions included. 
human$Country

# From the print out we can see that the last 7 observations in the data set refer to regions instead of countries.
#   We can remove these by defining the last observation that we want to keep in the data set as the last one referring
#   to a country and then by choosing all the observations before that to be included in the data:

# Define the last indice we want to keep:
last <- nrow(human) - 7

# Choose everything until the last 7 observations:
human <- human[1:last, ]

#Check that we removed the regions and that the last country is Niger and that we have 188 observations for 20 
#   variables:
human$Country
glimpse(human)

######################################################################################################################

### 6. Transforming GNI to numeric:

# Start by looking at the structure of the GNI column in 'human'. We can see that this variable is in string form.
str(human$GNI)

# Make it numeric by removing the commas from GNI and then converting the variable as numeric:
human$GNI <- str_replace(human$GNI, pattern=",", replace ="") 

human$GNI <- as.numeric(human$GNI)

# Glimpse at the data to make sure that the GNI is now in numeric form:
glimpse(human)

######################################################################################################################

### 7. Removing missing values:

# We can check if there are missing values in the data by taking a completeness indicator of the data:

data.frame(human[-1], comp = complete.cases(human))

# As we can see from the print out, there are a number of NA values, indicated by the value FALSE in the completeness
#   indicator. Next we want to filter out all rows that contain NA values:

human_ <- filter(human, complete.cases(human))

# From glimpsing the data, we can see that there are now 155 observations on 20 variables in our data set. 
glimpse(human_)

######################################################################################################################

### 8. Finishing touches: define country names as row names and remove the country name columm before saving the data

# Defining countries as rownames is easy:
rownames(human_) <- human_$Country

# As is removing the Country variable:
human_ <- dplyr::select(human_, -Country)

#Glimpse at the data to make sure that we have the correct number of observations and variables, 155 and 19 
#   respectively:
glimpse(human_)

# Everything as it should be! Last, let's save the data to our current working directory:

# Set the working directory to be the IODS final folder:
setwd("\\\\ATKK/home/a/awsalo/Documents/GitHub/IODS-final")

# Save the data set:
write.table(human_, file = "humanfinal.csv", sep = ",", col.names = TRUE, row.names = TRUE )

# Read the data once again to R just to make sure that it looks good:
human <- read.table(file = "humanfinal.csv", sep = ",", header = TRUE)

glimpse(human)

# Everything seems to be okay.

######################################################################################################################









