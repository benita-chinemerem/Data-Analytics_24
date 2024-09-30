#Importing the Files
epi2024results = read.csv("C:\Data-Analytics_24/epi2024results06022024.csv")
epi2024results0602
View(epi2024results06022024)

attach(epi2024results06022024)


#Here we are setting the epi2024results as a default object, 
#so i can refer to its columns directly without using the $ operator.


attach(epi2024results)

#This prints out the values in the EPI.new column of the epi2024results dataset.
EPI.new

#Here we cleaned the data and identify missing values then filter them out.
epi_cleaned = is.na(EPI.new)
E = EPI.new[!epi_cleaned]

#Here we explored the distribution


summary(EPI.new) #provides a summary of the EPI.new data

fivenum(EPI.new,na.rm=TRUE) #This gives the five-number summary excluding NA values

stem(EPI.new) # #This creates a stem and leaf plot to visualize the data distribution

hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) #This creates a histogram with breaks from 20 to 80 in increments of 1 and scales the y-axis to show probabilities

lines(density(EPI.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ” this adds a density plot
rug(EPI.new) #This adds a rug plot


# Let's create a sequence of numbers from 20 to 80 with an increment of 1
x <- seq(20, 80, 1)

# Calculate the density of the normal distribution for each value in x
# with a mean of 42 and a standard deviation of 5
q <- dnorm(x, mean=42, sd=5, log=FALSE)

# Add a line plot of the normal distribution to the current plot
lines(x, q)

# Add another line plot, scaling the values in q by 0.4
# This reduces the height of the normal distribution curve to 40% of its original height
lines(x, .4 * q)

# Calculate the density of another normal distribution for each value in x
# with a mean of 65 and a standard deviation of 5
ln <- dnorm(x, mean=65, sd=5, log=FALSE)

# Add another line plot, scaling the values in q by 0.12
# This reduces the height of this normal distribution curve to 12% of its original height
lines(x, .12 * q)

#fitting the distribution beyond histograms for EPI.new
plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE)
qqnorm(EPI.new); qqline(EPI.new)
qqplot(rnorm(ppoints(250), df= 5), EPI.new, xlab= "Q-Q plot for norm dsn")
qqline(EPI.new)

#Here we explored another column APO.new just the exact way we explored EPI.new
APO.new
# I did not bother checking for missing value for APO.new because there is no missing values

summary(APO.new)
fivenum(APO.new,na.rm=TRUE)
stem(APO.new) # stem and leaf plot
hist(APO.new)
hist(APO.new, seq(20., 80., 1.0), prob=TRUE) #this particular code gave me error 'breaks do not span range of X'
range(APO.new, na.rm=TRUE) # I added this code to give me the minimum and maximum values in APO.new.
hist(APO.new, seq(7., 100., 1.0), prob=TRUE) # I use the value range i got from the previous code and it worked
lines(density(APO.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(APO.new) #This adds a rug plot

#fitting the distribution beyond histograms for APO.new
plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE)
qqnorm(APO.new); qqline(APO.new)
qqplot(rnorm(ppoints(250), df= 5), APO.new, xlab= "Q-Q plot for norm dsn")
qqline(APO.new)

#Okay let's compare the distributions
boxplot(EPI.new, APO.new)


# Let's create a sequence of numbers from 20 to 80 with an increment of 1
x <- seq(20, 80, 1)

# Calculate the density of the normal distribution for each value in x
# with a mean of 42 and a standard deviation of 5
q <- dnorm(x, mean=42, sd=5, log=FALSE)

# Add a line plot of the normal distribution to the current plot
lines(x, q)

# Add another line plot, scaling the values in q by 0.4
# This reduces the height of the normal distribution curve to 40% of its original height
lines(x, .4 * q)

# Calculate the density of another normal distribution for each value in x
# with a mean of 65 and a standard deviation of 5
ln <- dnorm(x, mean=65, sd=5, log=FALSE)

# Add another line plot, scaling the values in q by 0.12
# This reduces the height of this normal distribution curve to 12% of its original height
lines(x, .12 * q)


#Here we explored another column WRS.new just the exact way we explored EPI.new
WRS.new
# I did not bother checking for missing value for WRS.new because there is no missing values

summary(WRS.new)
fivenum(WRS.new,na.rm=TRUE)
stem(WRS.new) # stem and leaf plot
hist(WRS.new)
hist(WRS.new, seq(20., 80., 1.0), prob=TRUE) #this particular code gave me error 'breaks do not span range of X'
range(WRS.new, na.rm=TRUE) # I added this code to give me the minimum and maximum values in APO.new.
hist(WRS.new, seq(7., 100., 1.0), prob=TRUE) # I use the value range i got from the previous code and it worked
lines(density(WRS.new,na.rm=TRUE,bw=1.)) # or try bw=“SJ”
rug(WRS.new) #This adds a rug plot

#fitting the distribution beyond histograms for WRS.new
#these commands are used to visualize and compare the distribution of WRS.new
#to a normal distribution using ECDF and Q-Q plots
plot(ecdf(WRS.new), do.points=FALSE, verticals=TRUE)
qqnorm(WRS.new); qqline(WRS.new)
qqplot(rnorm(ppoints(250), df= 5), WRS.new, xlab= "Q-Q plot for norm dsn")
qqline(WRS.new)





###Lab 02 Exercise 02

#Read in data population_2023, epi.results, epi.weights

population_2023 <- countries_populations_2023


epi.result <- epi2024results06022024


epi.weights <- epi2024weights



# Drop countries not in epi results
populations <- population_2023[-which(!population_2023$Country %in% epi.result$country),]


## sort populations by country
populations <- populations[order(populations$Country),]

#drop countries not in populations
epi.result.sub <- epi.result[-which(!epi.result$country %in% populations$Country),]

### sort epi results by country
epi.result.sub <- epi.result.sub[order(epi.result.sub$country),]

##only keep necessary columns
epi.result.sub <- epi.result.sub[,c("country","EPI.old","EPI.new")]

##convert population to numeric
epi.result.sub$population <- as.numeric(populations$Population)

#compute population log base 10
epi.result.sub$population_log <- log10(epi.result.sub$population)


##Linear Model
attach(epi.result.sub)
#EPI.new <- epi.result$EPI.new
#population_log <- epi.result.sub$population_log
#range(population_log)
#range(EPI.new)

lin.mod.epinew <- lm(EPI.new~population_log,epi.result.sub)
plot(EPI.new,population_log)
abline(lin.mod.epinew)
summary(lin.mod.epinew)
plot(lin.mod.epinew)


##ggplot
#I installed  the ggpplot2 package then add it
install.packages("ggplot2")
library(ggplot2)
ggplot(epi.result.sub, aes(x = population_log, y=EPI.new)) +
  geom_point() +
  stat_smooth(method ="lm")

ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

