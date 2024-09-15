#Importing the Files
epi2024results = read.csv("C:/Users/Benita chinemerem/Downloads/epi2024results06022024.csv")
View(epi2024results)


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
