library(ggplot2)


View(epi2024results_DA_F24_lab03)

lab_region <- read.csv("~/Desktop/lAB03_TEST/epi2024results_DA_F24_lab03.csv")

newRegion <- epi2024results_DA_F24_lab03

#We print the data for the variable MPE & EPI
MPE_region <-newRegion$MPE
EPI_region <- newRegion$EPI


EPI_region_N <- as.numeric(EPI_region)
MPE_region_N <- as.numeric(MPE_region)

#We set True values for NA
is.na(EPI_region_N)
is.na(MPE_region_N)

#We filter out NA values from the row MPE.new
EPI_region <- EPI_region_N[!is.na(EPI_region_N)]
MPE_region <- MPE_region_N[!is.na(MPE_region_N)]

MPE

EPI_region <- as.numeric(EPI_region)
MPE_region <- as.numeric(MPE_region)


# Create subsets for two regions
global_west <- filter(newRegion, newRegion$region == "Global West")
asia_pacific <- filter(newRegion, newRegion$region == "Asia-Pacific")

View(global_west)

# Histogram with density line for MPE_region in Global West
ggplot(global_west, aes(x = MPE)) +
  geom_histogram(aes(y = ..density..), binwidth = 4, fill = "green", color = "black") +
  geom_density(color = "red", size = 2) +
  labs(title = "Histogram of MPE for Global West",
       x = "Marine Protection Stringency (MPE)",
       y = "Density") +
  theme_minimal()


# Histogram with density line for MPE in Asia-Pacific
ggplot(asia_pacific, aes(x = MPE)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "yellow", color = "black") +
  geom_density(color = "blue", size = 1) +
  labs(title = "Histogram of MPE for Asia-Pacific",
       x = "Marine Protection Stringency (MPE)",
       y = "Density") +
  theme_minimal()


# QQ plot for Global West MPE compared to normal distribution

ggplot(global_west, aes(sample = MPE)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of MPE for Global West",
       x = "Theoretical Quantiles",
       y = "MPE Quantiles") +
  theme_minimal()

# QQ plot for Asia-Pacific EPI compared to normal distribution
ggplot(asia_pacific, aes(sample = EPI)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "QQ Plot of EPI for Asia-Pacific",
       x = "Theoretical Quantiles",
       y = "EPI Quantiles") +
  theme_minimal()


hist(MPE)
lines(density(MPE, na.rm = TRUE, bw=1.))

mean <- mean(newRegion$MPE, na.rm = TRUE)

sd <- sd(newRegion$MPE, na.rm = TRUE)


MPE_data <- 

#Make a Q-Q plot against the generating distribution
qqplot(rnorm(180),MPE, xlab = "Q-Q plot for norm dsn")
qqline(MPE)


MPE_data <- 
  
  #Make a Q-Q plot against the generating distribution
  qqplot(rnorm(180),EPI, xlab = "Q-Q plot for norm dsn")
qqline(EPI)



##Exercise 2.1 - Linear Models

### Choose a subset of 5 variables (excluding EPI) and using the formula 
### EPI~VAR1+VAR2+VAR3+VAR4+VAR5, fit a linear model and identify 
### which variable most significantly influences EPI. Plot that variable
### with another and overlay the fitted line.

##drop NA values in row 

newRegion_Clean <- na.omit(newRegion)

##Clean data for ECO,BDH, MPE, PFL, APO

ECO <- epi2024results_DA_F24_lab03$ECO
ECO <- ECO[!is.na(ECO)]

BDH <- epi2024results_DA_F24_lab03$BDH
BDH <- BDH[!is.na(BDH)]

PFL <- epi2024results_DA_F24_lab03$PFL
PFL <- epi2024results_DA_F24_lab03$PFL

APO <- epi2024results_DA_F24_lab03$APO
APO <- APO[!is.na(APO)]

# Fit the linear model
new_model <- lm(EPI ~ ECO + BDH + MPE + PFL + APO, newRegion_Clean)
plot(EPI~ ECO + BDH + MPE + APO)
abline(new_model)

# Summarize the model
summary(new_model)
plot(new_model)


# Identify the most significant variable
new_model_coef <- summary(new_model)$coefficients
significant_var <- rownames(new_model_coef)[which.min(new_model_coef[, "Pr(>|t|)"])]


# Create a scatter plot with the most significant variable and EPI
ggplot(newRegion_Clean, aes(x = !!sym(significant_var), y = EPI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = paste("EPI vs", significant_var),
       x = significant_var,
       y = "EPI") +
  theme_minimal()


##Linear Model 2.2

#Subset data for the region "Sub-Saharan Africa"

new_data_Sub_Saharan_Africa <- newRegion[newRegion$region == "Sub-Saharan Africa",]
new_data_Asia_Pacific <- newRegion[newRegion$region == "Asia-Pacific",]

ECO_tes <- epi2024results_DA_F24_lab03$ECO
BDH_tes <- epi2024results_DA_F24_lab03$BDH
MPE_tes <- epi2024results_DA_F24_lab03$MPE
PFL_tes <- epi2024results_DA_F24_lab03$PFL
APO_tes <- epi2024results_DA_F24_lab03$APO

# Fit the linear model for Global West
Sub_Saharan_Africa_model <- lm(EPI ~ ECO + BDH + MPE + PFL + APO, new_data_Sub_Saharan_Africa)
Asia_Pacific_model <- lm(EPI ~ ECO + BDH + MPE + PFL + APO, new_data_Asia_Pacific)

# Summarize the Global West model
summary(Sub_Saharan_Africa_model)
plot(Sub_Saharan_Africa_model)

summary(Asia_Pacific_model)
plot(Asia_Pacific_model)


# Compare R-squared values
new_global_rsqaured <- summary(new_model)$r.squared
Sub_Saharan_Africa_rsqaured <- summary(Sub_Saharan_Africa_model)$r.squared
Asia_Pacific_rsquared <- summary(Asia_Pacific_model)$r.squared

# Identify the most significant variable for each model
new_global_coef <- summary(new_model)$coefficients
new_global_most_significant_var <- rownames(new_global_coef)[which.min(new_global_coef[, "Pr(>|t|)"])]


new_Sub_Saharan_Africa_coef_summary <- summary(Sub_Saharan_Africa_model)$coefficients
Sub_Saharan_Africa_most_significant_var <- rownames(new_Sub_Saharan_Africa_coef_summary)[which.min(new_Sub_Saharan_Africa_coef_summary[, "Pr(>|t|)"])]

new_Asia_Pacific_coef_summary <- summary(Asia_Pacific_model)$coefficients
Asia_Pacific_most_significant_var <- rownames(new_Asia_Pacific_coef_summary)[which.min(new_Asia_Pacific_coef_summary[, "Pr(>|t|)"])]

View(Sub_Saharan_Africa_most_significant_var)
View(new_global_most_significant_var)

# Create scatter plots for both models
global_plot <- ggplot(newRegion_Clean, aes(x = !!sym(new_global_most_significant_var), y = EPI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  labs(title = paste("Global: EPI vs", new_global_most_significant_var),
       x = new_global_most_significant_var,
       y = "EPI") +
  theme_minimal()



##Asia_Pacific_plot
Asia_pacific_plot <- ggplot(new_data_Asia_Pacific, aes(x = !!sym(Asia_Pacific_most_significant_var), y = EPI)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = paste("Asia-Pacific: EPI vs", Asia_Pacific_most_significant_var),
       x = Asia_Pacific_most_significant_var,
       y = "EPI") +
  theme_minimal()


print(global_plot)
print(Asia_pacific_plot)

##Based on the R-squared value, Asia_pacific (0.8205256521581928) has a better fit compared to the
##global model (0.675242525361669) because it better explains 82% of the variance in EPI within
##its region compared to the global model's 67% across all regions. Although using just R-squared
##as the only criterion is limiting in itself.



#Classification (kNN)

library(class)
library(caret)
library(dplyr)

new_epi_region <- read.csv("C:/Data-Analytics_24/Lab03/epi2024results_DA_F24_lab03.csv")


attach(new_epi_region)

View(new_epi_region$region)
print(new_epi_region$region)

region <-new_epi_region$region

# Select subset of variables and regions
selected_variables <- c('ECO', 'BDH', 'PFL', 'APO', 'MPE')
selected_regions <- c('Eastern Europe', 'Sub-Saharan Africa', 'Greater Middle East')

#Filter data
filtered_new_epi_region <- new_epi_region[new_epi_region$region %in% selected_regions, c(selected_variables, 'region')]


### Handle missing values
for(var in selected_variables) {
  filtered_new_epi_region[is.na(filtered_new_epi_region[,var]), var] <- mean(filtered_new_epi_region[,var], na.rm = TRUE)
}

# Prepare the data for modeling
x <- filtered_new_epi_region[, selected_variables]
class(x)
y <- filtered_new_epi_region$region


# Split the data into training and testing sets
set.seed(42)
train_index <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- x[train_index, ]
X_test <- x[-train_index, ]
y_train <- y[train_index]
y_test <- y[-train_index]

