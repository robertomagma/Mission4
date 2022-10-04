# Mission #4 - Supplementary R Code
# September 27, 2022

library('mosaic')

################### Age

range(bodyPerformance$age) # 21 to 64 years old
mean(bodyPerformance$age) # 36.78 years old
median(bodyPerformance$age) # 32 years old
sd(bodyPerformance$age) # 13.63 years old
var(bodyPerformance$age) # 185.66 years old

# Transform the participant's age into months
# Each year has 12 months
bodyPerformance$age_months = bodyPerformance$age * 12
bodyPerformance$age_months

# Transform the participant's age into days
# Each year has 365.25 days
bodyPerformance$age_days = bodyPerformance$age * 365.25
bodyPerformance$age_days

# Transform the participant's age into hours
# Each day has 24 hours
bodyPerformance$age_hours = bodyPerformance$age * 365.25 * 24
bodyPerformance$age_hours

# Transform the participant's age into minutes
# Each hour has 60 minutes
bodyPerformance$age_min = bodyPerformance$age * 365.25 * 24 * 60
bodyPerformance$age_min

# Transform the participant's age into seconds
# Each minute has 60 seconds
bodyPerformance$age_sec = bodyPerformance$age * 365.25 * 24 * 60 * 60
bodyPerformance$age_sec

# Convert the participant's age in seconds into years
# There are 365.25 * 24 * 60 * 60 = 31557600 seconds in a year
bodyPerformance$age_sec / 31557600

# Transform the broad.jump_cm into inches
# 1 cm = 0.3937007874 inches

# Calculate the Sum of Squared Deviations from the Mean for age
sum((bodyPerformance$age - mean(bodyPerformance$age))^2) # SS = 2486333
# Calculate the sample size for bodyPerformance
length(bodyPerformance$age) # n = 13393 observations
# Calculate the population standard deviation for age
2486333/13393 # Population Variance = 185.6442
# Calculate the average age
mean(bodyPerformance$age) # Mean = 36.77511

sd(bodyPerformance$height_cm)
sd(bodyPerformance$weight_kg)
cov(bodyPerformance$height_cm, bodyPerformance$weight_kg)
74.00158/(8.426583*11.94967)
cor(bodyPerformance$height_cm, bodyPerformance$weight_kg)

# What is the z-score for a participant who is 36.77511 years old?
(36.77511 - 36.77511)/

boxplot_age <- boxplot(bodyPerformance$age)
boxplot_age$out # No outliers
boxplot_age$stats # The 25th percentile is 25 years old, and the 75th percentile is 48 years old

hist(bodyPerformance$age,
     main="Distribution of Age",
     xlab="Age (Years)",
     xlim=c(20,70),
     breaks=20,
     col="lightgreen",
     freq=FALSE
)
abline(v = mean(bodyPerformance$age),                       # Add line for mean
       col = "red",
       lwd = 3)
# Add text to the histogram
text(x = mean(bodyPerformance$age) + 15,                    # Add text for mean
     y = 0.04,
     paste("Mean Age =", round(mean(bodyPerformance$age),2), "years old"),
     col = "red",
     cex = 1)

################### Height

boxplot_height <- boxplot(bodyPerformance$height)
boxplot_height$out # Outliers: 139.8 143.4 141.0 193.8 143.6 139.9 139.5 125.0 140.5 143.7

hist(bodyPerformance$height_cm,
     main="Distribution of Height",
     xlab="Height (cm)",
     # xlim=c(50,100),
     breaks=20,
     col="darkmagenta",
     freq=FALSE
)
abline(v = mean(bodyPerformance$height_cm),                       # Add line for mean
       col = "red",
       lwd = 3)
text(x = mean(bodyPerformance$height_cm) - 25,                    # Add text for mean
     y = 0.04,
     paste("Mean Height =", round(mean(bodyPerformance$height_cm), 2), "cm"),
     col = "red",
     cex = 1)

################### Weight

hist(bodyPerformance$weight_kg,
     main="Distribution of Body Weight",
     xlab="Weight (kg)",
     # xlim=c(50,100),
     breaks=20,
     col="lightcyan",
     freq=FALSE
)
abline(v = mean(bodyPerformance$weight_kg),                       # Add line for mean
       col = "red",
       lwd = 3)
# Add text to the histogram
text(x = mean(bodyPerformance$weight_kg) + 45,                    # Add text for mean
     y = 0.02,
     paste("Mean =", round(mean(bodyPerformance$weight_kg),2), "kg"),
     col = "red",
     cex = 1)

################### Body Fat

hist(bodyPerformance$body.fat_.,
     main="Distribution of Body Fat",
     xlab="Body Fat (kg)",
     # xlim=c(50,100),
     breaks=20,
     col="lightgreen",
     freq=FALSE
)
abline(v = mean(bodyPerformance$body.fat_.),                       # Add line for mean
       col = "red",
       lwd = 3)
# Add text to the histogram
text(x = mean(bodyPerformance$body.fat_.) + 30,                    # Add text for mean
     y = 0.04,
     paste("Mean Age =", round(mean(bodyPerformance$body.fat_.),2), "years old"),
     col = "red",
     cex = 1)

################### Diastolic Blood Pressure

hist(bodyPerformance$diastolic,
     main="Distribution of Diastolic Blood Pressure",
     xlab="Diastolic Blood Pressure (mmHg)",
     # xlim=c(50,100),
     breaks=20,
     col="lightcyan",
     freq=FALSE
)
abline(v = mean(bodyPerformance$diastolic),
       col = "red",
       lwd = 3)
text(x = mean(bodyPerformance$diastolic) + 50,
     y = 0.02,
     paste("Mean =", round(mean(bodyPerformance$diastolic),2), "mmHg"),
     col = "red",
     cex = 1)

################### Systolic Blood Pressure

hist(bodyPerformance$systolic,
     main="Distribution of Systolic Blood Pressure",
     xlab="Diastolic Blood Pressure (mmHg)",
     # xlim=c(50,100),
     breaks=20,
     col="lightcyan",
     freq=FALSE
)
abline(v = mean(bodyPerformance$systolic),                       # Add line for mean
       col = "red",
       lwd = 3)
# Add text to the histogram
text(x = mean(bodyPerformance$systolic) - 65,                    # Add text for mean
     y = 0.02,
     paste("Mean =", round(mean(bodyPerformance$systolic),2), "mmHg"),
     col = "red",
     cex = 1)

################### Sit and Bend Forward

# https://www.datamentor.io/r-programming/histogram/
hist(bodyPerformance$sit.and.bend.forward_cm,
     main="Distribution of Sit and Bend Forward Distances",
     xlab="Sit and Bend Forwards (cm)",
     xlim=c(-40,60),
     breaks=20,
     col="lightcyan",
     freq=FALSE
)
abline(v = mean(bodyPerformance$sit.and.bend.forward_cm),                       # Add line for mean
       col = "red",
       lwd = 3)
# Add text to the histogram
# URL: https://statisticsglobe.com/text-function-plot-r/
text(x = mean(bodyPerformance$sit.and.bend.forward_cm) - 30,                    # Add text for mean
     y = 0.03,
     paste("Mean =", round(mean(bodyPerformance$sit.and.bend.forward_cm),2), "times"),
     col = "red",
     cex = 1)

################### Sit Up Counts

# https://www.datamentor.io/r-programming/histogram/
hist(bodyPerformance$sit.ups.counts,
     main="Distribution of Sit Ups",
     xlab="Sit Ups (count)",
     # xlim=c(50,100),
     breaks=20,
     col="lightgreen",
     freq=FALSE
)
abline(v = mean(bodyPerformance$sit.ups.counts),                       # Add line for mean
       col = "red",
       lwd = 3)
# Add text to the histogram
text(x = mean(bodyPerformance$sit.ups.counts) - 25,                    # Add text for mean
     y = 0.025,
     paste("Mean =", round(mean(bodyPerformance$sit.ups.counts),2), "times"),
     col = "red",
     cex = 1)

################### Broad Jump Distance

# https://www.datamentor.io/r-programming/histogram/
hist(bodyPerformance$broad.jump_cm,
     main="Distribution of Broad Jump Distances",
     xlab="Broad Jump Distance (cm)",
     xlim=c(0,350),
     breaks=20,
     col="lightyellow",
     freq=FALSE
)
abline(v = mean(bodyPerformance$broad.jump_cm),                       # Add line for mean
       col = "red",
       lwd = 3)
# Add text to the histogram
text(x = mean(bodyPerformance$broad.jump_cm) - 115,                    # Add text for mean
     y = 0.007,
     paste("Mean Distance =", round(mean(bodyPerformance$broad.jump_cm),2), "cm"),
     col = "red",
     cex = 1)

################### Correlation Analyses

# The relationship between age and the number of sit ups
# URL: https://r-coder.com/correlation-plot-r/
plot(x = bodyPerformance$age, y = bodyPerformance$sit.ups.counts, pch = 19, col = "lightblue")
abline(lm(bodyPerformance$sit.ups.counts ~ bodyPerformance$age), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(x = bodyPerformance$age, y = bodyPerformance$sit.ups.counts), 2)), x = 50, y = 75)

# The relationship between age and grip force
# URL: https://r-coder.com/correlation-plot-r/
plot(x = bodyPerformance$age, y = bodyPerformance$gripForce, pch = 19, col = "lightblue")
abline(lm(bodyPerformance$gripForce ~ bodyPerformance$age), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(x = bodyPerformance$age, y = bodyPerformance$gripForce), 2)), x = 50, y = 67)

# The relationship between height_cm and weight_kg
plot(x = bodyPerformance$height_cm,
     y = bodyPerformance$weight_kg,
     pch = 19,
     col = "lightblue")
# abline(lm(bodyPerformance$weight_kg ~ bodyPerformance$height_cm), col = "red", lwd = 3)
# text(paste("Correlation:", round(cor(x = bodyPerformance$height_cm, y = bodyPerformance$weight_kg), 2)), x = 150, y = 120)

sd(bodyPerformance$height_cm) # 8.426583 cm
var(bodyPerformance$height_cm) # 71.00729 cm^2
sd(bodyPerformance$weight_kg) # 11.94967 kg
var(bodyPerformance$weight_kg) # 142.7945 kg^2
cov(x = bodyPerformance$height_cm, y = bodyPerformance$weight_kg) # 74.00158

# The relationship between height_cm and weight_kg
plot(x = bodyPerformance$body.fat_.,
     y = bodyPerformance$gripForce,
     pch = 19,
     col = "lightblue")
mean(bodyPerformance$gripForce) # 36.96388
mean(bodyPerformance$body.fat_.) # 23.24016
cov(x = bodyPerformance$body.fat_., y = bodyPerformance$gripForce) # -41.77348

# The relationship between body fat and weight_kg
plot(x = bodyPerformance$weight_kg,
     y = bodyPerformance$body.fat_.,
     pch = 19,
     col = "lightblue")
abline(lm(bodyPerformance$body.fat_. ~ bodyPerformance$weight_kg), col = "red", lwd = 3)
text(paste("Correlation:", round(cor(x = bodyPerformance$weight_kg, y = bodyPerformance$body.fat_.), 2)), x = 50, y = 70)

plot(x = bodyPerformance$sit.ups.counts,
     y = bodyPerformance$broad.jump_cm,
     pch = 19,
     col = "lightblue")

cov(x = bodyPerformance$sit.ups.counts, y = bodyPerformance$broad.jump_cm) # 425.9045

sd(bodyPerformance$sit.ups.counts) # 14.2767
sd(bodyPerformance$broad.jump_cm) # 39.868

################### Z-score calculations

# Calculate the Sum of Squared Deviations from the Mean for height_cm
sum((bodyPerformance$height_cm - mean(bodyPerformance$height_cm))^2) # SS = 950929.7
# Calculate the average height_cm
mean(bodyPerformance$height_cm) # Mean = 168.5598
# Calculate the sample size for bodyPerformance
length(bodyPerformance$height_cm) # n = 13393 observations

950929.7/13393 # Population Variance = 71.0019935787
sqrt(950929.7/13393) # Population Standard Deviation = 8.426268

# What is the z-score for somebody who was 185 cm?
(185 - 168.5598)/(8.426268) # z-score = 1.951065

# Plot the z-score on a standardized normal distribution curve
install.package(ggplot2)
library(ggplot2)
z = -1.0158471105
ggplot(NULL, aes(c(-3,3))) +
  geom_area(stat = "function", fun = dnorm, fill = "#00998a", xlim = c(-3, z)) +
  geom_area(stat = "function", fun = dnorm, fill = "grey80", xlim = c(3, z)) +
  labs(x = "z", y = "") +
  scale_y_continuous(breaks = NULL) +
  scale_x_continuous(breaks = z)
# Convert the z-score into a percentile
pnorm(z) # 0.9744753

cor(x = bodyPerformance$sit.ups.counts, y = bodyPerformance$broad.jump_cm) # 0.7482728