# Loading the Excel File
library(XLConnect)
Data <- readWorksheet(loadWorkbook("C:/Users/anishjoshi500/Downloads/Data Analysis Homework/SaltConc.xlsx"),sheet=1)

# Calculating mean, median, standard deviation, maximum and minimum values
All <- function(x) {
  c(mean = mean(x), median = median(x), sd = sd(x), maximum = max(x), minimum = min(x))
}

# Function call to calculate the above values for the salt and area variables respectively
All(Data$Salt)
All(Data$Area)

# Boxplot
boxplot(Data, main="Box Plot For Salt and Area", xlab="X", ylab="Y")

# Histogram
hist(Data$Area, main="Area Distribution", xlab="Area")

# Scatterplot
plot(Data$Salt, Data$Area, xlab="Roadway Area", ylab="Salt Concentration")  

# t-test
t.test(Data$Salt, alternative = "two.sided", mu = 20, conf.level = 0.90)

# Sum of all values in the second observation
SecObservation <- function(x) {
  sum <- rowSums(x[2,] )
  sum
}

# Function call to calculate the sum of all values in the second observation for the salt concentration dataset
SecObservation(Data)
