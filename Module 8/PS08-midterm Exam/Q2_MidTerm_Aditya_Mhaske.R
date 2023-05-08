#' Aditya Sanjay Mhaske
#' 
#' Questiion 2 
#' 
#' 
#' #' 
#' ---------------------------------------------------------------------------------------
#' 
#' 
#' A) Calculate the plug-in estimates of the mean, median, variance, and interquartile range.
#' 

library(ggplot2)

data(ChickWeight)

set.seed(100)
x <- sample(ChickWeight$weight, size = 60, replace = TRUE)

#' plugin estimates
#' 
n=length(x)
n
fx = rep(1,n) / n
fx

EX = sum(x * fx)
EX
mean(x)
median(x)
VarX = sum ((x - EX)^2 * fx)
VarX
IQR(x)

#' 
#' 
#' ---------------------------------------------------------------------------------------
#' 
#' 

#' B) Do you think that the sample was drawn from a normal distribution? 
#' Justify your answer. 


qqnorm(x, main = "Normal Distribution of Sample Weight")
qqline(x)

plot(density(x))

#' 
#' Based on the qqplot and the kernel density plot, we can see that the 
#' sample isn't drawn from a normal distribution

#' 
#' 
#' 
#' ---------------------------------------------------------------------------------------
#' 
#' 
#' C) Now consider the transformed sample produced by replacing each value with its natural logarithm. 
#' Do you think that the transformed sample was drawn from a normal distribution? Justify your answer.
#' 
#' 
# Transformed Weight
transformed_sample <- log(x)

plot(density(transformed_sample)) #Kernel Density Plot

#qqplot
qqnorm(transformed_sample, main = "Normal Distribution of Transformed Weight")
qqline(transformed_sample)


#' 
#' From the Kernel Density Plot and the qqplot, we can illustrate that the transformed sample weight distribution
#' is more symmetric than part (C), this is why we can say that the transformed sample was drawn from a normal distribution.

#' 
#' ---------------------------------------------------------------------------------------
#' 
#' D. Calculate the test statistic
x_bar <- 116
s <- 70
n <- 60
se <- s/sqrt(n)
t <- (x - 150)/se
t
#' 
#' The value of the test statistic is -2.595893, indicating the number of standard errors by which x differs from the assumed mean of 150. 
#' To determine the p-value for this test statistic, we can utilize the pt() function available in R.
#' 
#' 
p_value <- pt(t, df = n-1, lower.tail = TRUE)
p_value

#' The p-value is 0.005772819

#' As the obtained p-value is below the typical significance threshold of 0.05, 
#' we have sufficient evidence to reject the null hypothesis and understand that the mean weight is possibly less than 150 grams. 
#' Therefore, the claim that the mean weight of the chicks in the entire population is no less 
#' than 150 grams is not supported by the available data.
#' 
#' 
#' ---------------------------------------------------------------------------------------
#' 
