#'---
#'title: "PS 12"
#'author: "Aditya Sanjay Mhaske"
#'---
#'
#'Question 1
#'
#'
#' Question 1.a.)
#' 
#' Null hyothesis = Ho : obsereverd proportion = expected proportions
#' Alternate hypothesis = H1 : obsereverd proportion = expected proportions
obs = c(121, 84, 118, 226, 226, 123)
n = sum(obs)
exp = n * c(0.13, 0.14, 0.13, 0.24, 0.20, 0.16) 
df = (6-1) - 0
g2 = 2*sum(obs * log(obs/exp))
g_pval = 1 - pchisq(g2,df)
g_pval
#' Since the p value is less than significance level, we can reject the null hypothesis
#' 
#' Question 1.b)
library(infer)
library(tidyverse)
x2 = sum((obs - exp)^2/exp)
x_pval = 1 - pchisq(x2,df)
vec = rep(as.character(1:6), obs)
df = data.frame(vec)
null_dist <- df |>
  specify(response = vec) |>
  hypothesize(null = "point", p = c("1" = 0.13,
                                    "2" = 0.14,
                                    "3" = 0.13,
                                    "4" = 0.24,
                                    "5" = 0.20,
                                    "6" = 0.16)) |>
  generate(reps = 2500, type = "draw") |>
  calculate(stat = "Chisq")

null_dist |>
  get_p_value(obs_stat = x2, direction = "greater")
#' Since the p value is less than significance level, we can reject the null hypothesis
#' 
#' Question 1.c) From the expected
obs
exp
#' From the expected, we can see that the the observed yellow candies are much less than expected and the observed orange candies are much more than expected. So we increase the proportion of orange candies and decrease the expected proportion of yellow candies.
exp = n * c(0.13, 0.11, 0.13, 0.24, 0.23, 0.16) 
df = (6-1) - 0
g2 = 2*sum(obs * log(obs/exp))
g_pval = 1 - pchisq(g2,df)
g_pval
#' Now we fail to reject the null, and hence change the conclusion of previous test
#' 
#' 
#' 
#' 
#' 
#' 
#' Question 2
#' 
#' 
#' Question 2.a.)
n = 16
p = 0.29
exp = c(sum(dbinom(0:1,n,p)), dbinom(2:8,n,p), sum(dbinom(9:16,n,p))) * 1000
exp
obs = c(30, 93, 159, 184, 195, 171, 92, 45, 31)
obs
df = (9-1)-0
g2 = 2*sum(obs * log(obs/exp))
g_pval = 1 - pchisq(g2,df)
g_pval
#' We fail to reject the null, i.e., the algorithm is not working as intended.
#' 
#' Question 2.b)
x2 = sum((obs - exp)^2/exp)
x_pval = 1 - pchisq(x2,df)
x_pval

color.vec = rep(as.character(1:9),obs)
df = data.frame(color.vec)

null_dist <- df |>
  specify(response = color.vec) |>
  hypothesize(null = "point", p = c("1" = sum(dbinom(0:1,n,p)),
                                    "2" = dbinom(2,n,p),
                                    "3" = dbinom(3,n,p),
                                    "4" = dbinom(4,n,p),
                                    "5" = dbinom(5,n,p),
                                    "6" = dbinom(6,n,p),
                                    "7" = dbinom(7,n,p),
                                    "8" = dbinom(8,n,p),
                                    "9" = sum(dbinom(9:16,n,p)))) |>
  generate(reps = 2500, type = "draw") |>
  calculate(stat = "Chisq")
null_dist |>
  get_p_value(obs_stat = x2, direction = "greater")
#' We fail to reject the null, i.e., the algorithm is not working as intended.
#' 
#' Question 2.c)
mu = sum(obs*c(1:9)) / 16000
mu
#'
#'
#' No, there is no change in degree of freedom as there is no change on the constraints on variables.

  
#' 
#' 
#' Question 3
#' 
#' 
#' Question 3.)
#' 
#' Null hyothesis = histological type and response to treatment are independent
#' Alternate hypothesis = histological type and response to treatment are dependent
#' 
#' 
#' Question 3.a)
obs = matrix(c(74, 18, 12, 68, 16, 12, 154, 54, 58, 18, 10, 44), nrow = 4, ncol = 3)
obs
exp = rowSums(obs)%o%colSums(obs)/sum(obs)
exp
df = (4 - 1)*(3 - 1)
g2 = sum(2*obs*log(obs/exp))
g2
1 - pchisq(g2, df)
#' 
#' 
#' We can reject the null. Hence, histological type and response to treatment are dependent
#' 
#' 
#' Question 3.b)
x2 = sum((obs - exp)^2/exp)
rownames(obs) <- c("LP", "NS", "MC", "LD")
colnames(obs) <- c("Positive", "Partial", "None")
obs
df.obs = as.data.frame(obs)
data2 <- df.obs |>
  rownames_to_column("histologicalType") |>            
  pivot_longer(cols=c('Positive', 'Partial', 'None'),
               names_to='response',
               values_to='count') |>
  rowwise() |> 
  mutate(count = list(1:count)) |>  
  unnest(count) |>                  
  select(-count)



null_dist <- data2 |>
  specify(response ~ histologicalType) |>
  hypothesize(null = "independence") |>
  generate(reps = 2500, type = "permute") |>
  calculate(stat = "Chisq")

null_dist |>
  get_p_value(obs_stat = x2, direction = "greater")
#' 
#' 
#' We can reject the null. Hence, histological type and response to treatment are dependent
