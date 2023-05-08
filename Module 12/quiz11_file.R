#' Quiz 11
#' 
#' This file contains the code used for your problem set. 
#' Modify this code and add the necessary code
#' to answer the quiz question
library(fivethirtyeight)
library(tidyverse)
library(infer)

set.seed(520) 
df_pass <- bechdel |>
  na.omit() |>
  filter(binary == "PASS") |>
  slice_sample(n = 60) |>
  mutate(profit = domgross - budget)
df_fail <- bechdel |>
  na.omit() |>
  filter(binary == "FAIL") |>
  slice_sample(n = 72)|>
  mutate(profit = domgross - budget)

df_final <- rbind(df_pass, df_fail)

#' Null Hypothesis = Ho: delta >= 0 
#' Alternate Hypothesis =H1:  delta < 0
#' where delta is difference of mean of profit between the movies that pass the test and the movies that fail the test.

pass_profit = df_pass$profit
fail_profit = df_fail$profit
n1 = length(pass_profit)
n2 = length(fail_profit)
mu1 = mean(pass_profit)
mu2 = mean(fail_profit)
std1 = sd(pass_profit)
std2 = sd(fail_profit)
v1 = std1^2
v2 = std2^2
delta = mu1 - mu2
se = sqrt(((std1^2)/n1)+((std2^2)/n2))
t = delta / se
t
df = ((v1/n1 + v2/n2)^2) / ((v1/n1)^2 /(n1-1) + (v2/n2)^2 /(n2-1))
df
p = pt(-abs(t),df)
p
#t.test(pass_profit, fail_profit ,conf.level = 0.07)
#'Since pval> alpha, we fail to reject the null