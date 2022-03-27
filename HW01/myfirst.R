#create 100 random repeatable number
num <- sample(x = 1:100, size = 100, replace = TRUE)
num
# compute mean, variance, standard deviation
mean(num)
var(num)
sd(num)
# replace 20 number by not a number (NaN)
y <- num
y[sample(x = 1:100, size = 20, replace = FALSE)] <-NaN
y
mean(y)
mean(y, na.rm = TRUE)

