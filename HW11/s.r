#109612054
#2021.12.27_HW

# load data
data("iris")
?iris
str(iris)
Sepal.Length <- iris$Sepal.Length
versicolor <- which(iris$Species == "versicolor")
d.versicolor <- iris$Species[versicolor]
len.versicolor <- iris$Sepal.Length[versicolor]
setosa <- which(iris$Species == "setosa")
d.setosa <- iris$Species[setosa]
len.setosa <- iris$Sepal.Length[setosa]
#dataframe
data <- data.frame(Sepal.Length = c(len.setosa, len.versicolor), Species = c(d.setosa, d.versicolor))
str(data)

#(1) Normality Test
#Shapiro-Francia
shapiro.test(len.versicolor)
shapiro.test(len.setosa)

#(2) Box Plot 
library(ggplot2)
ggplot(data = data, aes(x = Sepal.Length, y= Species))+
         geom_boxplot() +
         geom_point()+
         labs(title = "Boxplot Sepal data",
              x = "Sepal Length (mm)",
              y = "Species")

#(3) Test Statistics (Two sample z test)
library(BSDA)
z.test(len.versicolor, len.setosa
       , alternative = 'two.sided'
       , sigma.x = sd(len.versicolor), sigma.y = sd(len.setosa)
       , mu = 0
       , conf.level = 0.95)




