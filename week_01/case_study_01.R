Case Study 1

#load data
data(iris)

#Can view data head
#head(iris)

#run summary
summary(iris)

#draw graph
#Use $ to call out a variable from data columns
plot(iris$Sepal.Length,iris$Sepal.Width)

#Use y~x when building a model/regression, ln(y~x)
#Look in help files
plot(Sepal.Length~Sepal.Width, data=iris)

#finding mean for petal length
#use <- as an "equal sign" to assign a label to value
petal_length_mean <- mean (iris$Petal.Length)

#Plotting petal length as histogram
#Use col='color' to assign color to bars
#Use xlab="", and ylab"", and main="title"
hist (iris$Petal.Length, col='blue', border='white', xlab="Petal Length", main="Petal Length Distribution")

#Plotting a vertical line for the mean
abline(v = petal_length_mean, col='red', lwd=3, lty=2)

