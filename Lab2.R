getwd()
setwd("C:/Users/medoc/Desktop/M/AD/Lab2")
getwd()

readfile <- read.csv("fastfood.csv")

View(readfile)

#Q-Q plot:

#calories
qqnorm(readfile$calories, main="calories")
qqline(readfile$calories)

#total_carb 
qqnorm(readfile$total_carb, main="carbohydrate")
qqline(readfile$total_carb)

#cholesterol 
qqnorm(readfile$cholesterol, main="cholesterol")
qqline(readfile$cholesterol)

#Критерій Шапіро-Вілка

#calories
shapiro.test(readfile$calories)

#total_carb
shapiro.test(readfile$total_carb)

#cholesterol
shapiro.test(readfile$cholesterol)

#Критерій Андерсона-Дарлінга

library("nortest")

#calories
ad.test(readfile$calories)

#total_carb
ad.test(readfile$total_carb)

#cholesterol
ad.test(readfile$cholesterol)

#Аналіз істотності парних статистичних зв'язків

corr.readfile = readfile[,c(3,5,8)]
cor(corr.readfile, y=NULL, method = "spearman")

#калорії та вуглеводи
cor.test(readfile$calories,readfile$total_carb, method = "spearman", exact=FALSE)

#калорії та холестерин
cor.test(readfile$calories,readfile$cholesterol, method = "spearman", exact=FALSE)

#вуглеводи та холестерин
cor.test(readfile$total_carb,readfile$cholesterol, method = "spearman", exact=FALSE)

corr.readfile = readfile[,c(3,5,8)]
cor(corr.readfile, y=NULL, method = "kendall")

#калорії та вуглеводи
cor.test(readfile$calories,readfile$total_carb, method = "kendal", exact=FALSE)

#калорії та холестерин
cor.test(readfile$calories,readfile$cholesterol, method = "kendal", exact=FALSE)

#вуглеводи та холестерин
cor.test(readfile$total_carb,readfile$cholesterol, method = "kendal", exact=FALSE)

#коефіцієнт Спірмена для множинних статистичних зв’язків

#сalories 
mod <- lm(calories ~ total_carb + cholesterol, data = readfile)
sqrt(summary(mod)$r.squared)

#total_carb 
mod <- lm(total_carb ~ calories + cholesterol, data = readfile)
sqrt(summary(mod)$r.squared)

#cholesterol 
mod <- lm(cholesterol ~ total_carb + calories, data = readfile)
sqrt(summary(mod)$r.squared)


#Дізнаємося p-value

#сalories 
mod <- lm(calories ~ total_carb + cholesterol, data = readfile)
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm'")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
  p
}
lmp(mod)

#total_carb 
mod <- lm(total_carb ~ calories + cholesterol, data = readfile)
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm'")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
  p
}
lmp(mod)

#cholesterol 
mod <- lm(cholesterol ~ total_carb + calories, data = readfile)
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm'")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
  p
}
lmp(mod)

#Graphik

plot (readfile$calories,readfile$total_carb, main="Scatterplot",col="red",
      xlab="Calories", ylab="Carbohydrate", pch=19)

plot (readfile$calories,readfile$cholesterol, main="Scatterplot",col="red",
      xlab="Calories", ylab="Cholesterol", pch=19)

plot (readfile$cholesterol,readfile$total_carb, main="Scatterplot",col="red",
      xlab="Cholesterol", ylab="Carbohydrate", pch=19)
