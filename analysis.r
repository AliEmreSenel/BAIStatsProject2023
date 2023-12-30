#Loading data
setwd("~/University/2 year/Mathematical statistics/Stats_project")
data = read.table("insurance_2.csv", header=TRUE, sep=",")
attach(data)
names(data)

sex = factor(sex)
smoker = factor(smoker)
region = factor(region)

summary(data)

library(corrplot)
d = data[, c('age', 'bmi', 'children', 'charges')]
cor(d)
corrplot(cor(d))


#Preliminary plots:
hist(log(charges))
qqnorm(log(charges))
qqline(log(charges))
shapiro.test(log(charges))
?ks.test
?qqline
#Age vs charges:
plot(age, charges)

#Smoker vs non-smoker
plot(age, charges, type='n')
points(age[smoker=='yes'], charges[smoker=='yes'], pch='o', col='red')
points(age[smoker=='no'], charges[smoker=='no'], pch='o', col='blue')
legend('topright', legend=c('smoker', 'non-smoker'), col=c('red', 'blue'), pch='o')

boxplot(charges ~ smoker, data=data, col=c('blue', 'red'))

#bmi, charges:
plot(bmi, charges, type='n')
points(bmi[smoker=='yes'], charges[smoker=='yes'], pch='o', col='red')
points(bmi[smoker=='no'], charges[smoker=='no'], pch='o', col='blue')
legend('topleft', legend=c('smoker', 'non-smoker'), col=c('red', 'blue'), pch='o')

#3D plot age, bmi, charges:
library(scatterplot3d)
colors <- c("blue", "red")
colors <- colors[as.numeric(smoker)]
s3d = scatterplot3d(age, bmi, charges, pch=20,
    main="3D plot of bmi, charges and age", color=colors, 
    box=FALSE, angle=10, grid=TRUE)
legend('topleft', legend=c('smoker', 'non-smoker'), col=c('red', 'blue'), pch='o')

lm_smoker <- lm(charges[smoker=='yes'] ~ age[smoker=='yes'] + bmi[smoker=='yes'], data=data)
s3d$plane3d(lm_smoker, col='red')
lm_nonsmoker <- lm(charges[smoker=='no'] ~ age[smoker=='no'] + bmi[smoker=='no'], data=data)
s3d$plane3d(lm_nonsmoker, col='blue')

summary(lm_smoker)


#Region and charges:
region = factor(region)
boxplot(charges ~ region, data=data, col=c('blue', 'red', 'green', 'yellow'))

