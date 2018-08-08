
#1.Mengaktifkan librari

library(Hmisc)
library(psych)
library(car)


# menset direktori
setwd("D:/dataset_metkuan/automobile")

# Membaca data set Automobile.csv
auto <- read.csv(file = "automobile.csv", header=TRUE, na.strings="?")
summary(auto)

#mengeliminasi missing values
auto$price <- as.numeric(impute(auto$price, mean))
auto$normalized.losses <- as.numeric(impute(auto$normalized.losses, mean))
auto$num.of.doors <- as.numeric(impute(auto$num.of.doors, median))
auto$horsepower <- as.numeric(impute(auto$horsepower, mean))
auto$peak.rpm <- as.numeric(impute(auto$peak.rpm, mean))
auto$bore <- as.numeric(impute(auto$bore, mean))
auto$stroke <- as.numeric(impute(auto$stroke, mean))
summary(auto)

# Untuk pemdodelan regresi saya melakukan subset beberapa variable numerik (terbaik)
auto.sel <- subset(auto, select = c(horsepower, city.mpg, peak.rpm, curb.weight, num.of.doors, price))

# memeriksa kesalahan secara visual
pairs.panels(auto.sel, col="red")


# Split data
set.seed(2017)
train.size <- 0.8 
train.index <- sample.int(length(auto.sel$price), round(length(auto.sel$price) * train.size))
train.sample <- auto.sel[train.index,]
valid.sample <- auto.sel[-train.index,]

# stepwise selection dan backwards elimination

fit <- lm(price ~ horsepower+city.mpg+peak.rpm+curb.weight+num.of.doors, data=train.sample)
summary(fit)

fit <- lm(price ~ horsepower+city.mpg+peak.rpm+curb.weight, data=train.sample)
summary(fit) 

fit <- lm(price ~ horsepower+city.mpg+curb.weight, data=train.sample)
summary(fit) 

fit <- lm(price ~ horsepower+curb.weight, data=train.sample)
summary(fit)
plot(fit)

##### Tahap Lanjut
# menentukan nilai prediksi dari train.sample dan valid.sample
train.sample$Pred.price <- predict(fit, 
                                   newdata = subset(train.sample, select=c(price, horsepower, curb.weight)))
valid.sample$Pred.price <- predict(fit, 
                                   newdata = subset(valid.sample, select=c(price, horsepower, curb.weight)))

# model teori didefinisikan -> R Square
summary(fit)




# cek seberapa baik model di train
train.RMSE <- round(sqrt(mean((train.sample$Pred.price - train.sample$price)^2)))
train.MAE <- round(mean(abs(train.sample$Pred.price - train.sample$price)))
train.corr <- round(cor(train.sample$Pred.price, train.sample$price), 2)
c(train.RMSE, train.MAE)
c(train.corr^2)


# cek seberapa baik model di valid
valid.RMSE <- round(sqrt(mean((valid.sample$Pred.price - valid.sample$price)^2)))
valid.MAE <- round(mean(abs(valid.sample$Pred.price - valid.sample$price)))
valid.corr <- round(cor(valid.sample$Pred.price, valid.sample$price), 2)
c(valid.RMSE, valid.MAE)
c(valid.corr^2)

#Analisis

#analisis -> cek Multicolinearity
vif(fit)

#analisis -> cek Heteroscedasticity
ncvTest(fit)
spreadLevelPlot(fit)

#analisis -> cek Autocorrelation (Durbin-Watson Test)
library(lmtest)
dwtest(fit)

res <- residuals(fit)
plot(res, ylab="Resdiuals", xlab="observation order", main="plot Residuals vs observation order", col="blue", pch=16, cex=1.3)

abline(0,0)

