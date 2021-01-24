#set work directory
setwd('C:/xxx/xxx')

#read data
AF = read.csv(file = 'mmm-data.csv', header = TRUE)
colnames(AF)[1] = 'Period'

#update period column to correct date format
AF$Period = as.Date(AF$Period, '%m/%d/%Y')

#add Black Friday column
AF[,'Black_Friday'] = 0
AF[which(AF$Period == '2014-11-24'), 'Black_Friday'] = 1
AF[which(AF$Period == '2015-11-30'), 'Black_Friday'] = 1
AF[which(AF$Period == '2016-11-28'), 'Black_Friday'] = 1
AF[which(AF$Period == '2017-11-27'), 'Black_Friday'] = 1
sum(AF[,'Black_Friday'])

#add July 4th column
AF[,'July_4th'] = 0
AF[which(AF[,'Period'] == '2014-07-07'), 'July_4th'] = 1
AF[which(AF$Period == '2015-07-06'), 'July_4th'] = 1
AF[which(AF$Period == '2016-07-04'), 'July_4th'] = 1
AF[which(AF$Period == '2017-07-03'), 'July_4th'] = 1
sum(AF[,'July_4th'])
sum(AF$July_4th)
AF[1,1]
AF[1,'July_4th']

#build model
#step1: add baseline variables
model1 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend)
summary(model1)

#step2: add media variables (we have 2 )
#add TV
model2 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV1)
summary(model2)

model2 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2)
summary(model2)
#roll out the best one (TV2)

#add search 
model3 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1)
summary(model3)

model3 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch2)
summary(model3)
#roll out the best one (search1)

#add wechat
model4 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1+Wechat1)
summary(model4)

model4 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1+Wechat2)
summary(model4)
#roll out (Wechat2)

#add magazine
model5 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1+Wechat2+Magazine1)
summary(model5)

model5 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1+Wechat2+Magazine2)
summary(model5)
#roll out (Magazine1)

#add display
model6 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1+Wechat2+Magazine1+Display1)
summary(model6)

model6 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1+Wechat2+Magazine1+Display2)
summary(model6)
#roll out (Display2)

#add Facebook
model7 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1+Wechat2+Magazine1+Display2+Facebook1)
summary(model7)

model7 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1+Wechat2+Magazine1+Display2+Facebook2)
summary(model7)
#roll out Facebook1 *because facebook2 shows strong multicolinearity with other variables, make the whole model less stable

#export results
model7 = lm(data = AF, Sales~July_4th+Black_Friday+CCI+Sales.Event+Comp.Media.Spend+NationalTV2+PaidSearch1+Wechat2+Magazine1+Display2+Facebook1, x=TRUE, y=TRUE)
#this x & y refers to x in y = ax1+bx2+cx3..., we do this so that we can seperately pull all x (x1, x2, x3...) out in the next step

summary(model7)
View(model7$x)
model7$coefficients
View(model7$coefficients)

coefficient = data.frame(model7$coefficients)

#export coefficients
write.csv(coefficient, file = 'coefficients.csv')

#calculate contribution
contribution = sweep(model7$x, 2, model7$coefficients, "*")
#2 means do calculation between columns, 1 means do calculations between rows

#convert contribution to data frame format
contribution = data.frame(contribution)

#2 ways to fill in 'Period' data
contribution$Period = AF$Period
contribution[,'Period'] = AF[, 'Period']

#pull names of coefficients
names(model7$coefficients)

#this c means 'concat', concat all coefficient names plus a 'Period'
colnames(contribution) = c(names(model7$coefficients), 'Period') 

#reshape and export contribution data set
install.packages('reshape')
library('reshape')
contri = melt(contribution, id.vars='Period')
#id.vars means the variable we wanna keep, all others will be melt (unpivot)
write.csv(contri, file='contribution.csv', row.names=FALSE)

#get AVM (actual vs modeled) and export it
AVM = cbind(AF[,c('Period', 'Sales')], model7$fitted.values)
colnames(AVM) = c('Period', 'Sales', 'Modeled Sales')
write.csv(AVM, file='AVM.csv', row.names=FALSE)

#calculate MAPE
AVM$MAPE = abs(AVM$Sales-AVM$`Modeled Sales`)/AVM$Sales
mean(AVM$MAPE)