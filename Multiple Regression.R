install.packages("xlsx")
library(xlsx)

#How is total elec conc. related to solar power, hydro power and combustion power.
electr <- read.csv("C:/Users/91773/Downloads/Electricity 1/final2.csv")
head(electr)
electr1 = subset(electr, select = -c(Country.or.Area) )

pairs(electr1)
pairs(electr1, panel = panel.smooth)

model1<-lm(total~solar*hydro*combust+I(solar^2)+I(hydro^2)+I(combust^2),data = electr1)
summary(model1)

model2<-update(model1,~.-solar:hydro:combust)
summary (model2)

model3<-update(model2,~.-solar:combust)
summary(model3)

model4<-update(model3,~.-I(combust^2))
summary(model4)

model5<-update(model4,~.-I(hydro^2))
summary(model5)

par(mfrow = c(2,2))
plot(model5)

hist(residuals(model5))
durbinWatsonTest(model5)
ncvTest(model5) #insignificant va;ue req -- in this case it is significant
cooks.distance(model5)
influencePlot(model=model5, scale=3, main='Influence Plot') #Influence points = 3, Cooks dist -- more than 1
##Thus, model not fit##



model7<-lm(sqrt(total)~solar*hydro*combust+I(solar^2)+I(hydro^2)+I(combust^2),data = electr1)
summary(model7)

model8<-update(model7,~.-solar:hydro:combust)
summary (model8)

model9<-update(model8,~.-solar:combust)
summary (model9)

model10<-update(model9,~.-I(solar^2))
summary(model10)

model11<-update(model10,~.-I(hydro^2))
summary(model11)

model12<-update(model11,~.-solar:hydro)
summary (model12)

model13<-update(model12,~.-solar)
summary (model13)

par(mfrow = c(2,2))
plot(model13)


hist(residuals(model13))
durbinWatsonTest(model13)
ncvTest(model13)
vif(model13)  ##vif greater than 10 for variable combust
##Hence model is not fit



vif(model5) ##to find some variables with less correlation

##from above vif vales, hydro, combust, solar:hydro and hydr:combust have comparable values
##Thus, creating regression model using these variables.
mode1x<-lm(total~combust+hydro+(solar:hydro)+(hydro:combust),data = electr1)
summary (mode1x)

mode1y<-update(mode1x,~.-hydro:solar)
summary (mode1y)

par(mfrow = c(2,2))
plot(mode1y)

#from graph, it is clear that one influence point lies in the top right corner, thus creating issue. checking for the same
influencePlot(model=mode1y, scale=3, main='Influence Plot')
## Cook's Dist greater than 1, thus this model is not fit



##Taking square root of the DV for abover eqaution
model20<-lm(sqrt(total)~combust+hydro+(solar:hydro)+(hydro:combust),data = electr1)
summary (model20)

model21<-update(model20,~.-hydro:solar)
summary (model21)

par(mfrow = c(2,2))
plot(model21)

hist(residuals(model21)) #Normal dist
durbinWatsonTest(model21) #DW statistic between 2 to 4
ncvTest(model21) # p value is insignificant
vif(model21) # comparable values
cooks.distance(model21) 
influencePlot(model=model21, scale=3, main='Influence Plot') #Distance of all influence points is less than 1

##Nearly all assumptions satisfied. Best model with adjusted R square -> 0.892
