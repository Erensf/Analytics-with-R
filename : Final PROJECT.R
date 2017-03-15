// Final PROJECT

# How Heating Load is affected 
# by surface area whether Overall Height (how far the sensor from the roof) is 3.5 or 7 inches ?

  d <- read.csv("data1.csv")


  summary(d)
  " 
 Overall_Height  Surface_Area    Heating_Load  
 Min.   :3.50   Min.   :514.5   Min.   : 6.01  
 1st Qu.:3.50   1st Qu.:606.4   1st Qu.:12.99  
 Median :5.25   Median :673.8   Median :18.95  
 Mean   :5.25   Mean   :671.7   Mean   :22.31  
 3rd Qu.:7.00   3rd Qu.:741.1   3rd Qu.:31.67  
 Max.   :7.00   Max.   :808.5   Max.   :43.10  
"
  
 head(d)
 
  height3.5 <- d[which(d$Overall_Height==3.5),]
  height7 <- d[which(d$Overall_Height==7),]

 head(height3.5)

 "
   Overall_Height Surface_Area Heating_Load
25            3.5        686.0         6.07
26            3.5        686.0         6.05
27            3.5        686.0         6.01
28            3.5        686.0         6.04
29            3.5        710.5         6.37
30            3.5        710.5         6.40

 "
 head(height7)

 "
  Overall_Height Surface_Area Heating_Load
1              7        514.5        15.55
2              7        514.5        15.55
3              7        514.5        15.55
4              7        514.5        15.55
5              7        563.5        20.84
6              7        563.5        21.46

 "

 t <- t.test(height7$Heating_Load, height3.5$Heating_Load)

"
	Welch Two Sample t-test

data:  height7$Heating_Load and height3.5$Heating_Load
t = 53.857, df = 531.21, p-value < 2.2e-16
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 17.28311 18.59165
sample estimates:
mean of x mean of y 
 31.27589  13.33851 
"

# CI diffrence of the means would be in this interval, various of your data will be in 
# this interval 
# the mean difference of heating load whether the sensor is 3.5 inches or 7 inches far from the roof is [1] 17.93738 in in my CI

## effect size = mean diffrence / SD 
#  Heating loads are not same for two groups (height=7 and height=3.5) because p-value is 2.2e-16 which is less than 0.05. I could say that the probibilty that two distribution can conflict is pretty less. # make it simple;
 fit1 <- lm(Heating_Load ~ Surface_Area , data=height7)
 summary(fit1)
"
Call:
lm(formula = Heating_Load ~ Surface_Area, data = height7)

Residuals:
     Min       1Q   Median       3Q      Max 
-16.2286  -2.8192   0.9919   4.4033   9.4924 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -2.767643   3.336822  -0.829    0.407    
Surface_Area  0.057104   0.005579  10.236   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 5.282 on 382 degrees of freedom
Multiple R-squared:  0.2152,	Adjusted R-squared:  0.2132 
F-statistic: 104.8 on 1 and 382 DF,  p-value: < 2.2e-16

 
"
For Height=7
Heating Load = -2.767643 + 0.057104 * Surface Area
# for every aditional foot square Surface Area, heating load is expected to increase by 0.057104 joule. 


 fit2 <- lm( Heating_Load ~ Surface_Area , data=height3.5) 
 summary(fit2)


"
Call:
lm(formula = Heating_Load ~ Surface_Area, data = height3.5)

Residuals:
    Min      1Q  Median      3Q     Max 
-6.7554 -1.5059  0.0566  1.5781  5.0614 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -9.437397   2.144962   -4.40 1.41e-05 ***
Surface_Area  0.030480   0.002866   10.63  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.35 on 382 degrees of freedom
Multiple R-squared:  0.2284,	Adjusted R-squared:  0.2264 
F-statistic: 113.1 on 1 and 382 DF,  p-value: < 2.2e-16
"
Heating Load = -9.437397 + 0.030480 * Surface Area
# for every aditional foot square Surface Area, heating load is expected to increase 0.030480 by joule. 

#Slope for overall height=7 is (0.057104) greater than slope for overall height=3.5 (0.030480). That makes sense because if the sensor is 7 inches far from the roof that means not as close as the sensor is 3 inches far from the roof. To sum up, more energy is needed to maintain the temperature.


#  the difference of slope is 3.7257 

    opar <- par (mfrow = c(1,2))
   plot(height7$Heating_Load, height7$Surface_Area, main="Plot of Overall Height=7",
     xlim=c(15,44), ylim=c(514,662), xlab="Heating Load", ylab="Surface Area ") 
 	abline (fit1)
 	
   plot(height3.5$Heating_Load, height3.5$Surface_Area, main="Plot of Overall Height=3.5",
     xlim=c(6,20), ylim=c(686,809), xlab="Heating Load", ylab="Surface Area ") 
 	abline (fit2)

    par(opar)
    
    
    

    # main thing for regression is prefict future, predict()
   
    # CI is the border for two gruoppus diffrence 
    # slope how this change in X , diffrence of slope greater one you say one gruop is having more revenue 
    # power and effect size - 
 

#BOXPLOT
    opar <- par (mfrow = c(1,2))
 boxplot(height7$Heating_Load)
     par(opar)
     
# DENSITY
   opar <- par (mfrow = c(1,2))
   a<-density(height7$Heating_Load)
   plot(a,main="Density of Heating_Load for height=7", xlab= "Heating Load", xlim=c(15,44))  
    b<-density(height3.5$Heating_Load)
    plot(b, main="Density of Heating_Load for height=3.5",xlab= "Heating Load",xlim=c(6,20))
     par(opar)
# Heating load is changing between 6.01 and 19.52 for height=3.5, and changing between 
# 15.55 and 43.10 for height=7. Again, density plots clearly shows that for height=7 heating load is higher than height=3.5

# HISTOGRAM
   opar <- par (mfrow = c(1,2))
hist(height7$Heating_Load, breaks=10, col="red", xlab="Heating Load", 
   	main="Histogram of Heating Load for Height=7") 
hist(height3.5$Heating_Load, breaks=10, col="red", xlab="Heating Load", 
   	main="Histogram of Heating Load for Height=3.5") 
   par(opar)
   
   # PREDICTION
   new7 <- data.frame(Surface_Area =c(max(height7 $Surface_Area)*2,max(height7 $Surface_Area)*3,max(height7 $Surface_Area)*4))
pred.w.clim <- predict(fit1, new7, interval="confidence")
matplot(new7$Surface_Area, pred.w.clim,
            main='Forecast',
            lty=c(1,2,2),type="l",lwd=c(1.5,1,1),
            ylab="Heating Load",
            col=c("cadetblue4","chocolate4","chocolate4"),
            xlab="Surface Area",
            xaxt='n',
            cex=0.8,
            cex.axis=0.8)
           
grid(lty=6,col="cornsilk2")
axis(side=1,at=new7$Surface_Area,labels=(c("*2","*3","*4")))

   
   
   

