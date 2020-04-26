#MPG Regression
#Using multiple linear regression, design a linear model that predicts the mpg of MechaCar prototypes using a number of variables within the MechaCar mpg dataset.

#Load a CSV file
mechacar <- read.csv("MechaCar_mpg.csv")

#Generate multiple linear regression model w/ all variables
lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + 
     ground.clearance + AWD, data = mechacar)
#Generate summary statistics
summary(lm(mpg ~ vehicle.length + vehicle.weight + spoiler.angle + 
             ground.clearance + AWD, data = mechacar)) 

#Multiple R-squared for all variable = 0.71 suggesting that all factors taken together may indicate some prediction of mpg
#Factors with p-value < 0.05 (vehicle.length and ground.clearance)

#Generate multiple linear regression model w/ factors of p-values < 0.05
lm(mpg ~ vehicle.length + ground.clearance, data = mechacar)
#Generate summary statistics
summary(lm(mpg ~ vehicle.length + ground.clearance, data = mechacar)) 

#Multiple Adjusted R-squared for vehicle.length and ground.clearance < 0.66

#Create linear model vehicle.length
model <- lm(vehicle.length ~ mpg,mechacar) 
#Determine y-axis values from linear model
yvals <- model$coefficients['vehicle.length']*mechacar$mpg + model$coefficients['(Intercept)'] 
#Import dataset into ggplot2
plt <- ggplot(mechacar,aes(x=vehicle.length,y=mpg)) 
#Plot scatter and linear model
plt + geom_point() + geom_line(aes(y=yvals), color = "red") 

#Create linear model ground.clearance
model <- lm(ground.clearance ~ mpg,mechacar) 
#Determine y-axis values from linear model
yvals <- model$coefficients['ground.clearance']*mechacar$mpg + model$coefficients['(Intercept)'] 
#Import dataset into ggplot2
plt <- ggplot(mechacar,aes(x=ground.clearance,y=mpg)) 
#Plot scatter and linear model
plt + geom_point() + geom_line(aes(y=yvals), color = "red") 

#Based having on p-values < 0.05, vehicle.length and ground clearance have significant impact(s) on mpg



#Suspension Coil Summary
#Create a summary statistics table for the suspension coil’s pounds-per-inch continuous variable.

#Load a CSV file
suspCoil <- read.csv("Suspension_Coil.csv")

psi_summary1 <- suspCoil %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),SD_PSI=sd(PSI))
print(psi_summary1)


#Scatter chart with mean + standard deviation
psi_summary2 <- suspCoil %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),Variance_PSI=var(PSI),SD_PSI=sd(PSI))
print(psi_summary2)
#Import dataset into ggplot2
plt <- ggplot(psi_summary,aes(x=Manufacturing_Lot,y=Mean_PSI)) 
#Add scatter plot with labels
plt + geom_point(size=4) + labs(x="Lot",y="Mean PSI") + 
#Overlay with error bars
geom_errorbar(aes(ymin=Mean_PSI-SD_PSI,ymax=Mean_PSI+SD_PSI)) 

#Suspension Coil T-Test
#Determine if the suspension coil’s pound-per-inch results are statistically different from the mean population results of 1,500 pounds per inch

t.test(suspCoil$PSI, mu = 1500)


