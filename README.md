#MechaCar Challenge

##MPG Regression 
All factors including: 1. vehicle length, 2. vehicle weight, 3. spoiler angle, 4. ground clearance, and 5. AWD (all-wheel drive) were placed in to a multiple linear regression model to determine their impact(s) on predicting MPG (miles per gallon).

Interpretation of the multiple linear regression results:
    The Variables/coefficients of vehicle length and ground clearance provided a non-random amount of variance to the mpg values, having p-values < 0.05.  Whereas, vehicle weight, spoiler angle, and AWD have a p-value > .05 and are not significant to predicting variance in mpg.

The slope of the linear model is not considered to be zero, because there are factors of significance.  Also, when the factors of vehicle length and ground clearance are analyzed and plotted individual, both show a strong positive correlation with increasing MPG.

The linear model can predict mpg of MechaCar prototypes effectively because it has an adjusted R-squared result of 0.6825.  The model explains 68% of mpg variation.


##Suspension Coil Summary  

Summary statistics table for the suspension coil’s pounds-per-inch:  All Manufacturing Lots
  Mean = 1,498.78
  Median = 1500
  Variance = 62.29
  Standard deviation = 7.89

The design specifications for the MechaCar suspension coils dictate that the variance of the suspension coils must not exceed 100 pounds per inch. When you analyze all Manufacturing Lots as a whole it would appear that they meet the design specifications, because the variance for the whole population is under 100, at 62.29.  However, most of the population variance comes from Lot 3 which has variance of 170.29, far exceeding the design specifications.  There has been drift in the manufacturing control between the first Lot and the third, resulting in reject parts.

The manufacturing control of PSI can be easily visualized using a scatter plot overlaid with error bars clearly shows drift in manufacturing control.
  

##Suspension Coil T-Test

The suspension coil’s pound-per-inch results are statistically different from the mean population results of 1,500 pounds per inch; t = -1.98, with a p-value = 0.06.  The alternate hypothesis is true, mean is not equal to 1500.


##Design Your Own Study

The suspension coil PSI data demonstrates the variation that can exist in a manufacturing process.  Consumers want to purchase reliable vehicles that can be depended upon to last over their useable life, relatively free of defects and breakdowns. Ensuring the reliability of critical components is key to providing customers with an overall reliable vehicle.

Continued testing of MechaCar key components comparing manufacturing quality assurance data to competitive vehicles will allow MechaCar to design for reliability and directly match their design and results to the competition.  Using the continuous data from key components such as engine compression, suspension coil PSI, vehicle frame strength and others in a multiple linear regression and plotted by manufacturing year, make, and model with an overlay of error bars can visualize these comparisons.

Null hypothesis: Component matching design specification predicts vehicle reliability and customer satisfaction. Alternative hypothesis: Component matching design specification does not have an effect on vehicle reliability and customer satisfaction.

Data to collect:  component standard deviation to design specifications form MechaCar and Competitor vehicles, % of cars produced by model year remaining in operations, customer satisfaction survey results, particularly if customers would purchase their same vehicle again and if they recommend their vehicles to others for purchase. 
