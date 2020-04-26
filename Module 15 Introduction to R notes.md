#load a CSV file
demo_table3 <- read.csv('demo2.csv',check.names = F,stringsAsFactors = F)

demo_table4 <- read.csv('demo2.csv',check.names = F,stringsAsFactors = F)

install.packages("rjson")

#Load a JSON from file
demo_table2 <- fromJSON(txt='demo.json')

filter_table <- demo_table2[demo_table2$price > 10000,]

#filter by price and drivetrain
filter_table2 <- subset(demo_table2, price > 10000 & drive == "4wd" & "clean" %in% title_status) 

#Use sample method to generate a sized sample
sample(c("cow", "deer", "pig", "chicken", "duck", "sheep", "dog"), 4)

#Use sample method on demo_table
demo_table[sample(1:nrow(demo_table), 3),]

library(tidyverse)

#Use mutate()function add columns
demo_table <- demo_table %>% mutate(Mileage_per_Year=Total_Miles/(2020-Year),IsActive=TRUE) #add columns to original data frame

#Use groupby()function
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer)) #create summary table

#Create summary table with multiple columns
#Use the summarize function along with statistical operators, mearn(), median(), sd(), min(), max(), and n()
summarize_demo <- demo_table2 %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer),Maximum_Price=max(price),Num_Vehicles=n()) 

#Use gather()function
long_table <- gather(demo_table3,key="Metric",value="Score",buying_price:popularity)


long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)

#Use spread()function
wide_table <- long_table %>% spread(key="Metric",value="Score")

#Compare data tables with all.equal()function
all.equal(demo_table3,wide_table)

#Syntax for order()function
#table <- table[,order(colnames(table))]

#US mph dataset is built into R 
head(mpg)

#1 Variable - geombar() Create a bar chart with mpg data 
plt <- ggplot(mpg,aes(x=class)) #import dataset into ggplot2
plt + geom_bar() #plot a bar plot

#2 variables - geom_col()
#Create summary table
mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count=n()) 
#Import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=manufacturer,y=Vehicle_Count)) 
#Plot a bar plot w/ labels
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") +
theme(axis.text.x=element_text(angle=45,hjust=1)) #rotate the x-axis label 45 degrees

#Create summary table
mpg_summary <- subset(mpg,manufacturer=="toyota") %>% group_by(cyl) %>% summarize(Mean_Hwy=mean(hwy)) 
#Import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=cyl,y=Mean_Hwy)) 
#Plot line chart use scal_x_discrete() and scal_y_discrete() functions to set axis values to specified values
plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30)) #add line plot with labels

#Import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class))
#Add scatter plot with labels
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class") 

#Import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv)) 
#Add scatter plot with multiple aesthetics
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive") 

#Boxplot
#Import dataset into ggplot2
plt <- ggplot(mpg,aes(y=hwy)) 
plt + geom_boxplot() #add boxplot

#import dataset into ggplot2
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) 
#add boxplot and rotate x-axis labels 45 degrees
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) 


#Heatmaps
#Create summary table
mpg_summary <- mpg %>% group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy)) 
plt <- ggplot(mpg_summary, aes(x=class,y=factor(year),fill=Mean_Hwy))
#Create heatmap with labels 
plt + geom_tile() + labs(x="Vehicle Class",y="Vehicle 
Year",fill="Mean Highway (MPG)") 


#Create summary table
mpg_summary <- mpg %>% group_by(model,year) %>% summarize(Mean_Hwy=mean(hwy)) 
#Import dataset into ggplot2
plt <- ggplot(mpg_summary, aes(x=model,y=factor(year),fill=Mean_Hwy)) 
#Heatmap w/ rotate x-axis labels 90 degrees
plt + geom_tile() + labs(x="Model",y="Vehicle Year",fill="Mean Highway (MPG)") + #add heatmap with labels > theme(axis.text.x = element_text(angle=90,hjust=1,vjust=.5)) 

#Import dataset into ggplot2
#Add boxplot
#Rotate x-axis labels 45 degrees
#Overlay scatter plot on top
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) #import dataset into ggplot2
plt + geom_boxplot() + #add boxplot
theme(axis.text.x=element_text(angle=45,hjust=1)) + #rotate x-axis labels 45 degrees
geom_point() #overlay scatter plot on top


#Mapping an argument
#Create summary table
mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ))
#Import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) 
#Add scatter plot
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") 

#Scatter chart with mean + standard deviation
mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ),SD_Engine=sd(displ))
#Import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) 
#Add scatter plot with labels
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") + 
#Overlay with error bars
geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine)) 

#convert to long format
mpg_long <- mpg %>% gather(key="MPG_Type",value="Rating",c(cty,hwy)) 
head(mpg_long)

#Import dataset into ggplot2
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) 
#Add boxplot with labels rotated 45 degrees
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) 

#Import dataset into ggplot2
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) 
#Create multiple boxplots, one for each MPG type
plt + geom_boxplot() + facet_wrap(vars(MPG_Type)) + 
theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") +
#rotate x-axis labels
xlab("Manufacturer") 

##Statistical tests

#Test for normality of the dataset w/ geom_density()
#Visualize distribution using density plot
ggplot(mtcars,aes(x=wt)) + geom_density() 

#Test for normality of the dataset w/ shapiro_test()  provides W and p-value
shapiro.test(mtcars$wt)

## if p-value > 0.05 the distribution is normal

##Test hypothesis
#Import used car dataset
population_table <- read.csv('used_car_data.csv',check.names = F,stringsAsFactors = F) 
#Import dataset into ggplot2
plt <- ggplot(population_table,aes(x=log10(Miles_Driven))) 
#Visualize distribution using density plot
plt + geom_density() 

#Use sample_n()function to create a random sample set
#Randomly sample 50 data points
sample_table <- population_table %>% sample_n(50) 
#Visualize distribution using density plot
plt <- ggplot(sample_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() 

#Compare sample versus population means (average mile drive for both datasets)
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven))) 
## if p-value > 0.05, above significance level and CNNOT reject the null hypothessis

##Two-sample t-Test
#Generate two sample datasets
sample_table <- population_table %>% sample_n(50) 
sample_table2 <- population_table %>% sample_n(50)
#Compare means of two samples
t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven)) 

##pair-t-Test
#Import dataset
mpg_data <- read.csv('mpg_modified.csv') 
#Generate two sample datasets
#select only data points where the year is 1999
mpg_1999 <- mpg_data %>% filter(year==1999)
#select only data points where the year is 2008
mpg_2008 <- mpg_data %>% filter(year==2008)
#Compare the mean difference 
t.test(mpg_1999$hwy,mpg_2008$hwy,paired = T) 


##ANOVA test
#Filter columns from mtcars dataset
mtcars_filt <- mtcars[,c("hp","cyl")] 
#Convert numeric column to factor
mtcars_filt$cyl <- factor(mtcars_filt$cyl) 
#Compare means across multiple levels
summary(aov(hp ~ cyl,data=mtcars_filt))

##p-value < 0.05 = siginifact evidence to reject the null hypothesis

##Find correlation coefficient = "r", with cor()function

head(mtcars)
#Plot variables with geom_point()
#Import dataset into ggplot2
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) 
#Create scatter plot
plt + geom_point() 
#Calculate correlation coefficient
cor(mtcars$hp,mtcars$qsec) 

#Read in dataset
used_cars <- read.csv('used_car_data.csv',stringsAsFactors = F) 
head(used_cars)
#Import dataset into ggplot2
plt <- ggplot(used_cars,aes(x=Miles_Driven,y=Selling_Price)) 
#Create a scatter plot
plt + geom_point() 
#Calculate correlation coefficient
cor(used_cars$Miles_Driven,used_cars$Selling_Price) 
#Create a correlation matrix
#Convert data frame into numeric matrix
used_matrix <- as.matrix(used_cars[,c("Selling_Price","Present_Price","Miles_Driven")]) 
cor(used_matrix)


##Linear regression
#Create linear model
lm(qsec ~ hp,mtcars) 
#summarize linear model
summary(lm(qsec~hp,mtcars))

#Create linear model
model <- lm(hp ~ qsec,mtcars) 
#Determine y-axis values from linear model
yvals <- model$coefficients['hp']*mtcars$hp + model$coefficients['(Intercept)'] 
#Import dataset into ggplot2
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) 
#Plot scatter and linear model
plt + geom_point() + geom_line(aes(y=yvals), color = "red") 

#Generate multiple linear regression model
lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars) 
#Generate summary statistics
summary(lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars)) 


#Chi-squared testing
#Generate contingency table
table(mpg$class,mpg$year) 
#Generate contingency table
tbl <- table(mpg$class,mpg$year)
#compare categorical distributions
chisq.test(tbl) 
