#---------Introduction---------
#Hi there! This is a short(ish) tutorial on coding in R, designed to be digestible for all ranges of 
#programming experience. Hopefully, by reading this, that means you have downloaded RStudio - which is all 
#you should need!

#While most of this tutorial covers programming syntax, it also gives a very brief overview of some 
#statistical techniques common in research methodologies. Hopefully by the end of this, you will be a little
#more comfortable with both of these! If not, I at least hope you had fun :)

#NOTE: For this tutorial, we are going to be using sample data, since the real data won't be collected until 
#later on in the workshop. Once you walk through analyzing this sample data, you can replicate the analysis with 
#the real data!

#Without further ado, let's get started!

#---------Preparation---------

#Step 1: Clear current work space (allows us to start with a clean environment for analysis!)

#If you're using RStudio, the environment should be in the top right panel. After running this, you should 
#see it display 'Environment is empty.' Clearing your work space is generally recommended, as it can prevent
#previously loaded data and functions from filling your memory!

rm(list = ls())


#Step 2: Load all necessary packages

#Most coding languages are supplemented by additional 'packages' that are not included in the base program.
#These programs provide a lot of extra functions and capabilities that are not accessible otherwise.

#To use packages in R, we first have to install them onto our application. We only need to do this one time!
#Once we've installed a package once on a computer, we should never have to install it again. To do this,
#we use the format: install.packages('package name')

#Once they are installed, we simply need to load them. Every time we restart R (or clear memory),
#we will need to load them again! To load, we use the format: library(package name)

#Let's install and load all of the packages needed for this tutorial! Don't worry about these too much now,
#we'll go over them later on :)

install.packages('psych')
library(psych)

install.packages('skimr')
library(skimr)

install.packages('tidyverse')
library(tidyverse)

#Remember, we only need to install a package one time ever! After that, we should load it in (with library),
#every time we restart R.


#Step 3: Set working directory

#This allows us to set a 'path' to where we want to access our files. Without a working directory 
#specified, R won't know where the correct files are and won't be able to load them in.

#To see your current working directory, run the line below.

getwd()

#We want to set our working directory to where our data is located. Ideally, we want all of our data
#to be in the same place to avoid needing to set a new working directory for each data set. This normally
#means keeping all of our data together in a single folder that's easy to find/access.

#Here's how to find your working directory:

#On Windows - https://www.youtube.com/watch?v=vbb-lDh477g
#On Mac - https://www.youtube.com/watch?v=kIhGavBqXYc


#Below is an example of what a working directory should look like:

#setwd('C:/Users/jo/Desktop/Teaching/Data')

#Now set your own! Remember, all your data should be located within this working directory.

setwd('__')


#Step 4: Load the data

#Now that we have our working directory set, we can load in the subsequent data. To do this, we are
#going to use the function read.csv() to, well, read in a csv file! Since your data sets should already be 
#in csv form, this is practically transferring the excel sheet into R.

#We don't have the real data yet, so please run the function below to create the sample data! Don't worry
#about what this does, it just prepares the sample for use and saves it to your current directory as a csv.

prepare_sample_methylation <- function() {
  
  set.seed(1)
  
  m_sample <- data.frame(
    ID = 1:100,
    Point1 = runif(100, min = 90.5, max = 99.5),
    Point2 = runif(100, min = 90.5, max = 99.5)
  )
  
  assign_missing <- function(data, prop) {
    total <- round(length(data) * prop)
    obs <- sample(1:length(data), total)
    data[obs] <- NA
    return(data)
  }

  m_sample$Point1 <- assign_missing(m_sample$Point1, 0.05)
  m_sample$Point2 <- assign_missing(m_sample$Point2, 0.05)
  
  write.csv(m_sample, file = 'SampleMethylationData.csv', row.names = FALSE)
  
}
prepare_sample_methylation()


prepare_sample_stress <- function() {
  
  set.seed(1)
  
  s_sample <- data.frame(
    ID = 1:100,
    Maternal_Age = round(runif(100, min = 20, max = 40)),
    Gender = sample(c("Male", "Female"), 100, replace = T),
    Q1 = round(runif(100, min = 0, max = 4)),
    Q2 = round(runif(100, min = 0, max = 4)),
    Q3 = round(runif(100, min = 0, max = 4)),
    Q4 = round(runif(100, min = 0, max = 4))
  )
  
  assign_missing <- function(data, prop) {
    total <- round(length(data) * prop)
    obs <- sample(1:length(data), total)
    data[obs] <- NA
    return(data)
    
  }
  
  #s_sample$Q1 <- assign_missing(s_sample$Q1, 0.05)
  #s_sample$Q2 <- assign_missing(s_sample$Q2, 0.05)
  #s_sample$Q3 <- assign_missing(s_sample$Q3, 0.05)
  #s_sample$Q4 <- assign_missing(s_sample$Q4, 0.05)
  
  write.csv(s_sample, file = 'SampleStressData.csv', row.names = FALSE)
}
prepare_sample_stress()

#To make sure the data stays within our memory, we need to assign the csv file to a name. The syntax below
#effectively reads in the first csv file, saving it as 'methylation_data'

methylation_data <- read.csv('SampleMethylationData.csv')

#Now let's do the same with our stress data!
stress_data <- read.csv('SampleStressData.csv')
  

#Step 5: Merge data sets

#Now, let's merge our methylation data with our stress data so we can look at them together!
#When merging data frames in R, we generally want to have a common column in order to match data together.
#A common example of this is an 'ID' variable. So, if we have the same ID numbers across different data 
#frames, then we can combine all of the variables together! In our case, we want every 'participant' to have 
#stress data and methylation data within the same data set.

#Before we start, let's first look at our data sets. By running the code below, we can look at the dimensions 
#(rows/columns) of our data, but also the variable names. This is important for merging because we want to 
#make sure that merging is viable. For instance, we want to make sure the 'ID' variable is spelled the same, 
#and that there are enough rows in each data set so we won't loose too much data by merging.

dim(methylation_data)
dim(stress_data)

names(methylation_data)
names(stress_data)

#If this all looks good, let's start merging!

#To do this, let's first look at the 'merge' function in R. After running the line below, the documentation 
#will show in the bottom right. This will give us information about the function while providing us with 
#different variations of use!

?merge()

#The syntax for merging is relatively straightforward, as we want to specify which data sets we want to 
#combine, and by which column we want to combine by!

#An important option in the merge function is the use of all.x, all.y, or simply all. Typically,
#the merge function will only keep the observations where a match was found. In our case, a match
#would mean that there is a specific ID number in both data sets. Sometimes, one data set may have
#more participants than another (due to attrition or any number of reasons). By specifying 'all = T',
#that means we are keeping all observations / ID numbers, even if they were only present in a single data set.

#Putting this all together, here's how we're going to merge our data! We specify that we want to merge the
#two data sets, by ID, with all observations being preserved. We then save this to a new data frame called 
#'merged_data'

merged_data <- merge(methylation_data, stress_data, by = 'ID', all = TRUE)

#If this didn't work, then there may be an error in some of the data - most often the ID names may not all 
#match perfectly, with something misspelled or having a space after it... Always check the dimensions of 
#your new data set, and ensure it has the right number of rows. If all looks good, we can finally 
#begin our analysis :)

#---------Creating variables---------

#We also want to create some variables for our analysis, based upon the data we have. By creating new variables,
#we can better understand our data and uncover trends that may not be noticeable via excel!

#Let's start by calculating the difference between the methylation levels for each individual (across 
#duplicates). To do this, we want to specify the creation of a new variable: 'methylation_differential'

#Since we want to end up with a positive value, we want to take the absolute value of the difference. 

merged_data$differential <- abs(merged_data$Point1 - merged_data$Point2)

#When creating or referencing variables in R, we use a $ to divide the data frame and variable of interest. 
#So, since we are creating a variable named 'methylation_average' in the 'merged_data' data frame, we 
#reference it as: 'merged_data$methylation_average'

#While this may seem a bit clunky and unnecessary, it's pretty important for when we are working with 
#multiple data frames and similarly named variables (For example, we may have 'Age' in 3 different data 
#sets so specifying the right one is important).

#Now it's your turn! Let's calculate the average methylation level for each individual. To do this, use 
#the same format as before to create a variable entitled: 'methylation_average'
#Remember, we want this variable to represent the average methylation of the two time points!

merged_data$methylation_average <- __

#To find out if the code worked properly, click on the 'merged_data' label in the top right window.
#This should show the entire data frame! Now look for the methylation_average variable to see the values.
#Don't worry if there are some NA values, we will fix those next!

#Before we fix the NA values, let's try and find the total average using the 'mean' function.

mean(merged_data$methylation_average)

#As you can see, dealing with NA's is an important part of data analysis since missing values can lead 
#to analyses being incomplete. Oftentimes, a lot of individuals may have a few missing variables but that 
#doesn't mean we should throw out the entire individual! These individuals still contain important data, 
#so it's best for us to 'work around' missing data.

#Let's try making the methylation_average variable a different way, this time accounting for NA values.
#To do this, we are going to use the 'rowMeans' function.

#rowMeans (for this sake) simply calculates the mean of a specified variable or variables for EACH row 
#(or individual). It also allows us to use na.rm, which is a very powerful tool in R. This option, common 
#in many functions, allows us to ignore missing variables in our calculations if we set it equal to TRUE.

#To see na.rm in action, try finding the total average again, this time with na.rm = TRUE specified!

mean(merged_data$methylation_average, na.rm = TRUE)

#Below is some starter code for using rowMeans. Try and see if you can create the variable 
#'methylation_average' using this format. How is it different from our last version of the variable?
#Also, play around with the settings! What happens if na.rm is equal to FALSE?

rowMeans(data_set[, c('variable1', 'variable2', '...')], na.rm = TRUE)
#Side note: Brackets in R can be used to subset data by specifying rows first and columns after, separated 
#by a comma. In our case, we only need to specify our columns since the function already iterates through 
#each row!

merged_data$methylation_average <- __

#Remember to save this new version of methylation_average as a variable in the data set. Even though we 
#already have methylation_average saved before, this new version will overwrite it!


#Another variable we should create is a total PSS score, which will combine our stress questions into a composite
#score to look at. However, before we do this we should look closer at the questions themselves. Notice how 
#the first and last questions have higher values as higher indicators of stress, while the second and third questions
#have LOWER values as higher indicators of stress. Because of this, we need to reverse the coding of these questions.

#To change the coding of a variable, all we have to do is update the variable with a new formula, similar to how
#we just updated methylation_average. Try and write these lines yourself! Start by looking at what the highest
#possible selection is and then subtract the current value.
#NOTE: you can reference the same variable you're updating as part of your formula!

merged_data$Q2 <- __
merged_data$Q3 <- __

#Now that all of our variables are in the right format, we can compute the total PSS score! Run the code below
#to add it to the data frame.

merged_data$PSS <- merged_data$Q1 + merged_data$Q2 + merged_data$Q3 + merged_data$Q4


#The final variable we're going to create is a binary value representing the PSS scores we just created, as we
#want to differentiate 'low' stress from 'high' stress within our data set. This can be very useful for future
#analyses, especially when looking at trends between different groups of people. Usually, we will have some standard
#on how to differentiate low from high, but right now we're just going to use the mean as a midpoint.

#Start by calculating the mean of PSS:

___

#Then, input the mean value into the code below to create this variable!

merged_data$high_PSS <- ifelse(merged_data$PSS > ___, 1, 0)
#Side note: We use an if-else statement here to create a conditional. This is practically saying, "If PSS is
#greater than __, set the new variable, high_PSS, equal to 1. Otherwise, set this new variable equal to 0. If-else
#statements are incredibly useful in R, but can often result in syntax errors (especially when nesting them), so
#we'll hold off from using them too much in this tutorial.

#---------Getting rid of outliers---------

#Before we continue, we should also get rid of untrustworthy data. In this case, we are going to treat
#any observation with a differential of 5% or greater between duplicates as bad data that should be removed.

#By running the following line of code, the data frame will filter out these values. 

merged_data <- subset(merged_data, differential <= 5)

#Looking at the environment window (top right panel), we can see how many observations we are now left with. while
#we lost some of our data, that's okay! We still have enough observations to conduct meaningful analyses.

#---------Using Summary Statistics: summary() and skim()---------

#Since we have a lot of new variables, let's look at the distribution of these values. There are two 
#main ways to do this, through the use of summary statistics and through visualizations. Let's start with 
#some basic summary statistics.


#Using summary()

#If we want to look at all variables within a data frame, we can simply call summary()
#on the entire data frame to look at numerical summaries of each variable.

summary(merged_data)

#Try using summary with a single variable only!

summary(merged_data$___)


#Using skim()

#The skim function, part of the skimr package, is a more powerful function 
#to use for summary statistics. Try running the code below - how is it different from summary()?

skim(merged_data)

#Once again, you can use skim on a single variable!

skim(merged_data$___)

#A few interesting things about the skim function are the variable type, n_missing, and complete_rate
#labels. 

#Variable types are very important in most coding languages as they classify variables into different
#groups or 'types.' Different types can do different things, for example you can take the mean (average) 
#of a numerical variable but not a character variable (since you can't take the mean of a word).

#The labels for n_missing and complete_rate show how many missing values each variable has, and the 
#completion rate. So, if there are 100 observations and 10 of them have missing values for ID, the 
#n_missing for ID will display as 10, with complete_rate displaying as 0.90. From the output, it looks
#like we no longer have any missing variables which is good!

#---------Creating Visual Representations: basic---------

#Visualizations in R can be as simple or complex as required. There are countless customization options, 
#so a lot of plotting comes down to personal preference. For the sake of this section, we're just going to 
#go over a few very simple plots.


#Histograms

#Creating histograms in Base R is very simple, as there is a built in hist() function
#A simple histogram may look something like this:

hist(merged_data$methylation_average)

#This plot gives us a general sense of the distribution, but there is a lot of room for improvement
#For example, we may want more bars, a better title, etc. For this, we can look at the documentation of 
#hist().

?hist()

#While this may be hard to read, we can look through some of the basic 'arguments' for more customization.
#After reading through the possibilities and editing our code, we may end up with an updated plot like this:

hist(merged_data$methylation_average, 
     breaks = 20, 
     main = "Distribution of Methylation Averages", 
     xlab = "Values", 
     ylab = "Frequency", 
     col = "lightblue")


#Box plots

#Now using the documentation below, try to create a box plot of the total PSS!

?boxplot()

#You got this!

boxplot(__)

#Scatter plots

#Scatter plots, while possible with one variable (with indexing) are more commonly used with 2 variables:
#an x and a y variable. Let's try making a scatter plot with 2 variables to see a possible relationship.

#Here's the documentation for a scatter plot! If two links pop up, choose the second one.

?plot()

#Now try to create the graph! Remember, we want to specify two variables (choose any 2).

plot(__)

#---------Creating Visual Representations: ggplot---------

#For more advanced plots, we are going to switch to ggplot2 (a sub-package included in tidyverse) 
#for plotting. This package is very powerful for data visualization and offers a lot of customization
#while being relatively simple in syntax.

#Below is an example of a ggplot graph with some commenting on the syntax:

#The first two lines are always necessary for a ggplot to work. We first need to specify the data 
#and variables, and then the desired type of graph. After this, we can add as many additional features 
#as we'd like.

#The 'ggplot' line - always start with this line! We need to specify which data set we are using, and then 
#our x and y
ggplot(data = merged_data, aes(x = Maternal_Age, y = PSS)) +
  #The 'plot' line - specifies what kind of plot we want! Point refers to a scatter plot.
  geom_point()

#Notice how each line within the graph is separated by a + sign. This is unique to ggplot and it's important
#to remember to add them!


#Now let's look at a more advanced graph using the same variables.

#Spend a few minutes trying to figure out what each line of code does.
#Try running this graph multiple times, each time commenting out a line to see how it affects the result. 
#To comment out a line, type a '#' sign in front of it. Also, try adjusting the values!

ggplot(data = merged_data, aes(x = Maternal_Age, y = PSS)) +
  geom_point(alpha = 1, na.rm = T) +
  geom_line(stat = 'smooth', alpha = 1, color = 'purple', na.rm = T) + 
  labs(title = 'Maternal Age vs PSS Score',
       y = 'PSS Score',
       x = 'Maternal Age') + 
  theme_minimal() + 
  ylim(0,15) +
  theme(
    plot.title = element_text(size = 15, face = "italic", color = "purple"),
    axis.title.x = element_text(size = 14, face = 'italic'),
    axis.title.y = element_text(size = 14, face = 'italic'),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13),
    panel.grid.major = element_line(color = "black", linetype = 'dotted'),
    panel.grid.minor = element_line(color = 'black', linetype = 'dotted'),
    panel.border = element_rect(color = "black", fill = NA, size = 1.5)
  ) 

#One thing we're interested in within this data is how methylation is related to stress. Using ggplot,
#try to make a few visualizations regarding the relationship! Below are a few options of plots with 
#their documentation.

?geom_point()    #Scatter plot, as shown above

?geom_violin()   #Violin plot, shows the distribution and density of a continuous variable

?geom_col()      #Bar chart


#Write your code below! Take some time going through the possible settings and experiment with some 
#interesting options. Also, don't be disappointed by errors! More than half of the time spent coding is 
#spent debugging (unfortunately), so getting used to combating errors is a good thing!

___

#Another useful thing about ggplot is that we can pair it with tidyverse to streamline data manipulation 
#and visualization into one code block. While I won't go too much into the capabilities of tidyverse in this
#tutorial, it is a very helpful package for combining operations.

#Some important things to note within this package are the filter() command, as well as the pipe. Filter works
#in a pretty self explanatory way: we specify a condition that will filter our data to observations with that 
#condition met. So if we say filter(Maternal_Age > 50), we will only be left with observations with age > 50.
#The pipe (%>%) is a tool that connects lines of code together. By including a pipe at the end of a line, the next 
#line will be ran immediately afterwards, connected to the same data if possible. So if our data frame is specified
#at the beginning, we could use the pipe to run an infinite number of modifications/filters without needing
#to specify the data frame again. Don't worry if this is confusing, this tutorial is mostly tidyverse-free!


#If we look at the code block below, we can see that the data frame is specified at the beginning, making it not
#needed in the ggplot line. Because of this, we can alter the data frame BEFORE the plot, adding in customization
#without needing to actually save another data frame to our memory. In other words, we are temporarily filtering
#our data frame here to individuals with high PSS, only for the sake of the plot. If we look back at our merged_data
#data frame, we will see that nothing is changed after running this.

#This code filters the data to only look at those with high PSS, and creates a scatter plot of methylation and age
#for those individuals.

merged_data %>%
  filter(high_PSS == 1) %>%
  ggplot(aes(x = Maternal_Age, y = methylation_average)) +       #Notice how we switch from using %>% to +!
  geom_point()


#Try using the format above to make a filtered plot! It can be of any variable(s), just make sure to use the filter
#command.

___

#---------Quantitative Relationships: correlations and t-tests---------

#So far, we've only looked at some basic summary statistics and visualizations. To further our analysis, 
#we are now going to look at some more impactful quantitative values.

#A good tool for determining the relationship between variables is a correlation matrix. This allows us 
#to see how 'related' each variable is to each other, on a scale from -1 to 1. A positive coefficient 
#indicates that two variables have a positive relationship, meaning the variables tend to move in the same 
#direction (increasing one is generally tied to an increase in the other). A negative coefficient indicates 
#a negative relationship, meaning the variables tend to move in inverse directions (increasing one generally 
#decreases the other). A coefficient of 0 indicates no relationship between the variables (they are not 
#correlated).

#To run correlations in R, we are going to use corr.test, a function present within the psych package. 
#This will give us the correlation coefficients as well as the associated p-values. A low p-value 
#(typically lower than 0.05) indicates a significant effect, or in this case, correlation!

#To calculate correlations, we need to make sure everything is numeric. Because of this, we want the data 
#frame that we are going to feed into the function to only have numeric types (remember how the skim function 
#gave us types earlier). To make a new data frame, we're going to modify an existing data frame, while 
#selecting certain variables. For now, let's just look at average methylation, the differential between 
#time points, and maternal age.

#Run the code below to create cor_df (correlation data frame), which uses brackets once again to select 
#certain columns. By viewing this data frame, we can see that it successfully got rid of the other variables!
cor_df <- merged_data[, c("methylation_average", "differential", "Maternal_Age")]


#Now that we have our data ready, let's run the correlation! There are two main methods of correlations that 
#we can run: spearman and pearson. Pearson correlations assume that variables are normally distributed and measure
#the linear relationship between them. Spearman correlations do not assume normality and instead focus on the
#strength of the directional relationship. Both methods may result in similar values, but it's good practice
#to use the correct one!

#To see if the chosen variables are roughly normal, we can look at their distributions via histograms! There are other
#ways to do this, such as the use of Q-Q plots, but we'll cover those in the next section. What we're looking for in 
#our histograms are roughly symmetrical distributions, with higher counts towards the middle and smaller counts at the
#ends. Make a histogram for each variable (of the three included in cor_df) and determine if they're roughly normal!
#As long as the distributions aren't too asymmetrical, we can use Pearson as our method for this correlation.

___

#Now, run the correlation with the appropriate method. If all 3 plots are symmetrical, write 'pearson' within the
#function. Otherwise, write 'spearman'

corr.test(cor_df, method = "___")

#As you can see, the output shows the correlation, sample size, and p-value between ALL permutations of 
#our variables. However, it may be a bit difficult to view the output in this form. Because of that, we're 
#going to save each section into a saved table. To do this, we are going to reference the sections, sort of 
#like how we've been referencing variables.

#First, start by saving the correlation to a name, like how we saved our data earlier. For the next part to 
#work, name it: 'correlation_full'

____

#Now, try running the following three lines of code! If they produce an error, there is most likely an issue 
#with the saving of the correlation.

coefficients <- correlation_full$r
p_values <- correlation_full$p
sample_size <- correlation_full$n

#Look at the resulting matrices via the top right window. By specifying different aspects of the correlation 
#output, we can end up with only what we are interested in! A lot of objects (such as lists, plots, and 
#regressions) work in a similar way, in which we can reference specific attributes.


#Your turn! Run a correlation between average methylation and stress scores (you can add more variables in as well).
#Hint: this will require you to make a new data frame and check for normality!

____


#Another quantitative tool we can use is a t-test! A t-test simply compares the means between variables,
#determining if the difference is statistically significant or not (through p-values). For our data set,
#we can look at the methylation between time points.

#To run the test, we are going to use 't.test', in which we specify 2 variables of interest to compare.
#When comparing 2 variables, we can either specify an 'independent' t-test, or a 'paired' one. An independent
#test is used when we are looking at two, completely independent groups. For example, a control versus
#a treatment group. A paired t-test is used when there is only one group that takes 2 measures. In our case,
#we are looking at a paired test because each individual is tested at two time points.

#t.test will automatically assume that groups are independent. Since we are using paired data, we need to
#specify this! After running the code below, try and figure out what the output means!

t.test(merged_data$Point1, merged_data$Point2, paired = TRUE)

#---------Running Linear Regressions---------

#Now that we've seen some of the relationships of our variables, the next step is to establish significance
#To do this, we can run a linear regression to see if our variables are significantly affecting one another.

#To run a simple linear regression, we can use the format below, in which our first variable 
#(methylation_average) is our dependent and all subsequent variables are our independents. In other words, 
#we are trying to see if the variables after the first significantly affect the first variable. For example,
#it might seem plausible that stress impacts methylation so let's see!

#Run this line first to save the regression.

linear_regression_basic <- lm(methylation_average ~ PSS, data = merged_data)

#Then, run this to see the output! This output shows significance and R-squared among other things.

summary(linear_regression_basic)

#Based on the output, it looks like there is significance between PSS and methylation! We can tell based on
#the two stars to the right of PSS, indicating a p-value of less than 0.01. Different symbols indicate different
#levels of significance, shown in the 'Signif. codes' section. The estimate to the right of PSS indicates the 
#value of the linear relationship, showing how average methylation changes when we increase PSS by 1 unit.

#So in other words, this regression tells us that increasing total PSS by 1 unit will generally increase
#the average methylation of an individual by 0.22862 units! This is a positive relationship between the two.


#Let's also look at the residuals via a scatter plot. If the residuals (errors) are randomly scattered 
#around 0, then that's ideal. If the plot shows a clear pattern, then our model may not be reliable.

plot(fitted(linear_regression_basic), residuals(linear_regression_basic),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot",
     abline(h = 0, col = "red", lty = 2))

#The points in the plot look relatively random (no apparent pattern), so that's good!


#We can also check for normality within our residuals via a Q-Q plot (quantile-quantile plot). Simply put,
#this plot compares the quantiles of residuals with those of a normal distribution. If the residuals
#fall along a straight line, then the residuals are roughly normal (which is good!)

qqnorm(linear_regression_basic$residuals)
qqline(linear_regression_basic$residuals)

#Once again, this plot looks great! The residuals seem to be normal, adding validity to our regression.


#We can also use reference groups within our linear regressions to compare specific values of a variable,
#impacting the estimates. When using non-numeric variables, our 'estimate' refers to the difference between
#our value and a reference value. To see this, run the code below and note the impact that being male has 
#on average methylation.

linear_regression_male <- lm(methylation_average ~ Gender, data = merged_data)
summary(linear_regression_male)

#By looking at the output, we can see that it says 'GenderMale' for the coefficient. This actually means that
#we are seeing the effect of being male COMPARED to being female. In other words, this is telling us that male
#participants have a 0.13 higher methylation average compared to females in our data set (though this is not
#significant at all).

#However, sometimes we may want to see the opposite effect. In our case, we may be interested in the effect of
#being female relative to male. While we could simply take the inverse of the estimate in our case, this can be 
#confusing when our variables have a lot of possible values. Another way to do this is through modifying the code
#itself. First, we need to convert the variable of interest into a 'factor' type (remember how we mentioned variable
#types before). Then, we can manually change the reference within our regression (in this case we set it equal to
#"male")!

#Run the code below and note the change in the estimate! This format may be useful for when you analyze your own 
#data!

merged_data$Gender <- factor(merged_data$Gender)
linear_regression_female <- lm(methylation_average ~ relevel(Gender, ref = "Male"), data = merged_data)
summary(linear_regression_female)


#While we just looked at a simple linear regression, we also might be interested in a more complex 
#relationship between methylation and stress. To model this, we are going to look at some possible 
#'confounders' that may be impacting the relationship. For example, maternal age may significantly be 
#associated to methylation AND stress, so it might be necessary to CONTROL for age within our regression. 
#This way, we can be sure that our findings are truly showing the relationship between methylation and 
#stress, and not uncovering a hidden effect of age. To model this, we use the same formula as before, and 
#simply in add our controls.

linear_regression_full <- lm(methylation_average ~ PSS + control1 + control2 + ..., data = merged_data)

#Looking at the data frame yourself, try and hypothesize a few variables that might be possible confounders, 
#and add them to the model. Analyze the findings from the regression (via summary), and then look at a 
#residual plot and Q-Q plot to make sure the regression is suitable.

#Write your code below!

___

#Results:


#---------Putting it all together---------

#Hopefully by now, you should have your own data to work with! Using the techniques we have used throughout this
#walk-through, analyze the data yourself! Merge the files together, make some variables, and run some visualizations
#and quantitative tests! Feel free to stick to the format used above or explore different alternatives!

___

#---------Conclusion---------
#And that's all we have for this workshop! R is an extremely powerful tool for data analysis, and this 
#walk-through only covered a few, very simple applications of the program. Even if you're not interested 
#in coding at all, exposure to various quantitative techniques is always beneficial, especially for 
#interpreting complex results and methodologies.

#If you have any questions about coding in R, my experience with data analysis, or any of the projects 
#I'm apart of, feel free to reach out to my email! - joliner@ucsd.edu

#Otherwise, I hope you found this interesting! 

