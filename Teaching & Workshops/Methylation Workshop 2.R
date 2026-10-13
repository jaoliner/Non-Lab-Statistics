#---------Introduction---------
#Hi there! In this file, we will merge the collected Demographic, PSS, and Initial data and subsequently create some 
#simple visualizations. The code itself is very similar to that of the example data, so the explanations will be
#quite brief. As always, if you have any questions please reach out to one of the lab members running the workshop!

#---------Preparation + Merging---------

#Once again, there are a few steps we need to follow in order to merge our files correctly:

# 1. Load all necessary packages: As mentioned previously, you only have to install packages one time ever! If you have 
#already installed these packages previously, then don't worry about running the install lines.

#For looking at a brief summary of the collected data:
#install.packages('skimr')
library(skimr)

#For plotting the data:
#install.packages('tidyverse')
library(tidyverse)

# 2. Set your working directory: Remember, all of your data should be within the directory you choose. Since all 
#of the data is within my "Downloads" file, I set my directory to the following:

setwd("C:/Users/joliner/Downloads")

#Please change this line to your own directory! All of the files should be csv files, so make sure the format is
#correct before loading them.

# 3. Load all relevant data: Simply run the following lines to load the data into RStudio! If a line results in
#an error, please check your directory to make all files are there, named correctly, and formatted as csv's.

PSS <- read.csv("Perceived Stress Scale 2025 new.csv")
demographics <- read.csv("Demographics.csv")


# 4. Merge the files: In R, the base "merge" function can only merge 2 data frames at a time. Because we have 3
#present, we'll run two lines of code; first merging demographics and initials, and then adding PSS.

#If you look closely at the demographic/initials files, you'll see that they both have the columns 'ID' and 
#'Your.Initials'. #Because of this, we want to merge by both columns to maintain structure (otherwise we may 
#'have duplicates!).

#However, if these files had slightly different participants, we could choose to keep all participants across
#both data frames, or just keep the ones that are present in both. In the code below, I set "all = True", indicating
#that I'd like to keep ALL participants. Run this code and note the ending structure of the merged data. Then, set all
#to FALSE and look again. Ask around and see which one would be preferred.
final_merge <- merge(demographics, PSS, by = c('ID'), all = TRUE)

#And there we go! All of the collected data should be merged :)

#---------Distribution summaries and simple plots---------

#Now that the data is merged, we can analyze it! The following will consist of a simple summary and some
#distribution visualizations. This follows closely to the example code, so look at that file if you have any 
#questions about the formatting!

#Summarizing the data via skim
summary <- skim(final_merge)

#If you look at the "summary" data frame, it may seem a little confusing. This is because the skim function
#analyzes text and numbers differently. While it may be helpful for numeric data (as you can see, it provides
#the mean, quartiles, even a histogram!), it isn't the most helpful when looking at characters. Because of this
#let's create a series of very simple bar plots for each variable of note.

#These plots are very simple, so feel free to augment them if you'd like! If you ended up setting "all=" to TRUE,
#then you may see some "NA" stratifications, which shows the amount of missing information.

#Racial Identity
final_merge %>%
  ggplot() +
  geom_bar(aes(x = What.is.your.racial.identity.))

#Hispanic/Latino Identity
final_merge %>%
  ggplot() +
  geom_bar(aes(x = Are.you.Hispanic.or.Latino.))

#Gender Identity
final_merge %>%
  ggplot() +
  geom_bar(aes(x = To.which.gender.do.you.currently.most.identify.with.))

#First Gen Student?
final_merge %>%
  ggplot() +
  geom_bar(aes(x = Are.you.a.first.generation.college.student.))

#PSS Q1
final_merge %>%
  ggplot() +
  geom_bar(aes(x = factor(In.the.last.month..how.often.have.you.felt.that.you.were.unable.to.control.the.important.things.in.your.life..)))

#PSS Q2
final_merge %>%
  ggplot() +
  geom_bar(aes(x = factor(In.the.last.month..how.often.have.you.felt.confident.about.your.ability.to.handle.your.personal.problems..)))

#PSS Q3
final_merge %>%
  ggplot() +
  geom_bar(aes(x = factor(X.In.the.last.month..how.often.have.you.felt.that.things.were.going.your.way.)))

#PSS Q4
final_merge %>%
  ggplot() +
  geom_bar(aes(x = factor(In.the.last.month..how.often.have.you.felt.difficulties.were.piling.up.so.high.that.you.could.not.overcome.them..)))


#Finally, let's look at a relationship between 2 variables. Suppose we wanted to see a visualization of the relationship
#between being a first generation student and PSS Q1 (inability to control important things); how would that look?

final_merge %>%
  ggplot() +
  geom_point(aes(x = Are.you.a.first.generation.college.student., y = In.the.last.month..how.often.have.you.felt.that.you.were.unable.to.control.the.important.things.in.your.life..))

#As you can see, without much data it can be hard to establish meaningful relationships. And that's the end of this short file!
#Please reach out to a lab member if you have any questions :)

#---------Additional Notes - Renaming variables + If/Else(8/21)---------
#By this point, you have probably noticed that the names and levels of the variables are quite long. So, let's shorten them!
#To rename a variable, simply follow the format below. We use the pipe operator (%>%) to directly modify a data frame,
#overwriting the previous data frame with updated names. Feel free to add new names for the PSS questions too!
final_merge <- final_merge %>%
  rename(race = What.is.your.racial.identity.,
         hispanic_or_latino = Are.you.Hispanic.or.Latino.,
         gender = To.which.gender.do.you.currently.most.identify.with.,
         first_gen = Are.you.a.first.generation.college.student.)


#We can also rename the levels of the variables themselves. For this example, we will rename the levels of gender.
#To do this, we need to follow a few steps:

#Start by converting the variable to a factor. This will allow the variable to be treated as categorical with a defined order.
final_merge$gender <- factor(final_merge$gender)

#Then, check the current levels of the variable. Since we will rename all levels at a time, it is important for us to match
#the order with our naming convention. For example, we can see that the order starts with "Female/Woman". Thus, it is important
#for our naming scheme to start with "Female" as each level will be replaced by our designation.
levels(final_merge$gender)

#Now, replace all of the previous levels with our own, shortened version!
levels(final_merge$gender) <- c("Female", "Male", "Non-binary", "Trans-Masc")


#Finally, let's work through the process of converting a variable to binary. Sometimes, we may be interested in dichotomizing 
#a variable for future analyses, so being able to bin can be very helpful!

#Let's look at PSS Q1, regarding the inability to control important things. Say we are interested in high vs. low scores, with
#low scores represented by values of 1 and 2, and high scores represented by values fo 3-5. The code for this is very simple,
#consisting of one single line and an if/else statement. Try and work out the logic of this function; how does it work?
final_merge$Q1_binary = ifelse(
  final_merge$In.the.last.month..how.often.have.you.felt.that.you.were.unable.to.control.the.important.things.in.your.life..
                               > 2, 1, 0)

#NOTE: If you have already renamed this question previously, then the provided code will result in an error. Please rename the
#variable name with the newly created name if so!
