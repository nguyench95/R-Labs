########################################################
# Name: Christopher Nguyen
# CSC-315
# Lab #2: Graphical and Numerical Summaries of Data
########################################################

##########################################################################
# Add R code to the script below and create a Notebook to complete
# the steps and explicitly answer the following questions.
# Your Notebook include output showing the requested tables and
# graphs, and answers to questions should be provided in comments.
# Also, all graphs must be given an appropriate title, x-axis label, and
# y-axis label. The ggplot2 library must be used to generate all
# graphs unless stated otherwise.
##########################################################################
library(dplyr)
library(ggplot2)

# 1.load our classes survey data 
#   (available at https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv)
#   and add the code for this to the script. 

library(readr)
survey <- read_csv("https://gdancik.github.io/CSC-315/data/datasets/CSC-315_survey.csv")
View(survey)

# 2. How many students completed the survey?

nrow(survey)

# 3. How many questions were asked (i.e., how many columns are there)?

ncol(survey)

# 4. Construct a frequency table for the response to whether someone is a
#    'Cat' or 'Dog' person.

t <- table(survey$CatOrDogPerson)
t

# 5. Construct a frequency bar graph for the response to "Are you a cat or a dog person?",
#    where the bars are colored using the default colors. Remove the legend by 
#    adding the following component to the end of your 
#    ggplot() code: theme(legend.position = "none")
counts <- data.frame(t)
counts
ggplot(counts) + geom_col(aes(x=Var1, y=Freq)) +
ggtitle("Cat or Dog Person") +
labs(x = "Cat or Dog", y = "No. of People") + theme_bw()
ggplot() code: theme(legend.position = "none")

# 6. Construct a relative frequency table for favorite CSC course. 
#     What proportion of students said that CSC-340 was their 
#     favorite? Note that the two missing values, denoted with the 
#     keyword NA, are correctly ignored by R when the tables are 
#     constructed using what was covered in class.
c <- table(survey$FavoriteCourse)
prop.table(c)
p <- c/sum(c)
p[7]


# 7. Construct a Pareto Chart using the frequencies for favorite CSC course (you may display
#    either frequency or relative frequency). Note: you may do this using
#    either 'geom_bar' with the raw data or 'geom_col' to work directly
#    with the frequencies or relative frequencies. However, if you use
#    geom_bar, then the missing values will be included in the graph. 
#    If you want to remove the missing values, you can use the 'drop_na'
#    function from the 'tidyverse' package. What course or courses were most
#    commonly listed as the favorite?
levels(counts$Freq)
counts
counts$Freq <- reorder(counts$Freq, -counts$Var1)

ggplot(counts,aes(x=Var1, y=Freq)) + 
  geom_col(aes(fill = Freq)) +
  ggtitle("Favorite CSC Course") +
  labs(x = "Courses", y = "Frequency") + theme_classic()

# 8. Construct a relative frequency table for whether or not a student consumes alcohol
#    at least 1 day per week, on average (i.e., consumes alcohol > 0 days per week).
#    Do this by first creating a logical vector where TRUE corresponds to consuming
#    alcohol and FALSE corresponds to does not consume alcohol. Then create a relative
#    frequency of these TRUE and FALSE values. Tables and relative frequency tables
#    are stored as named vectors (e.g., x <- c(item1 = 1, item2 = 2)). Use the 'names' 
#    function to change the names from FALSE and TRUE to "Consumes alcohol" and
#    "Does not consume alcohol"
a <- table(survey$Alcohol>0)
prop.table(a)
names(a)
names(a) <-  TRUE
a
#I have no idea how this logically changed the names how I wanted but it did

# 9. Out of the people who heard "Laurel" in this class, would they rather fight one 
#    horse-sized duck or one hundred duck-sized horses? Answer this question by using
#    dplyr's 'filter' function to create a new data.frame for those who heard "Laurel". 
#    Then generate a relative frequency table for the 'Fight' column results. 
#    Repeat the analysis to answer the same question for those who heard "Yanny"
#    What do you conclude about a person's choice regarding the "Fight" question?

fight <- filter(survey, YannyOrLaurel == "Laurel")
fightA <- table(fight$Fight)
prop.table(fightA)
fight2 <- filter(survey, YannyOrLaurel == "Yanny")
fightB <- table(fight2$Fight)
prop.table(fightB)
fightA
fightB
#Based on this comparison alone, people who heard "Laurel" are more likely to wanna fight 
#a hundred duck-sized horses
#while people who heard "Yanny" are more likely to prefer to fight a single horse-sized duck

# 10. Construct a histogram for Alcohol consumption, by using the hist() function with the argument
#     breaks = 14 to set the number of groupings. Describe the shape of its distribution. 
#     Is it unimodal, bimodal, or flat. Is it skewed right, skewed left, or symmetric?

drunks <- hist(survey$Alcohol, breaks=14, 
     main="Histogram for Alcohol Consumption", 
     xlab="Amount of Alcohol Consumed")
drunks

#skewed left

# 11. Calculate the mean and median for Alcohol consumption. 
#     Which is a better measure of averages? (Note: although these numbers are similar,
#     one would still be considered better than the other -- why?)
avgDrunks <- c(survey$Alcohol)
mean(avgDrunks)
median(avgDrunks)

#The median is closer to measure the average of this data in a rare circumstance.
#The data results are so one-sided that more than half the results were the same.
#The mean calculation takes account of extreme outlyers, causing the result to be warped

# 12. What is the 75th percentile for HS GPA??

hsGPA <- c(survey$hsGPA)
quantile(hsGPA,c(.75))

# 13. Ten percent of indivduals have HS GPAs above what value?
n <- 10
top10th <- survey[survey$hsGPA > quantile(survey$hsGPA, prob=1-n/100),]
top10th$hsGPA


# 14. Create side-by-side boxplots showing the average hours of sleep 
#     based on a person's gender, and answer the questions below:
#     (a) Does there appear to be a difference in the 'median' amount of 
#         sleep between those who identify as 'Female' compared to 'Male'.
#     (b) What does the difference in the boxes indicate?
#         groups? 
#     (c) Are there any outliers? If so, how many?

survey$Gender <- factor(survey$Gender)
levels(survey$Gender) <- c("Male", "Female", "Other or prefer not to say")

bplot <- ggplot(Sleep) + 
  geom_boxplot(aes(Gender, Sleep, fill = Gender))
bplot
bplot + theme_classic() + theme(legend.position = "none") +
  ggtitle("Comparison of sleep between males and females") +
  labs(x = "gender", y = "hours of sleep")

#my boxplot was working but for some reason it 
#stopped working after I cleared my objects and ran the whole code again

# 15. Adding a 'facet_grid' creates multiple plots by
#     splitting up your data based on one or more 
#     variables. Use ggplot to construct side-by-side 
#     boxplots comparing hsGPA with gender, then add
#               + facet_grid(rows = vars(YannyOrLaurel))
#     in order to generate two sets of boxplots, one for
#     the people who heard 'Yanny' and one for the 
#     people who heard 'Laurel'. What is the relationship
#     between hsGPA between males and females, and how
#     does this relationship differ between the 'Laurel' 
#     and 'Yanny' groups?

survey$Gender <- factor(survey$Gender)
levels(survey$Gender) <- c("Male", "Female", "Other or prefer not to say")

bplot <- ggplot(hsGPA) + 
  geom_boxplot(aes(Gender, hsGPA, fill = Gender))
bplot
bplot + theme_classic() + theme(legend.position = "none") +
  ggtitle("Comparison of GPA and Gender") +
  labs(x = "gender", y = "HIGHSCHOOL GPA") + facet_grid(rows = vars(YannyOrLaurel))

# 16. For college GPA, what is the variance and standard deviation?
x <- c(survey$collegeGPA)
plot.deviations <- function(x) {
  plot(x, 1:length(x), xlab = "observed value", ylab = "index", pch = 19)
  m = mean(x)
  abline(v = m, col = "red", lwd = 2)
  for (i in 1:length(x)) {
    p1 = c(x[i],m)
    p2 = c(i,i)
    lines(p1,p2, col = "blue")
  }
}
x.var <- var(x)
x.sd <- sd(x)  
x.var
x.sd

# 17. Create a vector with 20 values that has a standard deviation of 0.
z <- c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
var(z)
sd(z)

#not entirely sure what you're asking for this one