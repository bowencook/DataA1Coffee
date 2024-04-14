#1. obtain and import the dataset from Kaggle 
# import downloaded data set Coffee quality
library(readr)
Coffee_Qlty <- read_csv("Desktop/Assignment/Coffee_Qlty.csv")
View(Coffee_Qlty)

#2. Compute basic descriptive statistics
summary(Coffee_Qlty)
#get frequency for categorical variables
table(Coffee_Qlty$Species)
table(Coffee_Qlty$Continent.of.Origin)
table(Coffee_Qlty$Color)
table(Coffee_Qlty$Processing.Method)
#get standard deviations for continuous variables
sd(Coffee_Qlty$Aroma)
sd(Coffee_Qlty$Aftertaste)
sd(Coffee_Qlty$Flavor)
sd(Coffee_Qlty$Acidity)
sd(Coffee_Qlty$Body)
sd(Coffee_Qlty$Balance)
sd(Coffee_Qlty$Uniformity)
sd(Coffee_Qlty$Clean.Cup)
sd(Coffee_Qlty$Sweetness)
sd(Coffee_Qlty$Moisture)
sd(Coffee_Qlty$Quakers)
sd(Coffee_Qlty$Category.One.Defects)
sd(Coffee_Qlty$Category.Two.Defects)

#3.Clean and process the data - removal of outliers from the variable aroma was conducted.See under 4.

#4. Generate appropriate plots (e.g., bar plot, line plot) to visualise the data
install.packages("ggplot2") #to install packages in your R studio
install.packages("ggpubr")
library(ggplot2) # this retrieves the packages so they are ready to use
library(ggpubr)

#barplots to visualize level of aroma for species and continent of origin
barplot <- ggplot(Coffee_Qlty, aes(x=Species, y=Aroma)) + 
  geom_bar(stat = "identity") 
barplot#to print the plot -> This is before the outlier is removed hence not added in the report but the cleaned data is Figure 2 in your report

barplot1 <- ggplot(data = subset(Coffee_Qlty, !is.na(Continent.of.Origin)), aes(x=Continent.of.Origin, y=Aroma, fill = Species)) + 
  geom_bar(stat = "identity", position="dodge") +
  labs(y= "Aroma", x = "Continent of Origin")
barplot1 # Before removing the outlier (after removing it will be figure 3 in you report) I didnt add it in as the cleaned data is most presentable.

#scatterplots to assess correlation of continuous variables (Figure 1-4 in your technical report)
scatterplot <- ggplot(Coffee_Qlty, aes(x=Aftertaste, y=Aroma)) +
  geom_point() 
scatterplot#to print the plot

scatterplot1 <- ggplot(Coffee_Qlty, aes(x=Sweetness, y=Aroma)) +
  geom_point() 
scatterplot1

scatterplot2 <- ggplot(Coffee_Qlty, aes(x=Body, y=Aroma)) +
  geom_point() 
scatterplot2

scatterplot3 <- ggplot(Coffee_Qlty, aes(x=Acidity, y=Aroma)) +
  geom_point() 
scatterplot3

#due to some outliers the tests are conducted again. To remove any outliers:
Q <- quantile(Coffee_Qlty$Aroma, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(Coffee_Qlty$Aroma)
up <-  Q[2]+1.5*iqr   
low<- Q[1]-1.5*iqr
eliminated<- subset(Coffee_Qlty, Coffee_Qlty$Aroma > (Q[1] - 1.5*iqr) & Coffee_Qlty$Aroma < (Q[2]+1.5*iqr))

#barplots
barplot <- ggplot(eliminated, aes(x=Species, y=Aroma)) + 
  geom_bar(stat = "identity") 
barplot #barplot after removal of the outlier Figure 2

bar <- ggplot(data = subset(eliminated, !is.na(Continent.of.Origin)), aes(x=Continent.of.Origin, y=Aroma)) + 
  geom_bar(stat = "identity") +
  labs(y= "Aroma", x = "Continent of Origin")
bar #bar plot figure called After removal of outlier


barplot1 <- ggplot(data = subset(eliminated, !is.na(Continent.of.Origin)), aes(x=Continent.of.Origin, y=Aroma, fill = Species)) + 
  geom_bar(stat = "identity", position="dodge") +
  labs(y= "Aroma", x = "Continent of Origin")
barplot1 #bar plot figure 3

#scatterplots (figure 5-8 in the technical report)

scatter <- ggplot(eliminated, aes(x=Aftertaste, y=Aroma)) +
  geom_point() + 
  stat_cor(method = "pearson")
scatter  

scatter1 <- ggplot(eliminated, aes(x=Sweetness, y=Aroma)) +
  geom_point() + 
  stat_cor(method = "pearson")
scatter1

scatter2 <- ggplot(eliminated, aes(x=Body, y=Aroma)) +
  geom_point() + 
  stat_cor(method = "pearson")
scatter2

scatter3 <- ggplot(eliminated, aes(x=Acidity, y=Aroma)) +
  geom_point() + 
  stat_cor(method = "pearson")
scatter3

#5. Interpret the descriptive statistics and graphs to summarize the main features 
#and patterns of the data (e.g., trends, variability, outliers, and relationships between variables).
#. see report

#6. Select appropriate inferential statistics to test hypotheses or make predictions about the population 
# based on the sample data (if possible).
t.test(Aroma ~ Species, data=eliminated)#to investigate for any significant difference between the mean score of Aroma and Species group.
#The mean Aroma score is higher in the Robusta species than in the Arabica species, with a p-value = 0.0212.


lmAroma = lm(Aroma ~ Aftertaste + Acidity + Sweetness + Body, data = Coffee_Qlty)
summary(lmAroma) #mulitple linear regression model before removal of outliers

lmAroma = lm(Aroma ~ Aftertaste + Acidity + Sweetness + Body, data = eliminated)
summary(lmAroma) #multiple linear regression model after removal of outliers. This looks at Aroma as a function of the 
#included covariates. For each units increase of each of the covariates the score Aroma increases with the value in column Estimate.
#For the covariate Aftertaste it means the for every unit increase in Aftertaste, the Aroma score increases with 0.43. In this model
#all included covariates show a statisically significant association with the dependent (outcome) variable.

#7. Discuss any limitations of your work as well as unresolved questions or problems 
#(i.e., technical issues, limitations of the dataset, etc.) that arise from your interpretation. 
#If you have none, then state this clearly in your report.
#. see report

#8. Prepare all required documents for submission (as described above) to summarize the main findings 
#and their implications, and identifies any limitations or future directions for further research.


#9. Check the accuracy and functionality of the R code used to produce the statistics and graphs, 
#and document any errors or bugs encountered.






