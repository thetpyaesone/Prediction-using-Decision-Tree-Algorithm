"NAME : THET PYAE SONE AUNG"
"TASK:  Create the Decision Tree classifier and visualize it graphically."
"TITLE  : Data Science and Business Analytics Tasks"


install.packages("caret")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("DataExplorer")
install.packages("tidyverse")
install.packages("ClusterR")
install.packages("cluster")
install.packages("rpart.plot")
install.packages("rpart")
install.packages("caTools")
install.packages("randomForest")
library(randomForest)
library(caTools)
library(rpart)
library(ClusterR)
library(cluster)
library(caret)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(tidyverse)
library(rpart.plot)

#Upload Data

Iris <- read_csv("C:/Users/DELL/Downloads/Iris (1).csv")
Iris<-select(Iris,-Id)
View(Iris)

#Visualizations Between class and features

plot_missing(Iris)
plot_histogram(Iris)
dim(Iris)
plot_correlation(Iris)
qplot(Species,SepalWidthCm,data=Iris,geom="boxplot",color=Species)
qplot(Species,SepalLengthCm,data=Iris,geom="boxplot",color=Species)
qplot(Species,PetalLengthCm,data=Iris,geom="boxplot",color=Species)
qplot(Species,PetalWidthCm,data=Iris,geom="boxplot",color=Species)

# Splitting the data in Train and Test

sample <- sample.split(Iris$Species, SplitRatio = 0.7)
train_data <- subset(Iris, sample == TRUE)
test_data <- subset(Iris, sample == FALSE)
View(train_data)
View(test_data)

#Build the model

fit <- rpart(Species~., data = train_data, method = 'class')
fit
      "/n= 105 
      
      node), split, n, loss, yval, (yprob)
      * denotes terminal node
      
      1) root 105 70 Iris-setosa (0.3333333 0.3333333 0.3333333)  
      2) PetalLengthCm< 2.6 35  0 Iris-setosa (1.0000000 0.0000000 0.0000000) *
        3) PetalLengthCm>=2.6 70 35 Iris-versicolor (0.0000000 0.5000000 0.5000000)  
      6) PetalLengthCm< 4.95 39  4 Iris-versicolor (0.0000000 0.8974359 0.1025641)  
      12) PetalWidthCm< 1.55 32  0 Iris-versicolor (0.0000000 1.0000000 0.0000000) *
        13) PetalWidthCm>=1.55 7  3 Iris-virginica (0.0000000 0.4285714 0.5714286) *
        7) PetalLengthCm>=4.95 31  0 Iris-virginica (0.0000000 0.0000000 1.0000000) *
/"

# Summary for model

summary(fit)

      "/Call:
        rpart(formula = Species ~ ., data = train_data, method = "class")
      n= 105 
      
                CP nsplit  rel error     xerror       xstd
      1 0.50000000      0 1.00000000 1.27142857 0.05260927
      2 0.44285714      1 0.50000000 0.77142857 0.07316262
      3 0.01428571      2 0.05714286 0.07142857 0.03117398
      4 0.01000000      3 0.04285714 0.07142857 0.03117398
      
      Variable importance
      PetalLengthCm  PetalWidthCm SepalLengthCm  SepalWidthCm 
      35            33            21            12 
      
      Node number 1: 105 observations,    complexity param=0.5
      predicted class=Iris-setosa      expected loss=0.6666667  P(node) =1
      class counts:    35    35    35
      probabilities: 0.333 0.333 0.333 
      left son=2 (35 obs) right son=3 (70 obs)
      Primary splits:
        PetalLengthCm < 2.6  to the left,  improve=35.00000, (0 missing)
      PetalWidthCm  < 0.75 to the left,  improve=35.00000, (0 missing)
      SepalLengthCm < 5.45 to the left,  improve=23.07971, (0 missing)
      SepalWidthCm  < 3.15 to the right, improve=10.79491, (0 missing)
      Surrogate splits:
        PetalWidthCm  < 0.75 to the left,  agree=1.000, adj=1.000, (0 split)
      SepalLengthCm < 5.45 to the left,  agree=0.914, adj=0.743, (0 split)
      SepalWidthCm  < 3.35 to the right, agree=0.800, adj=0.400, (0 split)
      
      Node number 2: 35 observations
      predicted class=Iris-setosa      expected loss=0  P(node) =0.3333333
      class counts:    35     0     0
      probabilities: 1.000 0.000 0.000 
      
      Node number 3: 70 observations,    complexity param=0.4428571
      predicted class=Iris-versicolor  expected loss=0.5  P(node) =0.6666667
      class counts:     0    35    35
      probabilities: 0.000 0.500 0.500 
      left son=6 (39 obs) right son=7 (31 obs)
      Primary splits:
        PetalLengthCm < 4.95 to the left,  improve=27.820510, (0 missing)
      PetalWidthCm  < 1.65 to the left,  improve=25.904610, (0 missing)
      SepalLengthCm < 5.75 to the left,  improve= 7.329060, (0 missing)
      SepalWidthCm  < 2.95 to the left,  improve= 2.878289, (0 missing)
      Surrogate splits:
        PetalWidthCm  < 1.75 to the left,  agree=0.886, adj=0.742, (0 split)
      SepalLengthCm < 6.25 to the left,  agree=0.757, adj=0.452, (0 split)
      SepalWidthCm  < 2.95 to the left,  agree=0.643, adj=0.194, (0 split)
      
      Node number 6: 39 observations,    complexity param=0.01428571
      predicted class=Iris-versicolor  expected loss=0.1025641  P(node) =0.3714286
      class counts:     0    35     4
      probabilities: 0.000 0.897 0.103 
      left son=12 (32 obs) right son=13 (7 obs)
      Primary splits:
        PetalWidthCm  < 1.55 to the left,  improve=3.7509160, (0 missing)
      PetalLengthCm < 4.75 to the left,  improve=1.8134160, (0 missing)
      SepalLengthCm < 6.15 to the right, improve=0.3646724, (0 missing)
      SepalWidthCm  < 2.45 to the left,  improve=0.1794872, (0 missing)
      Surrogate splits:
        SepalWidthCm  < 3.15 to the left,  agree=0.897, adj=0.429, (0 split)
      PetalLengthCm < 4.75 to the left,  agree=0.846, adj=0.143, (0 split)
      
      Node number 7: 31 observations
      predicted class=Iris-virginica   expected loss=0  P(node) =0.2952381
      class counts:     0     0    31
      probabilities: 0.000 0.000 1.000 
      
      Node number 12: 32 observations
      predicted class=Iris-versicolor  expected loss=0  P(node) =0.3047619
      class counts:     0    32     0
      probabilities: 0.000 1.000 0.000 
      
      Node number 13: 7 observations
      predicted class=Iris-virginica   expected loss=0.4285714  P(node) =0.06666667
      class counts:     0     3     4
      probabilities: 0.000 0.429 0.571 
      
      /"

#Plotting the Decision Tree

rpart.plot(fit,extra = 106)

"Warning message:
  extra=106 but the response has 3 levels (only the 2nd level is displayed) 

#Predict the Model "


predict(fit,test_data,type = "class")

"/
1               2               3               4 
Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa 
5               6               7               8 
Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa 
9              10              11              12 
Iris-setosa     Iris-setosa     Iris-setosa     Iris-setosa 
13              14              15              16 
Iris-setosa     Iris-setosa     Iris-setosa Iris-versicolor 
17              18              19              20 
Iris-versicolor Iris-versicolor Iris-versicolor Iris-versicolor 
21              22              23              24 
Iris-versicolor Iris-versicolor Iris-versicolor  Iris-virginica 
25              26              27              28 
Iris-versicolor  Iris-virginica Iris-versicolor Iris-versicolor 
29              30              31              32 
Iris-versicolor Iris-versicolor  Iris-virginica  Iris-virginica 
33              34              35              36 
Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica 
37              38              39              40 
Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica 
41              42              43              44 
Iris-virginica  Iris-virginica  Iris-virginica  Iris-virginica 
45 
Iris-virginica 
Levels: Iris-setosa Iris-versicolor Iris-virginica
/"

#Making the table with predict class and Species
table_Iris <- table(test_data$Species, predict_class)
table_Iris

"Predicting the class"

predict_class<-predict(fit,test_data,type="class")
predict_class

  "/
  Iris-setosa Iris-versicolor Iris-virginica
  Iris-setosa              15               0              0
  Iris-versicolor           0              13              2
  Iris-virginica            0               0             15
  
  "
#Test the model's accuracy

accuracy_Test <- sum(diag(table_Iris)) / sum(table_Iris)
accuracy_Test
      "[1] 0.9555556"
print(paste('Accuracy for test', accuracy_Test))
     "Accuracy for test 0.955555555555556"

     
     "RANDOM FOREST"
# Fitting Random Forest to the train dataset 
     split <- sample.split(iris, SplitRatio = 0.7) 
     split 
     #[1] FALSE  TRUE  TRUE FALSE  TRUE
      
     train <- subset(iris, split == "TRUE") 
     test <- subset(iris, split == "FALSE") 
     
 # Fitting Random Forest to the train dataset 
     set.seed(120)  # Setting seed 
     classifier_RF = randomForest(x = train[-5],y = train$Species, ntree = 500) 
     View(train)
     
     classifier_RF
     
     "Call:
       randomForest(x = train[-5], y = train$Species, ntree = 500) 
     Type of random forest: classification
     Number of trees: 500
     No. of variables tried at each split: 2
     
     OOB estimate of  error rate: 3.33%
     
     Confusion matrix:
       setosa versicolor virginica class.error
     setosa         30          0         0  0.00000000
     versicolor      0         29         1  0.03333333
     virginica       0          2        28  0.06666667
     
     "
     y_pred = predict(classifier_RF, newdata = test[-5]) 
     "y_pred
     1          4          6          9         11         14         16 
     setosa     setosa     setosa     setosa     setosa     setosa     setosa 
     19         21         24         26         29         31         34 
     setosa     setosa     setosa     setosa     setosa     setosa     setosa 
     36         39         41         44         46         49         51 
     setosa     setosa     setosa     setosa     setosa     setosa versicolor 
     54         56         59         61         64         66         69 
     versicolor versicolor versicolor versicolor versicolor versicolor versicolor 
     71         74         76         79         81         84         86 
     virginica versicolor versicolor versicolor versicolor  virginica versicolor 
     89         91         94         96         99        101        104 
     versicolor versicolor versicolor versicolor versicolor  virginica  virginica 
     106        109        111        114        116        119        121 
     virginica  virginica  virginica  virginica  virginica  virginica  virginica 
     124        126        129        131        134        136        139 
     virginica  virginica  virginica  virginica  virginica  virginica  virginica 
     141        144        146        149 
     virginica  virginica  virginica  virginica 
     Levels: setosa versicolor virginica"
     
     confusion_mtx = table(test[, 5], y_pred) 
     confusion_mtx
     
     "y_pred
     setosa versicolor virginica
     setosa         20          0         0
     versicolor      0         18         2
     virginica       0          0        20 "
     
      # Plotting model 
     plot(classifier_RF)
      # Importance plot 
     importance(classifier_RF) 
     
     "MeanDecreaseGini
     Sepal.Length         5.772467
     Sepal.Width          1.419426
     Petal.Length        25.728012
     Petal.Width         26.374384"
      
    # Variable importance plot 
    
     varImpPlot(classifier_RF) 
    
     "confusion_mtx
       y_pred
       setosa versicolor virginica
       setosa         20          0         0
       versicolor      0         18         2
       virginica       0          0        20
    "
     "Test the Random Forest Model's Accuracy"
     
     table_Iris_RF <- table(test$Species, y_pred)
     
     table_Iris_RF
     
     "y_pred
     setosa versicolor virginica
     setosa         20          0         0
     versicolor      0         18         2
     virginica       0          0        20"
     
     accuracy_Test_RF <- sum(diag(table_Iris_RF)) / sum(table_Iris_RF)
     accuracy_Test_RF
    # 0.9666667
     
     print(paste('Accuracy for test data in Radom Forest Classification', accuracy_Test_RF))
  
       # [1] "Accuracy for test data in Radom Forest Classification 0.966666666666667"
