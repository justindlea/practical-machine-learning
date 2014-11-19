# Practical Machine Learning Project
## Background
Using devices such as Jawbone Up, Nike FuelBand, and Fitbit it is now possible to collect a large amount of data about personal activity relatively inexpensively. These type of devices are part of the quantified self movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. One thing that people regularly do is quantify how much of a particular activity they do, but they rarely quantify how well they do it. In this project, your goal will be to use data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants. They were asked to perform barbell lifts correctly and incorrectly in 5 different ways. More information is available from the website here: http://groupware.les.inf.puc-rio.br/har (see the section on the Weight Lifting Exercise Dataset). 

### Processing
I began by downloading the [training data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv) and [testing data](https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv). Upon inspecting these files, it became apparent that some entries would need to be corrected to NA values:


```r
training <- read.csv("pml-training.csv", na.strings=c("#DIV/0!", "NA", ""))
testing <- read.csv("pml-testing.csv")
```
Much of this data does is either blank and will need to be removed. Our features will be each column which is not NA and represents a measure from a device.

```r
measures <- colnames(training[colSums(is.na(training))==0])
measures
```

```
##  [1] "X"                    "user_name"            "raw_timestamp_part_1"
##  [4] "raw_timestamp_part_2" "cvtd_timestamp"       "new_window"          
##  [7] "num_window"           "roll_belt"            "pitch_belt"          
## [10] "yaw_belt"             "total_accel_belt"     "gyros_belt_x"        
## [13] "gyros_belt_y"         "gyros_belt_z"         "accel_belt_x"        
## [16] "accel_belt_y"         "accel_belt_z"         "magnet_belt_x"       
## [19] "magnet_belt_y"        "magnet_belt_z"        "roll_arm"            
## [22] "pitch_arm"            "yaw_arm"              "total_accel_arm"     
## [25] "gyros_arm_x"          "gyros_arm_y"          "gyros_arm_z"         
## [28] "accel_arm_x"          "accel_arm_y"          "accel_arm_z"         
## [31] "magnet_arm_x"         "magnet_arm_y"         "magnet_arm_z"        
## [34] "roll_dumbbell"        "pitch_dumbbell"       "yaw_dumbbell"        
## [37] "total_accel_dumbbell" "gyros_dumbbell_x"     "gyros_dumbbell_y"    
## [40] "gyros_dumbbell_z"     "accel_dumbbell_x"     "accel_dumbbell_y"    
## [43] "accel_dumbbell_z"     "magnet_dumbbell_x"    "magnet_dumbbell_y"   
## [46] "magnet_dumbbell_z"    "roll_forearm"         "pitch_forearm"       
## [49] "yaw_forearm"          "total_accel_forearm"  "gyros_forearm_x"     
## [52] "gyros_forearm_y"      "gyros_forearm_z"      "accel_forearm_x"     
## [55] "accel_forearm_y"      "accel_forearm_z"      "magnet_forearm_x"    
## [58] "magnet_forearm_y"     "magnet_forearm_z"     "classe"
```

```r
training <- training[measures]
testing <- testing[measures[measures!='classe']]
```

#### Cross Validation

```r
library(caret)
```

```
## Warning: package 'caret' was built under R version 3.1.2
```

```
## Loading required package: lattice
## Loading required package: ggplot2
```

```r
set.seed(916)
inTrain <- createDataPartition(training$classe, p=.7, list=F)

training.train <- training[inTrain,]
training.test <- training[-inTrain,]
```
Use Random Forest to create a model based on the training.train dataset and test its fit.

```r
library(randomForest)
```

```
## Warning: package 'randomForest' was built under R version 3.1.2
```

```
## randomForest 4.6-10
## Type rfNews() to see new features/changes/bug fixes.
```

```r
modFit <- train(classe ~ ., data=training.train, tuneGrid=data.frame(mtry=3),                                trControl=trainControl(method="none")  
                       )
```
Now test model accuracy

```r
pred <- predict(modFit, training.test)
```
Based on the following confusion matrix, it would appear we have a very accurate model.

```r
confusionMatrix(pred, training.test$classe)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction    A    B    C    D    E
##          A 1674    1    0    0    0
##          B    0 1138    0    0    0
##          C    0    0 1026    4    0
##          D    0    0    0  960    0
##          E    0    0    0    0 1082
## 
## Overall Statistics
##                                     
##                Accuracy : 0.999     
##                  95% CI : (0.998, 1)
##     No Information Rate : 0.284     
##     P-Value [Acc > NIR] : <2e-16    
##                                     
##                   Kappa : 0.999     
##  Mcnemar's Test P-Value : NA        
## 
## Statistics by Class:
## 
##                      Class: A Class: B Class: C Class: D Class: E
## Sensitivity             1.000    0.999    1.000    0.996    1.000
## Specificity             1.000    1.000    0.999    1.000    1.000
## Pos Pred Value          0.999    1.000    0.996    1.000    1.000
## Neg Pred Value          1.000    1.000    1.000    0.999    1.000
## Prevalence              0.284    0.194    0.174    0.164    0.184
## Detection Rate          0.284    0.193    0.174    0.163    0.184
## Detection Prevalence    0.285    0.193    0.175    0.163    0.184
## Balanced Accuracy       1.000    1.000    1.000    0.998    1.000
```
The .999 Kappa value represents the out-of-sample error.

### Conlcusion
Based on the accuracy of this model, I fear that some overfitting may have occurred, but I will use the provided code to generate the files and upload them to test my theory.

```r
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

answers <- predict(modFit, testing)

answers
```

```
##  [1] B A B A A E D B A A B C B A E E A B B B
## Levels: A B C D E
```

```r
pml_write_files(answers)
```
After submitting each problem and getting the answer correct for all 20, I am forced to conclude that overfitting did not occur and the model fits well enough to accurately predict the test dataset.
