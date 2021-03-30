creditcard <- read.csv("1_data/creditcardcut.csv")

#install.packages("ROSE")
#Synthetic Minority Over-Sampling
#

#install.packages("smotefamily") 

library(ROSE)

library(smotefamily)

set.seed(123)
# 70% training 30% for test
dt <- sort(sample(nrow(creditcard), nrow(creditcard)*0.7))
dt

creditcard_train <- creditcard[dt,]
creditcard_test <-  creditcard[-dt,]

# Check
a<- nrow(creditcard_train)
b<- nrow(creditcard_test)
c<- sum(nrow(creditcard_train),nrow(creditcard_test))
d<- c(a/c, b/c)
d #shows 0.7 and 0.3


# Check out test and train
prop.table(table(creditcard_train$Class))
prop.table(table(creditcard_test$Class))

table(creditcard_train$Class)

# can remove some of the majority class


# Balancing a dataset by biasing the training dataset
prop.table(table(creditcard$default))

set.seed(123)
dt <- sort(sample(nrow(creditcard),nrow(creditcard)*.7))
dt

creditcard_train <- creditcard[dt,]
creditcard_test  <- creditcard[-dt,]

a <- nrow(creditcard_train)
b <- nrow(creditcard_test)
c <- sum(nrow(creditcard_train),nrow(creditcard_test))
d <- c(a/c, b/c)
d

prop.table(table(creditcard_test$Class))
prop.table(table(creditcard_train$Class))

##Undersampling method one
n_new <- 337/0.5
# could be 0.6
# 337 are the number of minority cases

#??ovun

undersamp_result1 <- ovun.sample(Class ~.,
                                 data = creditcard_train,
                                 method = "under",
                                 N = n_new ,
                                 seed = 123
)
str(undersamp_result1)
class(undersamp_result1)

#Check the class-balance of the under-sampled dataset
#convert to dataframe as currently an ovun sample
cc_undersamp_train <-  undersamp_result1$data
str(cc_undersamp_train)
class(cc_undersamp_train)

#then can convert to tibbles etc

## Oversampling method 1
# No info lost, duplicates, risks over fitting, training times longer
table(creditcard_train$Class)
n_new2 <- 6551/0.5

oversamp_result1 <- ovun.sample(formula = Class ~. ,
                                data = creditcard_train ,
                                method = "over" ,
                                N = n_new2 ,
                                seed = 123
                                )
str(oversamp_result1)
class(oversamp_result1)

# check the output
cc_oversamp_train <- oversamp_result1$data
class(cc_oversamp_train)
prop.table(table(cc_oversamp_train$Class))

## Random under and over sampling
# specify size and proportion of minority

n_new3 <-  10000
fraud_fraction <- 0.5

samp_result3 <- ovun.sample(formula = Class ~. ,
                               data = creditcard_train ,
                             method = "both" ,
                                  N = n_new3 ,
                                  p = fraud_fraction ,
                               seed = 123
                            )
cc_samp_train <- samp_result3$data
prop.table(table(cc_samp_train$Class))

## Method 4 using SMOTE synthetic minority oversampling technique pronounced smott
# create sythetic minoroty cases by studying actual fraud cases
# random select from  the specified (number) of nearest minorty cases
# lin interpolate using rnd between 0 and 1 between original and selected value to create sythetic
# repeat based on  specification of majority:minority ratio and/or dup_size 

str(creditcard_train)
table(creditcard_train$Class)

n0 <- 6551 ; n1 <- 337 ; r0 <- 0.5
# Non fraud, fraud, desired mix

#check dup_size
ntimes <- ((1-r0)/r0)*(n0/n1)-1

6551/337-1
337*18.43917

# create synthetic dataset
# only works for linear variables, so remove categorical
# watch out for case sensitivity or arguments

smote_output <- SMOTE(X = creditcard_train[,-c(1,2,32,33) ] ,
                      target = creditcard_train$Class , 
                      K = 5 , dup_size = ntimes)

cc_smote_train <-  smote_output$data
colnames(cc_smote_train)[30] <- "Class"

prop.table(table(cc_smote_train$Class))
str(cc_smote_train)
