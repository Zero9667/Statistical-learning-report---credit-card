#install.packages("here", dependencies=TRUE)
#install.packages("GGally",dependencies=TRUE)
#install.packages("ggplot2",dependencies=TRUE)
#install.packages("dplyr",dependencies=TRUE)
#install.packages("ggcorrplot", dependencies = TRUE)
#install.packages("car", dependencies=TRUE)
#install.packages("smotefamily",dependencies=TRUE)
#install.packages("patchwork", dependencies = TRUE)
#install.packages("skimr", dependencies = TRUE)
#install.packages("Metrics", dependencies = TRUE)
#install.packages("boot",dependencies=TRUE)
#install.packages("MASS",dependencies=TRUE)
lib <- c("here","dplyr","ggplot2","GGally","dplyr","ggcorrplot","car","smotefamily","skimr","Metrics","patchwork","boot","MASS")
lapply(lib, library, character.only = TRUE)
training <- read.csv(here("data/processed", "training.csv"))
test<- read.csv(here("data/processed", "test.csv"))
dataset<-training
rownames ( dataset )= dataset [ ,2]
dataset<-dataset[,-2]
dataset<-dataset[,-1]
dataset$acceptance<-dataset$label
dataset<-dataset[,-1]

#Converting the number of days in years

dataset$Birthday_count<-(dataset$Birthday_count/365)*(-1)
dataset$Employed_days<-(dataset$Employed_days/365)*(-1)

#"We are substituting missing values for 'Annual_income' with the average income based on the "Type_occupation" 

dataset$Annual_income <- ifelse(is.na(dataset$Annual_income),
                                ave(dataset$Annual_income, dataset$Type_Occupation, FUN = function(x) mean(x, na.rm = TRUE)),
                                dataset$Annual_income)


#"We are substituting missing values for 'Birthday_count' with the average age based on the "Type_Income" 

dataset$Birthday_count <- ifelse(is.na(dataset$Birthday_count),
                                 ave(dataset$Birthday_count,dataset$Type_Income, FUN = function(x) mean(x, na.rm=TRUE) ),
                                 dataset$Birthday_count
)


#converting Car_Owner in boolean 
dataset <- dataset %>%
  mutate(Car_Owner = ifelse(Car_Owner == "Y", 1, ifelse(Car_Owner == "N", 0, NA)))

#converting Propert_Owner in boolean 
dataset <- dataset %>%
  mutate(Propert_Owner = ifelse(Propert_Owner == "Y", 1, ifelse(Propert_Owner == "N", 0, NA)))

# applicating target encoding to transform categorical variables in numerical 
dataset <- dataset %>%
  group_by(Type_Income) %>%
  mutate(Type_Income_mean = mean(acceptance))

dataset <- dataset %>%
  group_by(GENDER) %>%
  mutate(GENDER_mean = mean(acceptance))

dataset <- dataset %>%
  group_by(EDUCATION) %>%
  mutate(EDUCATION_mean = mean(acceptance))

dataset <- dataset %>%
  group_by(Type_Occupation) %>%
  mutate(Type_Occupation_mean = mean(acceptance))

dataset <- dataset %>%
  group_by(Marital_status) %>%
  mutate(Marital_status_mean = mean(acceptance))

dataset <- dataset %>%
  group_by(Housing_type) %>%
  mutate(Housing_type_mean = mean(acceptance))

#here we have all the variable useful for a logistic regressione 

variabili_1<-c("acceptance","Type_Income_mean", "GENDER_mean", "EDUCATION_mean", "Type_Occupation_mean", "Marital_status_mean", "Housing_type_mean")

prova<-dataset[,variabili_1]

#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(prova), replace=TRUE, prob=c(0.7,0.3))
dtrain  <- prova[sample, ]
dtest   <- prova[!sample, ]


# Applying oversampling with SMOTE
dtrain_provisional <- SMOTE(dtrain,dtrain[,1])   
dtrain_b<-rbind(dtrain,dtrain_provisional$syn_data[,-8])  #this include the 70% of the row from the original dataset plus the one created from smote

prova_provisional <- SMOTE(prova,prova[,1])
prova_b<-rbind(prova,prova_provisional$syn_data[,-8]) #this includes every row plus the one created from smote


#Fitting the logistic models
glm_fit_prova<- glm(acceptance ~ ., data = prova, family = binomial(link = "logit"))
glm_fit_prova_b <- glm(acceptance ~ ., data = prova_b, family=binomial)
glm_fit_train<- glm(acceptance ~ ., data = dtrain, family = binomial(link = "logit"))
glm_fit_train_b <- glm(acceptance ~ ., data = dtrain_b, family=binomial)

#Fitting the LDA models

lda_fit_prova<- lda(acceptance ~ ., data = prova, family = binomial(link = "logit"))
lda_fit_prova_b <- lda(acceptance ~ ., data = prova_b, family=binomial)
lda_fit_train<- lda(acceptance ~ ., data = dtrain, family = binomial(link = "logit"))
lda_fit_train_b <- lda(acceptance ~ ., data = dtrain_b, family=binomial)

#Leave one out Cross validation

cv_glm_prova <- cv.glm(prova, glm_fit_prova)  #k=n  
cv_glm_prova_b <- cv.glm(prova_b, glm_fit_prova_b)


#!!!!!!!! NON FUNZIONANO SI DEVE TROVARE UN PACCHETTO ADATTO PER LA CROSS VALIDATION 
#cv_lda_prova <- cv.lda(prova, lda_fit_prova)  #k=n  
#cv_lda_prova_b <- cv.glm(prova_b, lda_fit_prova_b)

# Test on sample

# Predicting on the test set with logistic regression model
predicted_glm_acceptance <- predict(glm_fit_train, newdata = dtest, type = "response")
predicted_glm_b_acceptance <- predict(glm_fit_train_b, newdata = dtest, type = "response")

# Predicting on the test set with LDA model
predicted_lda_acceptance <- predict(lda_fit_train, newdata = dtest, type = "response")
predicted_lda_b_acceptance <- predict(lda_fit_train_b, newdata = dtest, type = "response")

# Converting probabilities to binary predictions
predicted_glm_acceptance_y  <- ifelse(predicted_glm_acceptance > 0.5, 1, 0)
predicted_glm_b_acceptance_y  <- ifelse(predicted_glm_b_acceptance > 0.5, 1, 0)
predicted_lda_acceptance_y <- predict(lda_fit_train, newdata = dtest)$class
predicted_lda_b_acceptance_y <- predict(lda_fit_train_b, newdata = dtest)$class


# Confusion matrices 
cm_glm<-table(dtest$acceptance,predicted_glm_acceptance_y)
cm_glm_b<-table(dtest$acceptance,predicted_glm_b_acceptance_y) #with smote 
cm_lda<-table(dtest$acceptance,predicted_lda_acceptance_y)
cm_lda_b<-table(dtest$acceptance,predicted_lda_b_acceptance_y) #with smote

# Precision and accuracy for the GLM model
precision_glm <- cm_glm[2, 2] / sum(cm_glm[, 2]) # Precision is the true positives divided by the sum of column 2 (predicted positives)
accuracy_glm <- sum(diag(cm_glm)) / sum(cm_glm) # Accuracy is the sum of diagonal elements divided by the total sum of the confusion matrix
misclassification_error_glm <- (cm_glm[1, 2] + cm_glm[2, 1]) / sum(cm_glm) # Misclassification error for the GLM model

# Precision and accuracy for the GLM model with SMOTE
precision_glm_b <- cm_glm_b[2, 2] / sum(cm_glm_b[, 2]) # Precision is the true positives divided by the sum of column 2 (predicted positives)
accuracy_glm_b <- sum(diag(cm_glm_b)) / sum(cm_glm_b) # Accuracy is the sum of diagonal elements divided by the total sum of the confusion matrix
misclassification_error_glm_b <- (cm_glm_b[1, 2] + cm_glm_b[2, 1]) / sum(cm_glm_b) # Misclassification error for the GLM model with SMOTE

# Precision and accuracy for the LDA model
precision_lda <- cm_lda[2, 2] / sum(cm_lda[, 2]) # Precision is the true positives divided by the sum of column 2 (predicted positives)
accuracy_lda <- sum(diag(cm_lda)) / sum(cm_lda) # Accuracy is the sum of diagonal elements divided by the total sum of the confusion matrix
misclassification_error_lda <- (cm_lda[1, 2] + cm_lda[2, 1]) / sum(cm_lda) # Misclassification error for the LDA model

# Precision and accuracy for the LDA model with SMOTE
precision_lda_b <- cm_lda_b[2, 2] / sum(cm_lda_b[, 2]) # Precision is the true positives divided by the sum of column 2 (predicted positives)
accuracy_lda_b <- sum(diag(cm_lda_b)) / sum(cm_lda_b) # Accuracy is the sum of diagonal elements divided by the total sum of the confusion matrix
misclassification_error_lda_b <- (cm_lda_b[1, 2] + cm_lda_b[2, 1]) / sum(cm_lda_b) # Misclassification error for the LDA model with SMOTE



#SOLUTION FOR A RANDOM ERROR
predicted_lda_b_acceptance_y <- as.numeric(predicted_lda_b_acceptance_y)
predicted_lda_b_acceptance_y <- predicted_lda_b_acceptance_y-1


# Calculating precision
precision_glm <- Metrics::precision(predicted_glm_acceptance_y, dtest$acceptance)
precision_glm_b <- Metrics::precision(predicted_glm_b_acceptance_y, dtest$acceptance)
precision_lda <- Metrics::precision(predicted_glm_acceptance_y, dtest$acceptance)
precision_lda_b <- Metrics::precision(predicted_lda_b_acceptance_y, dtest$acceptance)
precision_glm
precision_glm_b
precision_lda
precision_lda_b
# Calculating recall
recall_glm <- Metrics::recall(predicted_glm_acceptance_y, dtest$acceptance)
recall_glm_b <- Metrics::recall(predicted_glm_b_acceptance_y, dtest$acceptance)
recall_lda <- Metrics::recall(predicted_lda_acceptance_y, dtest$acceptance)
recall_lda_b <- Metrics::recall(predicted_lda_b_acceptance_y, dtest$acceptance)
recall_glm
recall_glm_b
recall_lda
recall_lda_b
# Calculating F1 score
f1_score_glm <- Metrics::f1(predicted_glm_acceptance_y, dtest$acceptance)
f1_score_glm_b <- Metrics::f1(predicted_glm_b_acceptance_y, dtest$acceptance)
f1_score_lda <- Metrics::f1(predicted_lda_acceptance_y, dtest$acceptance)
f1_score_lda_b <- Metrics::f1(predicted_lda_b_acceptance_y, dtest$acceptance)
f1_score_glm
f1_score_glm_b
f1_score_lda
f1_score_lda_b


