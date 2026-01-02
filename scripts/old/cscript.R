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


# Seleziona solo le variabili con valori nulli
variabili_con_null <- names(dataset)[colSums(is.na(dataset)) > 0]

# Visualizza il risultato

for (i in variabili_con_null) {
  print(i)
  print(sum(is.na(dataset[[i]])))
  
}

chisq.test(Annual_income,Type_Occupation) #meglio sostituire i missing value con la media di annual income in base al type of occupation


""" Not used but can be helpful 
mean_income_by_type <- aggregate(dataset$Annual_income, 
                                 by = list(dataset$Type_Occupation),
                                 FUN = mean,
                                 na.rm = TRUE)
                                 
colnames(mean_income_by_type) <- c("Type_Occupation", "Mean_Annual_income")
print(mean_income_by_type)
"""

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


head(dataset)
names(dataset)

# selecting the categorical variables

categorical_variables <- dataset %>%
  select_if(function(x) !is.numeric(x)) %>%
  names()
categorical_variables

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

View(dataset)

numeric_data <- dataset %>%
  select(where(function(x) is.numeric(x)))

attach(dataset)

"""
# Rimozione delle righe in cui la variabile "Employed_days" è 365243
numeric_data <- numeric_data[numeric_data$Employed_days != 365243, ]
"""

# Visualizzazione della relazione tra le variabili numeriche

ggpairs(numeric_data)


#Test for collinearity between Birthday_count and Employed_days 

lm.fit <- lm(acceptance ~ Birthday_count + Employed_days, data = dataset)
vif(lm.fit)
summary(lm.fit)




#here we have all the orignal variables
"""
variabili <- c("acceptance","GENDER", "Car_Owner", "Propert_Owner", "CHILDREN", "Annual_income",
               "Type_Income", "EDUCATION", "Marital_status", "Housing_type", "Birthday_count", "Employed_days",
               "Mobile_phone", "Work_Phone", "Phone", "EMAIL_ID", "Type_Occupation", "Family_Members",
               "Type_Income_mean", "GENDER_mean", "EDUCATION_mean", "Type_Occupation_mean", "Marital_status_mean", "Housing_type_mean")
"""

#here we have all the variable for the first analysis 
variabili <- c("acceptance","Car_Owner", "Propert_Owner", "CHILDREN", "Annual_income","Birthday_count",  "Work_Phone", "Phone", "EMAIL_ID", "Family_Members",
               "Type_Income_mean", "GENDER_mean", "EDUCATION_mean", "Type_Occupation_mean", "Marital_status_mean", "Housing_type_mean","Employed_days")
 

#creating the dataset 
prova<-dataset[,variabili]

# Creare il modello di regressione logistica con tutte le variabili nel dataframe
lm_fit<- glm(acceptance ~ ., data = prova, family = binomial(link = "logit"))

# Stampare un riepilogo del modello
summary(lm_fit)

lm_fit_f<-step(lm_fit, direction="both") #this is useful to make a selection of variables 
summary(lm_fit_f)

#here we have all the variable useful for a logistic regressione 

variabili_1<-c("acceptance","Type_Income_mean", "GENDER_mean", "EDUCATION_mean", "Type_Occupation_mean", "Marital_status_mean", "Housing_type_mean","Employed_days")

prova<-dataset[,variabili_1]

#Testing correlation between independent variable 

correlation_matrix <- cor(prova) #cration of the matrix
ggcorrplot(correlation_matrix, type = "full", lab = TRUE) #code for the plot! ggplot2 need a dataframe ad we must use reshape2 

#testing collinearity between Employed_days and Type_income_mean
lm.fit <- glm(acceptance ~ Type_Income_mean + Employed_days, data = dataset)
vif(lm.fit)

#We will test the predictive model with and without Employed_days to see if improve the prediction or not

#here we have all the variable useful for a linear regressione 

variabili_1<-c("acceptance","Type_Income_mean", "GENDER_mean", "EDUCATION_mean", "Type_Occupation_mean", "Marital_status_mean", "Housing_type_mean")

prova<-dataset[,variabili_1]


#make this example reproducible
set.seed(1)

#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(prova), replace=TRUE, prob=c(0.7,0.3))
dtrain  <- prova[sample, ]
dtest   <- prova[!sample, ]

#since we have an moderately imbalanced dataset we will use an oversempling tecnique to adjust the training set 

# Applying oversampling with SMOTE
dtrain_balanced <- SMOTE(dtrain,dtrain[,1])
dtrain_b<-rbind(dtrain,dtrain_balanced$syn_data[,-8])


  #da aggiungere alla lista iniziale!!!!!!!!!
library("boot")

lm_fit<- glm(acceptance ~ ., data = prova, family = binomial(link = "logit"))
glm.fit <- glm(acceptance ~ ., data = prova, family=binomial)
cv.err <- cv.glm(prova, glm.fit,K=1238)  #k=n  LOOCV
cv.err

cv.err <- cv.glm(dtrain,logistic_model)
cv.err



#lda model 

library(MASS) 

linear_discriminant<-lda(acceptance ~ .,data=dtrain,family="binomial")
linear_discriminant

linear_discriminant<-lda(acceptance ~ .,data=prova,family="binomial")
linear_discriminant


# Fitting the logistic regression model
lm_fit <- glm(acceptance ~ ., data = dtrain, family = binomial(link = "logit"))

# Predicting on the test set
predicted_labels <- predict(lm_fit, newdata = dtest, type = "response")

# Converting probabilities to binary predictions
predicted_labels_binary <- ifelse(predicted_labels > 0.5, 1, 0)
table(dtest$label,predicted_labels_binary)
table(predicted_labels_binary)
summary(lm_fit)
table(dtest$acceptance)


predicted <- predicted_labels_binary
actual <- dtest$acceptance

# to calculate precision
Metrics::recall(predicted, actual)

# to calculate precision
Metrics::precision(predicted, actual)

# to calculate f1_score
Metrics::f1(predicted, actual)







#TEST SUI DATI 
subset(dataset, rowSums(is.na(dataset)) > 0)
unique(Type_Occupation)
sum(dataset$Employed_days==365243)
rows <- dataset$Employed_days == 365243
subset_dataset <- dataset[rows, ]
View(subset_dataset)
rows <- dataset$Type_Income == "Pensioner"
subset_dataset1 <- dataset[rows, ]
View(subset_dataset1)
rows <- is.na(dataset$Birthday_count)
subset_dataset <- dataset[rows, ]
View(subset_dataset)
rows <- dataset$Employed_days != 365243
subset_dataset <- dataset[rows, ]
summary(subset_dataset)
plot(subset_dataset$Employed_days,subset_dataset$label)
plot(dataset$Employed_days,dataset$label)
mean(subset_dataset$Employed_days)/365

is.numeric(dataset)



for (i in variabili_1){
  plot(dataset$label,dataset[[i]], xlab=i)
}

prova$label<-label
ggplot(prova, aes(x = category)) +
  geom_bar() +
  labs(title = "Count Plot", x = "Category", y = "Count")
prova <- subset(prova, select = -label)  


#I dati sono sbilanciati in modo moderato 
frequenze<-table(lable)
barplot(frequenze,label)





#test, codice da non usare per il report 

# Load necessary libraries
install.packages("caret",dependencies=TRUE)
library(caret)

dtrain_balanced <- SMOTE(prova,prova$acceptance)
dtrain<-rbind(prova,dtrain_balanced$syn_data[-8])
# Define your logistic regression model
logistic_model <- glm(acceptance ~., data = dtrain, family = "binomial")

# Perform Leave-One-Out Cross-Validation (LOOCV)
loocv_results <- train(
  x = dtrain[,variabili_1],
  y = dtrain$acceptance,
  method = "glm",
  trControl = trainControl(method = "LOOCV"),
  family = "binomial"
)

# Get the LOOCV error
loocv_results   # Or any other appropriate metric



