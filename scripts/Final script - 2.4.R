#### Importing data ####

#A)
lib <- c("here","dplyr","ggplot2","GGally","dplyr","ggcorrplot","car","smotefamily","skimr","Metrics","patchwork","boot","MASS","e1071","randomForest","gbm","caret")
#lapply(lib, install.packages, depedencies = TRUE)
lapply(lib, library, character.only = TRUE)

#B
training <- read.csv(here("data/processed", "training.csv"))
test<- read.csv(here("data/processed", "test.csv"))
dataset<-training
rownames ( dataset )= dataset [ ,2]
dataset<-dataset[,-2]
dataset<-dataset[,-1]
dataset$acceptance<-dataset$label
dataset<-dataset[,-1]
dataset = na.omit(dataset)

#### Converting Data ####

#Converting the number of days in years

# C1
dataset$Birthday_count<-(dataset$Birthday_count/365)*(-1)
dataset$Employed_days<-(dataset$Employed_days/365)*(-1)


#C2 
#converting Car_Owner in boolean 
dataset <- dataset %>%
  mutate(Car_Owner = ifelse(Car_Owner == "Y", 1, ifelse(Car_Owner == "N", 0, NA)))

# Converting Gender to boolean
dataset <- dataset %>%
  mutate(GENDER = ifelse(GENDER == "F", 1, ifelse(GENDER == "M", 0, NA)))

#converting Propert_Owner in boolean 
dataset <- dataset %>%
  mutate(Propert_Owner = ifelse(Propert_Owner == "Y", 1, ifelse(Propert_Owner == "N", 0, NA)))


#D
# applicating target encoding to transform categorical variables in numerical 
dataset <- dataset %>%
  group_by(Type_Income) %>%
  mutate(Type_Income_mean = mean(acceptance))

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


#E 
#"We are substituting missing values for 'Annual_income' with the average income based on the "Type_occupation" 


dataset$Annual_income <- ifelse(is.na(dataset$Annual_income),
                                ave(dataset$Annual_income, dataset$Type_Occupation, FUN = function(x) mean(x, na.rm = TRUE)),
                                dataset$Annual_income)


#"We are substituting missing values for 'Birthday_count' with the average age based on the "Type_Income" 

dataset$Birthday_count <- ifelse(is.na(dataset$Birthday_count),
                                 ave(dataset$Birthday_count,dataset$Type_Income, FUN = function(x) mean(x, na.rm=TRUE) ),
                                 dataset$Birthday_count)

#here we have all the variable useful for a logistic regressione 

variabili_1<-c("acceptance","Type_Income_mean", "GENDER", "EDUCATION_mean", "Type_Occupation_mean", "Marital_status_mean", "Housing_type_mean")

prova<-dataset[,variabili_1]

#### SUMMARY STATISTICS AND VISUALIZATION ####

### Summary Statistics 

dataset2 <- dataset %>%
  ungroup()

#To remove n_missing and complete_rate, set all base to NULL (not by single points)
my_skim <- skim_with( base = NULL, append = TRUE)
my_skim(dataset2) #Summary statistics using skimr package.

#legacy to see if the base of the skimmers is actually NULL. 
#base_skimmers(my_skim)

#Codes for plot !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
{
### Graphs ###

## Pie Chart 
counts <- table(dataset$acceptance)
relative_freq <- prop.table(counts)
  pie(counts, edges = 600, col = c("steelblue2", "coral2"), labels = paste(names(counts), " (", round(relative_freq,2),"%, ", counts, " cases )", sep = ""), main = "Pie Chart of Acceptance")
#help(pie)
## Combined Histogram

#since we want to create an hist of all the numerical variables and not bool, we select only them
dataset <- dataset%>%
    ungroup()
dataset_hist <- dataset %>%
  select_if(function(x) is.numeric(x) && any(x > 1))

#It remains variable Housing_type, I remove it and add acceptance ##UPDATE It is not needed since they are ungrouped
#dataset_hist<- subset(dataset_hist, select = -c(Housing_type))

#set the theme for the plots
theme_set(theme_minimal())


#list where the plots will be
plot_hist <- list()
count <- 0
for (column in names(dataset_hist)){
  count <- count + 1
  plot <- ggplot(data = dataset_hist, aes(x = .data[[column]])) +  
    geom_histogram(aes(y = ..count..), alpha = 0.8, fill = "steelblue3", color = "grey70", bins = 30)+
    labs( y = "Frequencies")+
    theme_minimal()
  
  #uncomment if you want to show the plot manually
  #print(plot)
  plot_hist[[paste0("h", count)]] <- plot
}

#plot the combination of graphs, with options
combined_hist <- wrap_plots(plot_hist, ncol= 2) + plot_annotation(title = "Bar plots", theme = theme(plot.title= element_text(color = "steelblue4", size = 18)))

print(combined_hist)
}

## Categorical variables Histogram

#new list for the new grahs 
plot_cat <- list()
#reset count
count <- 0

#add other graphs, manually added. 
#colors for the graphs
colors <- c("0" = "steelblue3", "1" = "coral3")

#colors for legend
legend_fill <- "grey98"
legend_color <- "grey85"


#Type Income

#add count
count <- count + 1

percentages <- dataset %>%
  group_by(acceptance, Type_Income) %>%
  summarise(count = n()) %>%
  group_by(acceptance) %>%
  mutate(percentage = count / sum(count) * 100)

# Create bar plot

plot <- ggplot(data = percentages, aes(x = Type_Income, y = percentage, fill = as.factor(acceptance))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  scale_fill_manual(values = colors) +
  labs(x = "Type of Income", y = "Percentage", fill = "acceptance") +
  theme_minimal()+
  theme(legend.position=c(.12, .80), legend.background = element_rect(fill = legend_fill, color = legend_color))

print(plot)
plot_cat[[paste0("h", count)]] <- plot


#Marital Status
count <- count +1

percentages <- dataset %>%
  group_by(acceptance, Marital_status) %>%
  summarise(count = n()) %>%
  group_by(acceptance) %>%
  mutate(percentage = count / sum(count) * 100)

# Create bar plot
plot <- ggplot(data = percentages, aes(x = Marital_status, y = percentage, fill = as.factor(acceptance))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  scale_fill_manual(values = colors) +
  labs(x = "Marital status", y = "Percentage", fill = "acceptance") +
  theme_minimal()+
  theme(legend.position=c(.12, .80), legend.background = element_rect(fill = legend_fill, color = legend_color))


print(plot)
plot_cat[[paste0("h", count)]] <- plot

#Housing_Type
count <- count +1

percentages <- dataset %>%
  group_by(acceptance, Housing_type) %>%
  summarise(count = n()) %>%
  group_by(acceptance) %>%
  mutate(percentage = count / sum(count) * 100)

# Create bar plot
plot <- ggplot(data = percentages, aes(x = Housing_type, y = percentage, fill = as.factor(acceptance))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  scale_fill_manual(values = colors) +
  labs(x = "Housing Type", y = "Percentage", fill = "acceptance") +
  theme_minimal()+
  theme(legend.position=c(.12, .80), legend.background = element_rect(fill = legend_fill, color = legend_color))
print(plot)
plot_cat[[paste0("h", count)]] <- plot

#Education 
count <- count +1

percentages <- dataset %>%
  group_by(acceptance, EDUCATION) %>%
  summarise(count = n()) %>%
  group_by(acceptance) %>%
  mutate(percentage = count / sum(count) * 100)

# Create bar plot
plot <- ggplot(data = percentages, aes(x = EDUCATION, y = percentage, fill = as.factor(acceptance))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  scale_fill_manual(values = colors) +
  labs(x = "Education", y = "Percentage", fill = "acceptance") +
  theme_minimal()+
  theme(legend.position=c(.12, .80), legend.background = element_rect(fill = legend_fill, color = legend_color))


print(plot)
plot_cat[[paste0("h", count)]] <- plot



#showing the combined graph
combined_cat <- wrap_plots(plot_cat, ncol = 1, heights = c(20, 20, 20, 20), theme(legend.position = "bottom"), guides = "collect") + plot_annotation(title = "Categorical plots by acceptance", subtitle = "proportional percentages for each acceptance value", theme = theme(plot.title= element_text(color = "coral3", size = 18), plot.subtitle = element_text(color = "skyblue4", size = 12)))
#showing plots
combined_cat


## Correlation plot 

#keep only numeric variables
dataset_num <- dataset[, sapply(dataset, is.numeric)]
dataset_num <- subset(dataset_num, select= -c(Mobile_phone))
#replace variable names into more readable names
colnames(dataset_num) <- gsub("_", " ", colnames(dataset_num))

cormat <- cor(dataset_num)
#install.packages("ggcorrplot")
library(ggcorrplot)
#to create the new  
ggcorrplot(cormat, type = "full", lab = TRUE, lab_size = 2.3)




## correlation with acceptance bar plot
#determine correlation
## WARNING: You have to re-initialize the dataset each time you redo this part. 
correlations <- cor(dataset_num[, -which(names(dataset_num) == "acceptance")], dataset_num[,"acceptance"])#sort correlation
sorted_correlations <- sort(correlations, decreasing = TRUE)

#create the plot 
sorted_correlations

###Correlation plot with only acceptance
library(dplyr)
library(tidyr)
# Calculate correlation coefficients
correlation_data <- cor(dataset_num)
# Convert correlation matrix to data frame
correlation_data <- as.data.frame(correlation_data)
# Add a variable column with row names
correlation_data$variable <- rownames(correlation_data)
# Filter out rows where variable is "acceptance"
correlation_data <- correlation_data[correlation_data$variable != "acceptance", ]
# Rename the column corresponding to "acceptance" to "correlation"
names(correlation_data)[which(names(correlation_data) == "acceptance")] <- "correlation"
# Select only the relevant columns (variable and correlation)
correlation_data <- correlation_data[c("variable", "correlation")]
correlation_data <- correlation_data[order(correlation_data$correlation, decreasing = TRUE), ]


#### Validation ####

#make this example reproducible
set.seed(123)

#create a stratificate partition index
trainIndex <- createDataPartition(prova$acceptance, p = 0.8, list = FALSE)

#use 80% of dataset as training set and 20% as test set
dtrain  <- prova[trainIndex, ]
dtest   <- prova[-trainIndex, ]

#### Feature engineering ####

# Applying oversampling with SMOTE

# Remove rows with missing values from the dtrain data frame
dtrain <- na.omit(dtrain)

dtrain_provisional <- SMOTE(dtrain,dtrain[,1])   
dtrain_b<-rbind(dtrain,dtrain_provisional$syn_data[,-8])  #this include the 80% of the row from the original dataset plus the one created from smote

prova<-na.omit(prova)
prova_provisional <- SMOTE(prova,prova[,1])
prova_b<-rbind(prova,prova_provisional$syn_data[,-8]) #this includes every row plus the one created from smote


#Validation for Random Forest with complete dataset
# Create a stratified partition index
c_trainIndex <- createDataPartition(dataset$acceptance, p = 0.8, list = FALSE)

# Use 80% of the dataset as the training set and 20% as the test set
c_dtrain  <- dataset[c_trainIndex, ]
c_dtest   <- dataset[-c_trainIndex, ]

# Select only the numeric variables
numeric_vars <- sapply(c_dtrain, is.numeric)
c_dtrain <- c_dtrain[, numeric_vars]
c_dtest <- c_dtest[, numeric_vars]

c_dtrain$acceptance <-  factor(c_dtrain$acceptance)

nv_dataset<-sapply(dataset, is.numeric) #all numerical variables
n_dataset<-dataset[,nv_dataset]
n_dataset$acceptance <-  factor(n_dataset$acceptance)


#### Model tests ####

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

#Fitting the QDA
qda_model_prova <- qda(acceptance ~ ., data = prova)
qda_model_prova_b <- qda(acceptance ~ ., data = prova_b)
qda_model_train <- qda(acceptance ~ ., data = dtrain)
qda_model_train_b <- qda(acceptance ~ ., data = dtrain_b)

#Fitting Naive Bayes Classifier
nb_model_prova <- naiveBayes(acceptance ~ ., data = prova)
nb_model_prova_b <- naiveBayes(acceptance ~ ., data = prova_b)
nb_model_train <- naiveBayes(acceptance ~ ., data = dtrain)
nb_model_train_b <- naiveBayes(acceptance ~ ., data = dtrain_b)


#Fitting the random forest
n_dataset<-na.omit(n_dataset)
c_dtrain<-na.omit(c_dtrain)
rf_model_n_dataset <- randomForest(acceptance ~ ., data = n_dataset)
rf_model_train <- randomForest(acceptance ~ ., data = c_dtrain)



#Fitting the boosting 
boosting_model_prova <- gbm(acceptance ~ ., data = prova, distribution = "bernoulli")
boosting_model_prova_b <- gbm(acceptance ~ ., data = prova_b, distribution = "bernoulli")
boosting_model_train <- gbm(acceptance ~ ., data = dtrain, distribution = "bernoulli")
boosting_model_train_b <- gbm(acceptance ~ ., data = dtrain_b, distribution = "bernoulli")


#### Prediction Test ####

# Test on sample

# Predicting on the test set with logistic regression model
predicted_glm_acceptance <- predict(glm_fit_train, newdata = dtest, type = "response")
predicted_glm_b_acceptance <- predict(glm_fit_train_b, newdata = dtest, type = "response")

# Predicting on the test set with LDA model
predicted_lda_acceptance <- predict(lda_fit_train, newdata = dtest, type = "response")
predicted_lda_b_acceptance <- predict(lda_fit_train_b, newdata = dtest, type = "response")


# Converting probabilities to binary predictions for logistic regression models
predicted_glm_acceptance_y  <- ifelse(predicted_glm_acceptance > 0.5, 1, 0)  # Convert predicted probabilities from the logistic regression model into binary predictions
predicted_glm_b_acceptance_y  <- ifelse(predicted_glm_b_acceptance > 0.5, 1, 0)  # Convert predicted probabilities from the logistic regression model with SMOTE into binary predictions

# Predictions for LDA models
predicted_lda_acceptance_y <- predict(lda_fit_train, newdata = dtest)$class  # Direct predictions for the LDA model
predicted_lda_b_acceptance_y <- predict(lda_fit_train_b, newdata = dtest)$class  # Direct predictions for the LDA model with SMOTE

# Predictions for QDA models
predicted_qda_train <- predict(qda_model_train, dtest)$class  # Direct predictions for the QDA model
predicted_qda_train_b <- predict(qda_model_train_b, dtest)$class  # Direct predictions for the QDA model with SMOTE

# Predictions for Naive Bayes models
predicted_nb_train <- predict(nb_model_train, dtest)  # Direct predictions for the Naive Bayes model
predicted_nb_train_b <- predict(nb_model_train_b, dtest)  # Direct predictions for the Naive Bayes model with SMOTE

# Predictions for Random Forest models
predicted_rf_train <- predict(rf_model_train, newdata = c_dtest)  

# Predictions for Boosting models
predicted_boosting_train <- predict(boosting_model_train, dtest, n.trees = 100, type = "response")  # Direct predictions for the Boosting model
predicted_boosting_train_b <- predict(boosting_model_train_b, dtest, n.trees = 100, type = "response")  # Direct predictions for the Boosting model with SMOTE
predicted_boosting_train_class <- ifelse(predicted_boosting_train > 0.5, 1, 0)  # Convert predicted probabilities from the Boosting model into binary predictions
predicted_boosting_train_b_class <- ifelse(predicted_boosting_train_b > 0.5, 1, 0)  # Convert predicted probabilities from the Boosting model with SMOTE into binary predictions


# Confusion matrices 
cm_glm<-table(dtest$acceptance,predicted_glm_acceptance_y)
cm_glm_b<-table(dtest$acceptance,predicted_glm_b_acceptance_y) # with SMOTE 
cm_lda<-table(dtest$acceptance,predicted_lda_acceptance_y)
cm_lda_b<-table(dtest$acceptance,predicted_lda_b_acceptance_y) # with SMOTE
cm_qda_train <- table(dtest$acceptance, predicted_qda_train)
cm_qda_train_b <- table(dtest$acceptance, predicted_qda_train_b)
cm_nb_train <- table(dtest$acceptance, predicted_nb_train)  # Confusion matrix for the Naive Bayes model
cm_nb_train_b <- table(dtest$acceptance, predicted_nb_train_b)  # Confusion matrix for the Naive Bayes model with SMOTE
cm_rf_train <- table(c_dtest$acceptance, predicted_rf_train)
cm_boosting_train <- table(dtest$acceptance, predicted_boosting_train_class)
cm_boosting_train_b <- table(dtest$acceptance, predicted_boosting_train_b_class)


# Function to check whether the confusion matrix has the correct dimensions
check_cm <- function(cm) {
  if (!all(dim(cm) == c(2, 2))) {
    cm <- matrix(0, nrow = 2, ncol = 2, dimnames = list(levels(dtest$acceptance), levels(dtest$acceptance)))
  }
  return(cm)
}

# Definition of functions to compute the performance metrics
calculate_accuracy <- function(cm) {
  sum(diag(cm)) / sum(cm)
}

calculate_precision <- function(cm) {
  if (sum(cm[, 2]) == 0) return(NA)
  cm[2, 2] / sum(cm[, 2])
}

calculate_misclassification_error <- function(cm) {
  (cm[1, 2] + cm[2, 1]) / sum(cm)
}

calculate_f1 <- function(cm) {
  precision <- calculate_precision(cm)
  sensitivity <- calculate_sensitivity(cm)
  if (is.na(precision) || is.na(sensitivity) || (precision + sensitivity) == 0) return(NA)
  (2 * precision * sensitivity) / (precision + sensitivity)
}

calculate_sensitivity <- function(cm) {
  if (sum(cm[2, ]) == 0) return(NA)
  cm[2, 2] / sum(cm[2, ])
}

calculate_specificity <- function(cm) {
  if (sum(cm[1, ]) == 0) return(NA)
  cm[1, 1] / sum(cm[1, ])
}

# Computation of performance metrics for each confusion matrix
metrics <- lapply(list(cm_glm, cm_glm_b, cm_lda, cm_lda_b, cm_qda_train, cm_qda_train_b, cm_nb_train, cm_nb_train_b, cm_rf_train, cm_boosting_train, cm_boosting_train_b), function(cm) {
  cm <- check_cm(cm)
  accuracy <- calculate_accuracy(cm)
  precision <- calculate_precision(cm)
  error <- calculate_misclassification_error(cm)
  f1 <- calculate_f1(cm)
  sensitivity <- calculate_sensitivity(cm)
  specificity <- calculate_specificity(cm)
  c(accuracy = accuracy, precision = precision, error = error, f1 = f1, sensitivity = sensitivity, specificity = specificity)
})

# Convert the results into a data frame
metrics_df <- do.call(rbind, metrics)
rownames(metrics_df) <- c("GLM", "GLM with SMOTE", "LDA", "LDA with SMOTE", "QDA", "QDA with SMOTE", "Naive Bayes", "Naive Bayes with SMOTE", "Random Forest", "Boosting", "Boosting with SMOTE")

# Print the metrics data frame
print(metrics_df)



## PREDICTION

test<- read.csv(here("data/processed", "test.csv"))

# Converting the number of days to years for Birthday_count
test$Birthday_count <- (test$Birthday_count / 365) * (-1)

# Converting the number of days to years for Employed_days
test$Employed_days <- (test$Employed_days / 365) * (-1)

# Substituting missing values for 'Annual_income' with the average income based on 'Type_Occupation'
test$Annual_income <- ifelse(is.na(test$Annual_income),
                             ave(test$Annual_income, test$Type_Occupation, FUN = function(x) mean(x, na.rm = TRUE)),
                             test$Annual_income)


# Adding Birthday_count by calculating the mean conditional on Type_Income
test$Birthday_count <- ifelse(is.na(test$Birthday_count),
                              ave(test$Birthday_count, test$Type_Income, FUN = function(x) mean(x, na.rm = TRUE)),
                              test$Birthday_count
)

# Converting Car_Owner to boolean
test <- test %>%
  mutate(Car_Owner = ifelse(Car_Owner == "Y", 1, ifelse(Car_Owner == "N", 0, NA)))

# Converting Gender to boolean
test <- test %>%
  mutate(GENDER = ifelse(GENDER == "F", 1, ifelse(GENDER == "M", 0, NA)))

# Converting Propert_Owner to boolean
test <- test %>%
  mutate(Propert_Owner = ifelse(Propert_Owner == "Y", 1, ifelse(Propert_Owner == "N", 0, NA)))

# Applying target encoding to transform categorical variables into numerical

# Merge the mean values computed on the training dataset with the test dataset

# Extract unique values of EDUCATION
unique_education <- unique(dataset$EDUCATION)

# List to store the indices of the first occurrences of each EDUCATION category
first_indices_education <- vector("numeric", length(unique_education))

# Find the first index of each EDUCATION value in the dataset data frame
for (i in 1:length(unique_education)) {
  first_indices_education[i] <- which(dataset$EDUCATION == unique_education[i])[1]
}

# List to store the unique EDUCATION_mean values for each EDUCATION category
unique_education_mean <- sapply(first_indices_education, function(index) dataset$EDUCATION_mean[index])

# Create a data frame with EDUCATION and the corresponding EDUCATION_mean values
unique_education_df <- data.frame(EDUCATION = unique_education, EDUCATION_mean = unique_education_mean)


# Extract unique values of Type_Income
unique_type_income <- unique(dataset$Type_Income)

# List to store the indices of the first occurrences of each Type_Income category
first_indices_income <- vector("numeric", length(unique_type_income))

# Find the first index of each Type_Income value in the dataset data frame
for (i in 1:length(unique_type_income)) {
  first_indices_income[i] <- which(dataset$Type_Income == unique_type_income[i])[1]
}

# List to store the unique Type_Income_mean values for each Type_Income category
unique_type_income_mean <- sapply(first_indices_income, function(index) dataset$Type_Income_mean[index])

# Create a data frame with Type_Income and the corresponding Type_Income_mean values
unique_type_income_df <- data.frame(Type_Income = unique_type_income, Type_Income_mean = unique_type_income_mean)


# Extract unique values of Marital_status
unique_marital_status <- unique(dataset$Marital_status)

# List to store the indices of the first occurrences of each Marital_status category
first_indices_marital <- vector("numeric", length(unique_marital_status))

# Find the first index of each Marital_status value in the dataset data frame
for (i in 1:length(unique_marital_status)) {
  first_indices_marital[i] <- which(dataset$Marital_status == unique_marital_status[i])[1]
}

# List to store the unique Marital_status_mean values for each Marital_status category
unique_marital_status_mean <- sapply(first_indices_marital, function(index) dataset$Marital_status_mean[index])

# Create a data frame with Marital_status and the corresponding Marital_status_mean values
unique_marital_status_df <- data.frame(Marital_status = unique_marital_status, Marital_status_mean = unique_marital_status_mean)

# Extract unique values of Housing_type
unique_housing_type <- unique(dataset$Housing_type)

# List to store the indices of the first occurrences of each Housing_type category
first_indices_housing <- vector("numeric", length(unique_housing_type))

# Find the first index of each Housing_type value in the dataset data frame
for (i in 1:length(unique_housing_type)) {
  first_indices_housing[i] <- which(dataset$Housing_type == unique_housing_type[i])[1]
}

# List to store the unique Housing_type_mean values for each Housing_type category
unique_housing_type_mean <- sapply(first_indices_housing, function(index) dataset$Housing_type_mean[index])

# Create a data frame with Housing_type and the corresponding Housing_type_mean values
unique_housing_type_df <- data.frame(Housing_type = unique_housing_type, Housing_type_mean = unique_housing_type_mean)


# List to store the unique Type_Occupation categories
unique_occupation <- unique(dataset$Type_Occupation)

first_indices_occupation <- vector("numeric", length(unique_occupation))

# Find the first index of each Type_Occupation value in the dataset data frame
for (i in 1:length(unique_occupation)) {
  first_indices_occupation[i] <- which(dataset$Type_Occupation == unique_occupation[i])[1]
}

# List to store the unique Type_Occupation_mean values for each occupation category
unique_occupation_mean <- sapply(first_indices_occupation, function(index) dataset$Type_Occupation_mean[index])

# Create a data frame with Type_Occupation and the corresponding Type_Occupation_mean values
unique_occupation_df <- data.frame(Type_Occupation = unique_occupation, Type_Occupation_mean = unique_occupation_mean)

test <- merge(test, unique_type_income_df, by = "Type_Income")
test <- merge(test, unique_education_df, by = "EDUCATION")
test <- merge(test, unique_occupation_df, by = "Type_Occupation")
test <- merge(test, unique_marital_status_df, by = "Marital_status")
test <- merge(test, unique_housing_type_df, by = "Housing_type")



## Export Data
output<-test
test_predict_rf<-predict(rf_model_n_dataset, newdata  = test, type = "response")
output$test_predict_rf <- test_predict_rf
table(output$test_predict_rf)
test_table <- table(output$test_predict_rf)
barplot(test_table, col = "blue", border = NULL, main = "Tables of predictions", 
        xlab = "Predicted Values", ylab = "Frequencies")


write.xlsx(output, here("Dati", "output.xlsx"))









