##### Import Data (Rosario's part ) #####

#install.packages("here", dependencies=TRUE)
#install.packages("GGally",dependencies=TRUE)
#install.packages("ggplot2",dependencies=TRUE)
#install.packages("dplyr",dependencies=TRUE)
lib <- c("here","dplyr","ggplot2","GGally","dplyr")
lapply(lib, library, character.only = TRUE)
training <- read.csv(here("data/processed", "training.csv"))
test<- read.csv(here("data/processed", "test.csv"))
dataset<-training
rownames ( dataset )= dataset [ ,2]
dataset<-dataset[,-2]
dataset<-dataset[,-1]
attach(dataset)

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

#"We are substituting missing values for 'Annual_income' with the average income based on the "Type_occupation" 

dataset$Annual_income <- ifelse(is.na(dataset$Annual_income),
                                ave(dataset$Annual_income, dataset$Type_Occupation, FUN = function(x) mean(x, na.rm = TRUE)),
                                dataset$Annual_income)


#"We are substituting missing values for 'Birthday_count' with the average income based on the "Type_Income" 

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
  mutate(Type_Income_mean = mean(label))

dataset <- dataset %>%
  group_by(GENDER) %>%
  mutate(GENDER_mean = mean(label))

dataset <- dataset %>%
  group_by(EDUCATION) %>%
  mutate(EDUCATION_mean = mean(label))

dataset <- dataset %>%
  group_by(Type_Occupation) %>%
  mutate(Type_Occupation_mean = mean(label))

dataset <- dataset %>%
  group_by(Marital_status) %>%
  mutate(Marital_status_mean = mean(label))

dataset <- dataset %>%
  group_by(Housing_type) %>%
  mutate(Housing_type_mean = mean(label))

View(dataset)

numeric_data <- dataset %>%
  select(where(function(x) is.numeric(x)))

attach(dataset)


# Visualizzazione della relazione tra le variabili numeriche

#ggpairs(numeric_data) messo da parte, non serve


#Test for collinearity between Birthday_count and Employed_days 

#install.packages("car", dependencies=TRUE)
library(car)

lm.fit <- lm(label ~ Birthday_count + Employed_days, data = dataset)
vif(lm.fit)
summary(lm.fit)

names(dataset)


#here we have all the orignal variables

#here we have all the variable for the first analysis 
variabili <- c("Car_Owner", "Propert_Owner", "CHILDREN", "Annual_income","Birthday_count",  "Work_Phone", "Phone", "EMAIL_ID", "Family_Members",
               "Type_Income_mean", "GENDER_mean", "EDUCATION_mean", "Type_Occupation_mean", "Marital_status_mean", "Housing_type_mean","Employed_days")


#creating the dataset 
prova<-dataset[,variabili]




##### Data Visualization and Summary Statistics (Paolo's part) ##### 


## Summary statistics

#Try with skimr 
#install.packages("skimr", dependencies = TRUE)
library(skimr)
#skim(dataset) # This provides the n_missing and complete rate. 

#to remove useless fields, we create a new function of skim
my_skim <- skim_with(numeric = sfl(n_missing = NULL, complete_rate = NULL)) 
my_skim(dataset)

### Plots 

## Label pie chart
counts <- table(dataset$label)
relative_freq <- prop.table(counts)

pie(counts, edges = 600, col = c("steelblue2", "coral2"), labels = paste(names(counts), " (", counts, ")", sep = ""), main = "Pie Chart of Label")

## group histograms 

#We use patchwork for this
#install.packages("patchwork", dependencies = TRUE)
library(patchwork)

#since we want to create an hist of all the numerical variables and not bool, we select only them

dataset_hist <- dataset %>%
  select_if(function(x) is.numeric(x) && any(x > 1))

#It remains variable Housing_type, I remove it and add label
dataset_hist<- subset(dataset_hist, select = -Housing_type)

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
    
  #print(plot)
  plot_hist[[paste0("h", count)]] <- plot
}

#plot the combination of graphs, with options
combined_hist <- wrap_plots(plot_hist, ncol= 2) + plot_annotation(title = "Histogram plots", theme = theme(plot.title= element_text(color = "steelblue4", size = 18)))

print(combined_hist)



## Categorical variables

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
  group_by(label, Type_Income) %>%
  summarise(count = n()) %>%
  group_by(label) %>%
  mutate(percentage = count / sum(count) * 100)

# Create bar plot

plot <- ggplot(data = percentages, aes(x = Type_Income, y = percentage, fill = as.factor(label))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  scale_fill_manual(values = colors) +
  labs(x = "Type of Income", y = "Percentage", fill = "Label") +
  theme_minimal()+
  theme(legend.position=c(.12, .80), legend.background = element_rect(fill = legend_fill, color = legend_color))

print(plot)
plot_cat[[paste0("h", count)]] <- plot


#Marital Status
count <- count +1

percentages <- dataset %>%
  group_by(label, Marital_status) %>%
  summarise(count = n()) %>%
  group_by(label) %>%
  mutate(percentage = count / sum(count) * 100)

# Create bar plot
plot <- ggplot(data = percentages, aes(x = Marital_status, y = percentage, fill = as.factor(label))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  scale_fill_manual(values = colors) +
  labs(x = "Marital status", y = "Percentage", fill = "Label") +
  theme_minimal()+
  theme(legend.position=c(.12, .80), legend.background = element_rect(fill = legend_fill, color = legend_color))


print(plot)
plot_cat[[paste0("h", count)]] <- plot

#Housing_Type
count <- count +1

percentages <- dataset %>%
  group_by(label, Housing_type) %>%
  summarise(count = n()) %>%
  group_by(label) %>%
  mutate(percentage = count / sum(count) * 100)

# Create bar plot
plot <- ggplot(data = percentages, aes(x = Housing_type, y = percentage, fill = as.factor(label))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  scale_fill_manual(values = colors) +
  labs(x = "Housing Type", y = "Percentage", fill = "Label") +
  theme_minimal()+
  theme(legend.position=c(.12, .80), legend.background = element_rect(fill = legend_fill, color = legend_color))
print(plot)
plot_cat[[paste0("h", count)]] <- plot

#Education 
count <- count +1

percentages <- dataset %>%
  group_by(label, EDUCATION) %>%
  summarise(count = n()) %>%
  group_by(label) %>%
  mutate(percentage = count / sum(count) * 100)

# Create bar plot
plot <- ggplot(data = percentages, aes(x = EDUCATION, y = percentage, fill = as.factor(label))) +
  geom_bar(stat = "identity", position = "dodge", alpha = 1) +
  scale_fill_manual(values = colors) +
  labs(x = "Education", y = "Percentage", fill = "Label") +
  theme_minimal()+
  theme(legend.position=c(.12, .80), legend.background = element_rect(fill = legend_fill, color = legend_color))


print(plot)
plot_cat[[paste0("h", count)]] <- plot



#showing the combined graph
combined_cat <- wrap_plots(plot_cat, ncol = 1, heights = c(20, 20, 20, 20), theme(legend.position = "bottom"), guides = "collect") + plot_annotation(title = "Categorical plots by label", subtitle = "proportional percentages for each label value", theme = theme(plot.title= element_text(color = "coral3", size = 18), plot.subtitle = element_text(color = "skyblue4", size = 12)))
#showing plots
combined_cat

## correlation matrix

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




## correlation with label bar plot

#determine correlation
correlations <- cor(dataset_num[, -label], dataset_num[, label])
#sort correlation
sorted_correlations <- sort(correlations, decreasing = TRUE)

#create the plot 
sorted_correlations

###Correlation plot with only label
library(dplyr)
library(tidyr)
# Calculate correlation coefficients
correlation_data <- cor(dataset_num) %>%
  as.data.frame() %>%
  mutate(variable = rownames(.)) %>%
  filter(variable != "label") %>% # Exclude label itself
  rename(correlation = label) %>% # Rename the correlation column
  select(variable, correlation) # Select only relevant columns

# Create the plot
ggplot(correlation_data, aes(x = variable, y = correlation, fill = abs(correlation))) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "coral3", high = "darkblue") +
  coord_flip() +  # Flip coordinates to make it horizontal
  theme_minimal() +
  theme(axis.text.y = element_text(size=10)) +
  labs(
    title = "Correlation Coefficients with Label",
    x = "Variables", #since are flipped
    y = "Correlation Coefficient"
  ) +
  guides(fill = FALSE)  # Hide fill legend










