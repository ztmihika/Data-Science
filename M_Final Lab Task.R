data <- read.csv("E:/AIUB/Spring------------------24/Data Science/Dataset/Final_Project/adult.csv")
options(max.print = 65000)
data
str(data)
summary(data)
options(max.print = 65000)
data
str(data)
summary(data)


data$workclass <- as.numeric(factor(data$workclass,
                                    levels = c("Private", "Self-emp-not-inc", "Self-emp-inc", "Federal-gov", "Local-gov", "State-gov", "Without-pay", "Never-worked"),  
                                    labels = c(1,2,3,4,5,6,7,8)))

data$education <- as.numeric(factor(data$education, 
                                    levels = c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th", "12th", "HS-grad", "Some-college", 
                                               "Assoc-voc", "Assoc-acdm", "Bachelors", "Masters", "Prof-school", "Doctorate"),
                                    labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15)))

data$marital.status <- as.numeric(factor(data$marital.status, 
                                         levels = c("Married-civ-spouse", "Divorced", "Never-married", "Separated", "Widowed", "Married-spouse-absent", "Married-AF-spouse"),
                                         labels = c(1,2,3,4,5,6,7)))

data$occupation <- as.numeric(factor(data$occupation, 
                                     levels = c("Tech-support", "Craft-repair", "Other-service", "Sales", "Exec-managerial", "Prof-specialty", "Handlers-cleaners", "Machine-op-inspct",
                                                "Adm-clerical", "Farming-fishing", "Transport-moving", "Priv-house-serv", "Protective-serv", "Armed-Forces"),
                                     labels = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13)))

data$relationship <- as.numeric(factor(data$relationship,
                                       levels = c("Wife", "Own-child", "Husband", "Not-in-family", "Other-relative", "Unmarried"),
                                       labels = c(1,2,3,4,5,6)))

data$race <- as.numeric(factor(data$race,
                               levels = c("White", "Asian-Pac-Islander", "Amer-Indian-Eskimo", "Other", "Black"),
                               labels = c(0,1,2,3,4)))

data$sex <- as.numeric(factor(data$sex, 
                              levels = c("Female", "Male"),
                              labels = c(1,2)))

data$native.country <- as.numeric(factor(data$native.country, 
                                         levels = c("United-States", "Cambodia", "England", "Puerto-Rico", "Canada", "Germany", "Outlying-US(Guam-USVI-etc)", "India", "Japan", "Greece", "South", "China", "Cuba", "Iran",
                                                    "Honduras", "Philippines", "Italy", "Poland", "Jamaica", "Vietnam", "Mexico", "Portugal", "Ireland", "France", "Dominican-Republic", "Laos", "Ecuador", "Taiwan", "Haiti", "Columbia", "Hungary",
                                                    "Guatemala", "Nicaragua", "Scotland", "Thailand", "Yugoslavia", "El-Salvador", "Trinadad&Tobago", "Peru", "Hong", "Holand-Netherlands"),
                                         labels = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41)))


age_income <- cor.test(data$age, as.numeric(factor(data$income)), method = "pearson")
print(age_income)

education_num_income <- cor.test(data$education.num, as.numeric(factor(data$income)), method = "pearson")
print(education_num_income)


plot(jitter(data$age), data$gender, 
     xlab = "Age", ylab = "Sex", 
     main = "Scatter Plot of Age vs sex with Jitter")

plot(data$age, data$hours.per.week, 
     xlab = "Age", ylab = "Hours per Week",  
     main = "Scatter Plot of Age vs Hours per Week",
     xlim = range(data$age),  
     ylim = range(data$hours.per.week))    
abline(lm(data$hours.per.week ~ data$age), col = "black")

plot(jitter(data$age), data$education.num, 
     xlab = "Age", ylab = "Education Number", 
     main = "Scatter Plot of Age vs Education Number with Jitter")

plot(jitter(data$age), data$capital.gain, 
     xlab = "Age", ylab = "Capital Gain", 
     main = "Scatter Plot of Age vs Capital Gain with Jitter")


install.packages("infotheo")
library(infotheo)

# Compute Mutual Information between each feature and the target variable (income)
mi_values <- sapply(data[, -ncol(data)], function(x) mutinformation(x, data$income))

# Sort the features based on Mutual Information values
mi_sorted <- sort(mi_values, decreasing = TRUE)

# Select the top features based on Mutual Information scores
top_features <- names(mi_sorted)[1:5]  # Select top 5 features for example

print(mi_sorted)
print(top_features)

# Define categorical columns
categorical_cols <- c("workclass", "education", "marital.status", "occupation", "relationship", "race", "sex", "native.country")

# Define a function to calculate Spearman's Rank correlation coefficient
calculate_spearman <- function(variable) {
  cor_test <- cor.test(as.numeric(factor(data[[variable]])), as.numeric(factor(data$income)), method = "spearman")
  return(cor_test)
}

# Create an empty list to store the correlation results
spearman_results <- list()

# Calculate Spearman's correlation for each categorical variable
for (col in categorical_cols) {
  spearman_results[[col]] <- calculate_spearman(col)
}

# Print the results
for (col in categorical_cols) {
  cat("Spearman's Rank Correlation Coefficient for", col, "and income:\n")
  print(spearman_results[[col]])
  cat("\n")
}

# Histogram for age
hist(data$age, main = "Distribution of Age", xlab = "Age", ylab = "Frequency", col = "skyblue")

# Histogram for education.num
hist(data$education.num, main = "Distribution of Education Number", xlab = "Education Number", ylab = "Frequency", col = "lightgreen")

# Histogram for capital.gain
hist(data$capital.gain, main = "Distribution of Capital Gain", xlab = "Capital Gain", ylab = "Frequency", col = "salmon")

# Histogram for capital.loss
hist(data$capital.loss, main = "Distribution of Capital Loss", xlab = "Capital Loss", ylab = "Frequency", col = "lightblue")

# Bar plot for workclass
barplot(table(data$workclass), main = "Distribution of Workclass", xlab = "Workclass", ylab = "Frequency", col = "skyblue")

# Bar plot for education
barplot(table(data$education), main = "Distribution of Education Level", xlab = "Education Level", ylab = "Frequency", col = "lightgreen")

# Bar plot for marital.status
barplot(table(data$marital.status), main = "Distribution of Marital Status", xlab = "Marital Status", ylab = "Frequency", col = "salmon")

# Bar plot for occupation
barplot(table(data$occupation), main = "Distribution of Occupation", xlab = "Occupation", ylab = "Frequency", col = "lightblue")

# Bar plot for relationship
barplot(table(data$relationship), main = "Distribution of Relationship", xlab = "Relationship", ylab = "Frequency", col = "orange")

# Bar plot for race
barplot(table(data$race), main = "Distribution of Race", xlab = "Race", ylab = "Frequency", col = "pink")

# Bar plot for sex
barplot(table(data$sex), main = "Distribution of Sex", xlab = "Sex", ylab = "Frequency", col = "yellow")

# Bar plot for native.country
barplot(table(data$native.country), main = "Distribution of Native Country", xlab = "Native Country", ylab = "Frequency", col = "purple")

# Load required library for violin plots
install.packages("ggplot2")
library(ggplot2)

# Create a function to generate violin plots
create_violin_plot <- function(variable, title) {
  ggplot(data, aes(x = "", y = .data[[variable]], fill = income)) +
    geom_violin(trim = FALSE) +
    geom_boxplot(width = 0.1, fill = "white", color = "black") +
    labs(title = title, x = "", y = variable) +
    theme_minimal() +
    theme(legend.position = "none")
}

# Generate violin plots for selected numerical variables
violin_age <- create_violin_plot("age", "Distribution of Age by Income")
violin_education_num <- create_violin_plot("education.num", "Distribution of Education Number by Income")
violin_hours_per_week <- create_violin_plot("hours.per.week", "Distribution of Hours per Week by Income")
violin_capital_gain <- create_violin_plot("capital.gain", "Distribution of Capital Gain by Income")
violin_capital_loss <- create_violin_plot("capital.loss", "Distribution of Capital Loss by Income")

# Plot violin plots
print(violin_age)
print(violin_education_num)
print(violin_hours_per_week)
print(violin_capital_gain)
print(violin_capital_loss)
# Scatter plot for Age vs. Education Number
scatter_age_education_num <- ggplot(data, aes(x = age, y = education.num)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Education Number", x = "Age", y = "Education Number") +
  theme_minimal()

# Scatter plot for Age vs. Hours per Week
scatter_age_hours_per_week <- ggplot(data, aes(x = age, y = hours.per.week)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Hours per Week", x = "Age", y = "Hours per Week") +
  theme_minimal()

# Scatter plot for Age vs. Capital Gain
scatter_age_capital_gain <- ggplot(data, aes(x = age, y = capital.gain)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Capital Gain", x = "Age", y = "Capital Gain") +
  theme_minimal()

# Scatter plot for Age vs. Capital Loss
scatter_age_capital_loss <- ggplot(data, aes(x = age, y = capital.loss)) +
  geom_point() +
  labs(title = "Scatter Plot of Age vs. Capital Loss", x = "Age", y = "Capital Loss") +
  theme_minimal()

# Plot scatter plots
print(scatter_age_education_num)
print(scatter_age_hours_per_week)
print(scatter_age_capital_gain)
print(scatter_age_capital_loss)
