cat("========================================\n")
cat("CASE STUDY 2 : TITANIC SURVIVAL PREDICTION\n")
cat("========================================\n\n")

# Load dataset
titanic <- read.csv("Titanic-Dataset.csv", stringsAsFactors = FALSE)

cat("1. Summary statistics of dataset:\n")
print(summary(titanic))

cat("\nStructure of dataset:\n")
str(titanic)

# Convert variables if needed
titanic$Sex <- as.factor(titanic$Sex)
titanic$Pclass <- as.factor(titanic$Pclass)

cat("\n2. Survival rate of male and female passengers:\n")
survival_by_sex <- aggregate(Survived ~ Sex, data = titanic, mean)
print(survival_by_sex)

cat("\n3. Bar plot showing survival by passenger class:\n")
survival_by_class <- aggregate(Survived ~ Pclass, data = titanic, mean)
print(survival_by_class)

barplot(survival_by_class$Survived,
        names.arg = survival_by_class$Pclass,
        main = "Survival Rate by Passenger Class",
        xlab = "Passenger Class",
        ylab = "Survival Rate",
        col = "skyblue")

cat("\n4. Passenger class with highest survival rate:\n")
print(survival_by_class$Pclass[which.max(survival_by_class$Survived)])

cat("\n5. Relationship between Fare and Survival:\n")
fare_survival <- aggregate(Fare ~ Survived, data = titanic, mean)
print(fare_survival)
cat("Generally, passengers with higher fare had better survival chances.\n")

boxplot(Fare ~ Survived,
        data = titanic,
        main = "Fare vs Survival",
        xlab = "Survived (0 = No, 1 = Yes)",
        ylab = "Fare",
        col = c("orange", "lightgreen"))

cat("\n6. Logistic Regression Model:\n")
model_data <- titanic[complete.cases(titanic[, c("Survived", "Pclass", "Sex", "Age", "Fare")]), ]
model2 <- glm(Survived ~ Pclass + Sex + Age + Fare,
              data = model_data,
              family = binomial)

print(summary(model2))

cat("\nOdds Ratios:\n")
print(exp(coef(model2)))

cat("\n7. Interpretation of coefficient of Sex:\n")
sex_coef_name <- grep("Sex", names(coef(model2)), value = TRUE)
print(coef(model2)[sex_coef_name])
cat("If the coefficient of Sex is negative for male, it means males had lower survival odds compared to the reference category.\n")

cat("\ncode written and executed by Ujjwal Gupta , ERP: 0231BCA051 of BCA, 6th Sem, BVIMR\n")