cat("========================================\n")
cat("CASE STUDY 1 : IRIS DATASET IN R\n")
cat("========================================\n\n")

# Load dataset
iris <- read.csv("Iris.csv", stringsAsFactors = FALSE)
iris$Species <- as.factor(iris$Species)

cat("1. Structure of dataset:\n")
str(iris)

cat("\n2. Summary of dataset:\n")
print(summary(iris))

cat("\n3. Mean of each numeric variable:\n")
numeric_cols <- c("SepalLengthCm", "SepalWidthCm", "PetalLengthCm", "PetalWidthCm")
print(sapply(iris[, numeric_cols], mean, na.rm = TRUE))

cat("\nStandard Deviation of each numeric variable:\n")
print(sapply(iris[, numeric_cols], sd, na.rm = TRUE))

cat("\n4. Boxplot for Petal Length by Species will be displayed.\n")
boxplot(PetalLengthCm ~ Species,
        data = iris,
        main = "Petal Length by Species",
        xlab = "Species",
        ylab = "Petal Length",
        col = c("lightblue", "lightgreen", "lightpink"))

cat("\n5. Species with highest average petal length:\n")
avg_petal <- aggregate(PetalLengthCm ~ Species, data = iris, mean)
print(avg_petal)
cat("\nSpecies with highest average petal length is:\n")
print(avg_petal$Species[which.max(avg_petal$PetalLengthCm)])

cat("\nR-Based Question: First 10 rows of dataset:\n")
print(head(iris, 10))

cat("\nR-Based Question: Correlation matrix:\n")
print(cor(iris[, numeric_cols]))

cat("\nInterpretation of correlation between PetalLengthCm and PetalWidthCm:\n")
pl_pw_cor <- cor(iris$PetalLengthCm, iris$PetalWidthCm)
print(pl_pw_cor)
cat("There is a strong positive correlation between PetalLengthCm and PetalWidthCm.\n")

cat("\nLINEAR REGRESSION CASE\n")
cat("Fit linear regression model: PetalLengthCm ~ SepalLengthCm\n")
model1 <- lm(PetalLengthCm ~ SepalLengthCm, data = iris)

cat("\nRegression Model Summary:\n")
print(summary(model1))

cat("\nRegression Coefficients:\n")
print(coef(model1))

cat("\nInterpretation:\n")
cat("Intercept =", coef(model1)[1], "\n")
cat("Slope =", coef(model1)[2], "\n")
cat("For every 1 unit increase in SepalLengthCm, PetalLengthCm changes by", coef(model1)[2], "units.\n")

cat("\nPlotting regression line...\n")
plot(iris$SepalLengthCm, iris$PetalLengthCm,
     main = "Regression of Petal Length on Sepal Length",
     xlab = "Sepal Length",
     ylab = "Petal Length",
     pch = 19, col = "blue")
abline(model1, col = "red", lwd = 2)

cat("\ncode written and executed by Ujjwal Gupta , ERP: 0231BCA051 of BCA, 6th Sem, BVIMR\n")