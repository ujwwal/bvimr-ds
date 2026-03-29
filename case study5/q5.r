cat("========================================\n")
cat("CASE STUDY 5 : COVID-19 DATA ANALYSIS AND PREDICTION\n")
cat("========================================\n\n")

# Load dataset
covid <- read.csv("covid_final.csv", stringsAsFactors = FALSE, check.names = FALSE)
names(covid) <- trimws(names(covid))
names(covid) <- gsub("[ /]+", "_", names(covid))
names(covid) <- gsub("[^A-Za-z0-9_]", "", names(covid))

cat("1. Summary statistics of dataset:\n")
print(summary(covid))

cat("\nStructure of dataset:\n")
str(covid)

cat("\n2. Total confirmed cases country-wise:\n")
confirmed_by_country <- aggregate(Confirmed_Cases ~ Country_State, data = covid, sum)
print(confirmed_by_country)

cat("\n3. Top 5 countries with highest deaths:\n")
deaths_by_country <- aggregate(Deaths ~ Country_State, data = covid, sum)
top5_deaths <- head(deaths_by_country[order(-deaths_by_country$Deaths), ], 5)
print(top5_deaths)

cat("\n4. Plot daily new cases using line graph:\n")
covid$Date <- as.Date(covid$Date)
plot(covid$Date, covid$New_Cases,
     type = "l",
     main = "Daily New COVID-19 Cases",
     xlab = "Date",
     ylab = "New Cases",
     col = "red", lwd = 2)

cat("\n5. Mortality rate:\n")
covid$Mortality_Rate <- (covid$Deaths / covid$Confirmed_Cases) * 100
print(head(covid$Mortality_Rate, 10))
cat("Mortality Rate = (Deaths / Confirmed Cases) * 100\n")

cat("\nCorrelation and Regression Case\n")

cat("\n1. Correlation between Confirmed Cases and Deaths:\n")
cor_value <- cor(covid$Confirmed_Cases, covid$Deaths, use = "complete.obs")
print(cor_value)

cat("\n2. Linear regression model to predict Deaths from Confirmed Cases:\n")
model5 <- lm(Deaths ~ Confirmed_Cases, data = covid)
print(summary(model5))

cat("\n3. R-squared value:\n")
r2 <- summary(model5)$r.squared
print(r2)
cat("R-squared tells how much variation in deaths is explained by confirmed cases.\n")

cat("\ncode written and executed by Ujjwal Gupta , ERP: 0231BCA051 of BCA, 6th Sem, BVIMR\n")