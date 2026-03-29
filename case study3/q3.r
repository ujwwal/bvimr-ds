cat("========================================\n")
cat("CASE STUDY 3 : SHARE MARKET DATA ANALYSIS AND PREDICTION\n")
cat("========================================\n\n")

# Load dataset
stock <- read.csv("stock.csv", stringsAsFactors = FALSE)

cat("1. Summary statistics of stock dataset:\n")
print(summary(stock))

cat("\nStructure of dataset:\n")
str(stock)

cat("\n2. Daily Return (in %):\n")
stock$Daily_Return <- ((stock$Close - stock$Open) / stock$Open) * 100
print(head(stock$Daily_Return, 10))

cat("\n3. Highest and Lowest Closing Price:\n")
highest_close <- max(stock$Close, na.rm = TRUE)
lowest_close <- min(stock$Close, na.rm = TRUE)
cat("Highest Closing Price =", highest_close, "\n")
cat("Lowest Closing Price =", lowest_close, "\n")

cat("\n4. Plot Closing Price over Time:\n")
stock$Date <- as.Date(stock$Date)
plot(stock$Date, stock$Close,
     type = "l",
     main = "Closing Price Over Time",
     xlab = "Date",
     ylab = "Close Price",
     col = "blue", lwd = 2)

cat("\n5. Average Trading Volume:\n")
avg_volume <- mean(stock$Volume, na.rm = TRUE)
print(avg_volume)

cat("\n6. Correlation between Volume and Close Price:\n")
cor_stock <- cor(stock$Volume, stock$Close, use = "complete.obs")
print(cor_stock)

cat("\n7. Linear regression model to predict Close using Open:\n")
model3 <- lm(Close ~ Open, data = stock)
print(summary(model3))

cat("\n8. R-squared value:\n")
r2_stock <- summary(model3)$r.squared
print(r2_stock)
cat("R-squared shows how much variation in Close is explained by Open.\n")

cat("\ncode written and executed by Ujjwal Gupta , ERP: 0231BCA051 of BCA, 6th Sem, BVIMR\n")