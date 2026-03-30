cat("========================================\n")
cat("CASE STUDY 4 : LOAN APPROVAL PREDICTION\n")
cat("========================================\n\n")

# Load dataset (robust path lookup)
args <- commandArgs(trailingOnly = FALSE)
file_arg <- "--file="
script_path <- if (any(grepl(file_arg, args))) sub(file_arg, "", args[grep(file_arg, args)]) else ""
script_dir <- if (nzchar(script_path)) tryCatch(dirname(normalizePath(script_path)), error=function(e) dirname(path.expand(script_path))) else getwd()
filename <- "loan data.csv"
candidates <- c(
     file.path(script_dir, filename),
     file.path(getwd(), filename),
     file.path(getwd(), "case study4", filename)
)
data_file <- NULL
for (p in candidates) if (file.exists(p)) { data_file <- p; break }
if (is.null(data_file)) stop(paste("Data file not found. Tried:", paste(candidates, collapse=",")))
loan <- read.csv(data_file, stringsAsFactors = FALSE)

cat("1. Summary statistics of dataset:\n")
print(summary(loan))

cat("\nStructure of dataset:\n")
str(loan)

cat("\n2. Approval rate of loans:\n")
approval_rate <- mean(loan$Loan_Status == "Y", na.rm = TRUE)
print(approval_rate)

cat("\n3. Loan approval based on Gender:\n")
approval_by_gender <- aggregate((Loan_Status == "Y") ~ Gender, data = loan, mean)
names(approval_by_gender)[2] <- "Approval_Rate"
print(approval_by_gender)

cat("\n4. Property Area with highest approval rate:\n")
approval_by_area <- aggregate((Loan_Status == "Y") ~ Property_Area, data = loan, mean)
names(approval_by_area)[2] <- "Approval_Rate"
print(approval_by_area)
cat("\nProperty Area with highest approval rate is:\n")
print(approval_by_area$Property_Area[which.max(approval_by_area$Approval_Rate)])

cat("\n5. Relationship between ApplicantIncome and Loan_Status:\n")
boxplot(ApplicantIncome ~ Loan_Status,
     data = loan,
     main = "Applicant Income by Loan Status",
     xlab = "Loan Status",
     ylab = "Applicant Income",
     col = c("pink", "lightgreen"))
cat("This boxplot helps compare income distribution for approved and rejected loans.\n")

cat("\n6. Handling missing values in LoanAmount using median:\n")
cat("Missing values before:\n")
print(sum(is.na(loan$LoanAmount)))

loan$LoanAmount[is.na(loan$LoanAmount)] <- median(loan$LoanAmount, na.rm = TRUE)

cat("Missing values after:\n")
print(sum(is.na(loan$LoanAmount)))

cat("\n7. Convert Loan_Status into factor variable:\n")
loan$Loan_Status <- as.factor(loan$Loan_Status)
print(str(loan$Loan_Status))
cat("Loan_Status is categorical, so factor conversion is necessary for classification analysis.\n")

cat("\n8. Missing values in all columns:\n")
print(colSums(is.na(loan)))

cat("\n9. Normalization / Scaling of income variables:\n")
loan$ApplicantIncome_Scaled <- scale(loan$ApplicantIncome)
loan$CoapplicantIncome_Scaled <- scale(loan$CoapplicantIncome)

cat("First 10 scaled ApplicantIncome values:\n")
print(head(loan$ApplicantIncome_Scaled, 10))

cat("\nFirst 10 scaled CoapplicantIncome values:\n")
print(head(loan$CoapplicantIncome_Scaled, 10))

cat("\n10. Outlier detection in LoanAmount:\n")
boxplot(loan$LoanAmount,
     main = "Boxplot of LoanAmount",
     ylab = "LoanAmount",
     col = "lightblue")
cat("Outlier detection is important because extreme values can distort analysis and model performance.\n")

cat("\ncode written and executed by Ujjwal Gupta , ERP: 0231BCA051 of BCA, 6th Sem, BVIMR\n")