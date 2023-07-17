#TELECOM CUSTOMER CHURN
library(ggplot2)
library(dplyr)

########IMPORTING THE DATASET########
data <- read.csv("/Users/ratty/Desktop/Analytics and Viz/CS544_TelecomProject_Shivakumar_RatnaMeena/Telco-Customer-Churn.csv")
str(data)


#########DATA CLEANING###########
missing_values <- is.na(data)  # Identify missing values
missing_values
data <- na.omit(data)  # Remove rows with missing values
# Remove missing or non-numeric values from tenure and TotalCharges columns
valid_data <- data[!is.na(data$tenure) & !is.na(data$TotalCharges), ]
valid_data$TotalCharges <- as.numeric(valid_data$TotalCharges)
#DataConversion
data$TotalCharges <- as.numeric(data$TotalCharges)
data$Churn <- as.numeric(data$Churn == "Yes")  # Convert Churn to numeric (0 or 1)
data$MonthlyCharges <- as.numeric(valid_data$MonthlyCharges)

#########ANALYSIS OF CATEGORICAL VARIABLE#######
#- PAYMENT METHOD
payment_freq <- table(data$PaymentMethod)
print(payment_freq)
ggplot(data, aes(x = PaymentMethod)) +
  geom_bar(width = 0.6) +  # Adjust the width parameter to reduce the size of bars
  labs(title = "Payment Method Distribution") +
  xlab("Payment Method") +
  ylab("Count")
payment_prop <- prop.table(payment_freq)
print(payment_prop)
#-INTERNET SERVICE
internet_freq <- table(data$InternetService)
print(internet_freq)
table(data$MonthlyCharges)
# Bar plot
ggplot(data, aes(x = InternetService, fill = InternetService)) +
  geom_bar() +
  labs(title = "Distribution of InternetService") +
  xlab("InternetService") +
  ylab("Count")
# Box plot
ggplot(data, aes(x = InternetService, y = MonthlyCharges, fill = InternetService)) +
  geom_boxplot() +
  labs(title = "Monthly Charges by InternetService") +
  xlab("InternetService") +
  ylab("Monthly Charges")
# Calculate summary statistics by Internet Service
summary_data <- data %>%
  group_by(InternetService) %>%
  summarise(
    Mean = mean(MonthlyCharges),
    Median = median(MonthlyCharges),
    Min = min(MonthlyCharges),
    Max = max(MonthlyCharges)
  )
# Print the summary statistics
print(summary_data)

# Bar plot
ggplot(data, aes(x = InternetService, y = MonthlyCharges, fill = InternetService)) +
  stat_summary(fun.y = mean, geom = "bar") +
  labs(title = "Average Monthly Charges by InternetService") +
  xlab("InternetService") +
  ylab("Average Monthly Charges")
summary_data <- aggregate(MonthlyCharges ~ InternetService, data = data, FUN = mean)
print(summary_data)



######ANALYSIS OF NUMERICAL VARIABLE########
#- MONTHLY CHARGE
summary(data$MonthlyCharges)
library(ggplot2)
ggplot(data, aes(x = MonthlyCharges)) +
  geom_histogram(binwidth = 10, fill = "#808080", color = "black") +
  labs(title = "Monthly Charges Distribution") +
  xlab("Monthly Charges") +
  ylab("Count")
monthly_charges <- data$MonthlyCharges
# Calculate summary statistics
summary_stats <- summary(monthly_charges)
print(summary_stats)
# Create histogram
histogram <- hist(monthly_charges, breaks = seq(0, 200, by = 10), plot = FALSE)
print(histogram$counts)
mean(data$MonthlyCharges)
var(data$MonthlyCharges)
fivenum(data$MonthlyCharges)
#-CHURN
churn_freq <- table(data$Churn)
print(churn_freq)
# Pie chart
ggplot(data, aes(x = "", fill = Churn)) +
  geom_bar(width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Churn Distribution")
churn_rate <- sum(data$Churn == "Yes") / nrow(data) * 100
print(paste("Churn Rate:", churn_rate, "%"))


#######ANALYSIS OF TWO OR MORE########
#1.-----Tenure and Total Charges----
#CORRELATION B/W Tenure and Total Charges
# Calculate the correlation between tenure and TotalCharges
correlation <- cor(valid_data$tenure, valid_data$TotalCharges)
# Create a scatter plot
plot(valid_data$tenure, valid_data$TotalCharges, pch = 16, col = "lightgreen",
     xlab = "Tenure", ylab = "Total Charges", main = "Scatter Plot of Tenure vs Total Charges",
     xlim = c(0, max(valid_data$tenure)), ylim = c(0, max(valid_data$TotalCharges)))
# Add correlation value to the plot
text(0.8 * max(valid_data$tenure), 0.8 * max(valid_data$TotalCharges),
     paste("Correlation:", round(correlation, 2)), col = "red")

##2.----CALCULATION - Monthly Charges, Total Charges--
# Calculate correlation coefficient
correlation <- cor(data$MonthlyCharges, data$TotalCharges)
correlation
# Create contingency table
cross_tab <- table(data$Churn, data$PaymentMethod)
cross_tab
# Calculate the mean monthly charges for each payment method
mean_charges <- aggregate(MonthlyCharges ~ PaymentMethod, data = data, FUN = mean)
# Sort the data by mean charges in descending order
mean_charges <- mean_charges[order(mean_charges$MonthlyCharges, decreasing = TRUE), ]
# Calculate the average monthly charges for customers who have churned and have a partner
avg_charges <- mean(data$MonthlyCharges[data$Churn == "1" & data$Partner == "Yes"])
# Calculate the maximum total charges for customers who have churned and have dependents
max_charges <- max(data$TotalCharges[data$Churn == "1" & data$Dependents == "Yes"])
# Print the results
cat("Average Monthly Charges for Churned Customers with a Partner:", avg_charges, "\n")
cat("Maximum Total Charges for Churned Customers with Dependents:", max_charges, "\n")
barplot(mean_charges$MonthlyCharges, 
        names.arg = mean_charges$PaymentMethod,
        col = "lightblue", 
        xlab = "Payment Method", 
        ylab = "Mean Monthly Charges",
        main = "Mean Monthly Charges by Payment Method",
        las = 1,
        cex.names = 0.8,
        width = 0.5)   # Adjust the width of the bars (change the value as needed)




#######CENTRAL LIMIT THEORM#######
# Set the seed for reproducibility
set.seed(42)
# Extract the 'MonthlyCharges' data$MonthlyCharges
monthly_charges <- data$MonthlyCharges
# Define the number of samples and sample sizes
num_samples <- 3
sample_sizes <- c(30, 100, 500)
# Create a matrix to store the sample means
sample_means <- matrix(0, nrow = num_samples, ncol = max(sample_sizes))
# Generate random samples and calculate sample means
for (i in 1:num_samples) {
  for (j in 1:sample_sizes[i]) {
    sample <- sample(monthly_charges, size = sample_sizes[i], replace = FALSE)
    sample_means[i, j] <- mean(sample)
  }
}
# Plot the distribution of sample means
par(mfrow = c(num_samples, 1), mar = c(4, 4, 2, 1))
for (i in 1:num_samples) {
  hist(sample_means[i, ], main = paste("Sample Size:", sample_sizes[i]), xlab = "Sample Mean", prob = TRUE, col = "lightblue")
  curve(dnorm(x, mean = mean(monthly_charges), sd = sd(monthly_charges) / sqrt(sample_sizes[i])), add = TRUE, col = "darkblue", lwd = 2)
  legend("topright", legend = c("Sample Means", "Normal Distribution"), col = c("darkblue", "black"), lwd = 2, bg = "white")
}
# Print the sample means
print(sample_means)
# Print the summary statistics of sample means
summary(sample_means)

#######SAMPLING METHODS########
# Load required packages
library(dplyr)
# Random Sampling
random_sample1 <- data[sample(nrow(data), size = 200, replace = FALSE), ]
random_sample2 <- data[sample(nrow(data), size = 500, replace = FALSE), ]
random_sample3 <- data[sample(nrow(data), size = 1000, replace = FALSE), ]
# Stratified Sampling
stratified_sample <- data %>%
  group_by(Contract) %>%
  sample_n(size = 200, replace = FALSE)
# Systematic Sampling
systematic_sample <- data[seq(1, nrow(data), by = round(nrow(data) / 200)), ]
# Compare statistics between original dataset and samples
original_stats <- summarise(data, mean = mean(MonthlyCharges), sd = sd(MonthlyCharges))
random_sample1_stats <- summarise(random_sample1, mean = mean(MonthlyCharges), sd = sd(MonthlyCharges))
random_sample2_stats <- summarise(random_sample2, mean = mean(MonthlyCharges), sd = sd(MonthlyCharges))
random_sample3_stats <- summarise(random_sample3, mean = mean(MonthlyCharges), sd = sd(MonthlyCharges))
stratified_sample_stats <- summarise(stratified_sample, mean = mean(MonthlyCharges), sd = sd(MonthlyCharges))
systematic_sample_stats <- summarise(systematic_sample, mean = mean(MonthlyCharges), sd = sd(MonthlyCharges))
# Print statistics together
cat("Sample\t\tMean\t\tStandard Deviation\n")
cat("Original Dataset:\t", original_stats$mean, "\t", original_stats$sd, "\n")
cat("Random Sample 1:\t", random_sample1_stats$mean, "\t", random_sample1_stats$sd, "\n")
cat("Random Sample 2:\t", random_sample2_stats$mean, "\t", random_sample2_stats$sd, "\n")
cat("Random Sample 3:\t", random_sample3_stats$mean, "\t", random_sample3_stats$sd, "\n")
stratified_sample_stats
cat("Random Sample 3:\t", systematic_sample_stats$mean, "\t", systematic_sample_stats$sd, "\n")

#######FURTHER ANALYSIS########
data_col <- data[ , c('tenure','MonthlyCharges','TotalCharges')]
corr <- cor(data_col)
ggcorrplot(corr , hc.order = TRUE , type = 'lower', lab = TRUE)
data %>% ggplot(aes(x=MonthlyCharges,fill=Churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('green','blue'))+labs(title='Monthly Charges desnisty split churn vs non churn' )
data %>% ggplot(aes(x=tenure,fill=Churn))+ geom_density(alpha=0.8)+scale_fill_manual(values=c('pink','purple'))+labs(title='tenure desnisty split churn vs non churn' )
# Demographic Analysis
gender_distribution <- table(data$Gender)
senior_citizen_distribution <- table(data$SeniorCitizen)
partner_distribution <- table(data$Partner)
dependents_distribution <- table(data$Dependents)
print("Gender Distribution:")
print(gender_distribution)
print("Senior Citizen Distribution:")
print(senior_citizen_distribution)
print("Partner Distribution:")
print(partner_distribution)
print("Dependents Distribution:")
print(dependents_distribution)
# Service Usage Analysis
phone_service_distribution <- table(data$PhoneService)
internet_service_distribution <- table(data$InternetService)
online_security_distribution <- table(data$OnlineSecurity)
tech_support_distribution <- table(data$TechSupport)
print("Phone Service Distribution:")
print(phone_service_distribution)
print("Internet Service Distribution:")
print(internet_service_distribution)
print("Online Security Distribution:")
print(online_security_distribution)
print("Tech Support Distribution:")
print(tech_support_distribution)
# Gender and Senior Citizen Distribution
gender_senior_plot <- ggplot(data, aes(x = gender, fill = factor(SeniorCitizen))) +
  geom_bar(position = "fill") +
  labs(title = "Gender and Senior Citizen Distribution") +
  xlab("Gender") +
  ylab("Proportion") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkorange"))
print(gender_senior_plot)
# Partner and Dependents Distribution
partner_dependents_plot <- ggplot(data, aes(x = Partner, fill = Dependents)) +
  geom_bar(position = "fill") +
  labs(title = "Partner and Dependents Distribution") +
  xlab("Partner") +
  ylab("Proportion") +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "darkorange"))
print(partner_dependents_plot)
# Internet Service and Online Security Distribution
internet_security_plot <- ggplot(data, aes(x = InternetService, fill = OnlineSecurity)) +
  geom_bar(position = "fill") +
  labs(title = "Internet Service and Online Security Distribution") +
  xlab("Internet Service") +
  ylab("Proportion") +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "darkorange", "No internet service" = "gray"))
print(internet_security_plot)
# Contract and Paperless Billing Distribution
contract_paperless_plot <- ggplot(data, aes(x = Contract, fill = PaperlessBilling)) +
  geom_bar(position = "fill") +
  labs(title = "Contract and Paperless Billing Distribution") +
  xlab("Contract") +
  ylab("Proportion") +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "darkorange"))
print(contract_paperless_plot)
