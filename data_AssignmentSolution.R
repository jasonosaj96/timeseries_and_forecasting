# Install required packages if they're not already installed
required_packages <- c(
  "corrplot",
  "tidyverse",
  "zoo",
  "ggplot2",
  "lmtest",
  "sandwich",
  "forecast",
  "urca"  # for adfuller test
)

# Function to install missing packages
install_missing_packages <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
    install.packages(new_packages, dependencies = TRUE)
  }
}

# Install missing packages
install_missing_packages(required_packages)
install.packages("readxl")
install.packages("tseries")
install.packages("fGarch")
install.packages("rugarch")
install.packages("Hmisc")

# Load all required packages
library(corrplot)
library(zoo)
library(ggplot2)
library(lmtest)
library(sandwich)
library(urca)
library(readxl)
library(fGarch) 
library(rugarch)
library(Hmisc)
library(corrplot)
library(fGarch)


# Define the file path relative to the current working directory
file_path <- file.path(getwd(), "data_assignment.txt")

# Load the CSV file with tab as the separator and no headers
data <- read.csv(file_path, header = FALSE, sep = "\t")

# Set custom column names
colnames(data) <- c("HFRI", "EH", "M", "RVA", "ED", "CA", "DS", "EMN", "MA", "RUS-Rf", 
                    "RUS(-1)-Rf(-1)", "MXUS-Rf", "MEM-Rf", "SMB", "HML", "MOM", 
                    "SBGC-Rf", "SBWG-Rf", "LHY-Rf", "DEFSPR", "FRBI-Rf", 
                    "GSCI--Rf", "VIX", "Rf")

# Create a sequence of dates from April 1990 to December 2005
date_sequence <- seq(as.Date("1990-04-01"), as.Date("2005-12-01"), by = "months")

# Ensure the length of date_sequence matches the number of rows in data
if (length(date_sequence) == nrow(data)) {
  # Add date_sequence as a new column
  data <- data.frame(Date = date_sequence, data)  # Add Date as the first column
}


# Display the shape of the data
table_shape <- dim(data)
print(table_shape)

# Display the first few rows to confirm it loaded correctly
head(data)

# Load necessary libraries
library(tidyverse)
library(corrplot)
library(ggplot2)

# Inspect the data
str(data)
summary(data)


# Ensure data is numeric
numeric_data <- data[, sapply(data, is.numeric), drop = FALSE]

# Calculate the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")  # Using "complete.obs" to handle missing values

# Create correlation plot using base R
# Set up the plotting area with rotated labels
par(las = 2, mar = c(10, 10, 4, 2))  # Adjust margins for rotated labels

# Create the plot
image(1:ncol(cor_matrix), 1:nrow(cor_matrix), cor_matrix, 
      col = colorRampPalette(c("blue", "white", "red"))(100),
      xlab = "", ylab = "",
      axes = FALSE)

# Add correlation values
text(expand.grid(1:ncol(cor_matrix), 1:nrow(cor_matrix)),
     labels = round(c(cor_matrix), 2),
     cex = 0.7)

# Add variable names
axis(1, 1:ncol(cor_matrix), colnames(cor_matrix), las = 2)
axis(2, 1:nrow(cor_matrix), rownames(cor_matrix), las = 2)

# Add title
title("Correlation Matrix")


# Create histogram for each numeric column
# First identify numeric columns (excluding Date)
numeric_cols <- names(data)[sapply(data, is.numeric) & names(data) != "Date"]
n_cols <- length(numeric_cols)

# Determine a reasonable layout
n_rows <- ceiling(sqrt(n_cols))
n_cols_plot <- ceiling(n_cols/n_rows)

# Set up the plotting device with smaller margins
par(mfrow = c(n_rows, n_cols_plot))
par(mar = c(2, 2, 2, 1))  # Reduced margins (bottom, left, top, right)
par(oma = c(0, 0, 2, 0))  # Outer margins for overall title

# Create histogram for each numeric variable
for(col in numeric_cols) {
  hist(data[[col]], 
       main = col,  # Shorter title
       xlab = "",   # Remove x-label to save space
       col = "steelblue",
       border = "white",
       breaks = 30)
}

# Add overall title
title("Data Distribution by Feature", outer = TRUE)

# Reset plotting parameters
par(mfrow = c(1, 1))
par(mar = c(5, 4, 4, 2) + 0.1)  # Reset to default margins


# Reset plotting parameters
par(mfrow = c(1, 1))
# Examine time series trends
data %>%
  gather(key = "feature", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_wrap(~ feature, scales = "free_y") +
  labs(title = "Time Series Trends by Feature")


# Calculate 12-month rolling averages
data_with_rolling <- data %>%
  mutate(across(where(is.numeric), ~ zoo::rollmean(., 12, na.pad = TRUE)))

# Examine time series trends with rolling averages
data_with_rolling %>%
  gather(key = "feature", value = "value", -Date) %>%
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_wrap(~ feature, scales = "free_y") +
  labs(title = "Time Series with 12-month Rolling Averages")

# Assess stationarity
data %>%
  gather(key = "feature", value = "value", -Date) %>%
  group_by(feature) %>%
  adfuller(value) %>%
  broom::tidy() %>%
  select(feature, p.value) %>%
  arrange(p.value)





# Load the CSV file with tab as the separator and no headers
data <- read.csv("data_assignment.txt", header = FALSE, sep = "\t")

# Set custom column names
colnames(data) <- c("HFRI", "EH", "M", "RVA", "ED", "CA", "DS", "EMN", "MA", "RUS-Rf", 
                    "RUS(-1)-Rf(-1)", "MXUS-Rf", "MEM-Rf", "SMB", "HML", "MOM", 
                    "SBGC-Rf", "SBWG-Rf", "LHY-Rf", "DEFSPR", "FRBI-Rf", 
                    "GSCI--Rf", "VIX", "Rf")

# Create a sequence of dates from April 1990 to December 2005
date_sequence <- seq(as.Date("1990-04-01"), as.Date("2005-12-01"), by = "months")

# Ensure the length of date_sequence matches the number of rows in data
if (length(date_sequence) == nrow(data)) {
  # Add date_sequence as a new column
  data <- data.frame(Date = date_sequence, data)  # Add Date as the first column
}

################################################
################## Question 1 ##################
################################################
# Load the CSV file with tab as the separator and no headers
data <- read.csv("data_assignment.txt", header = FALSE, sep = "\t")

# Create the new column names mapping
new_colnames <- c(
  # Dependent variables (Y)
  "y1",  # HFRI
  "y2",  # EH
  "y3",  # M
  "y4",  # RVA
  "y5",  # ED
  "y6",  # CA
  "y7",  # DS
  "y8",  # EMN
  "y9",  # MA
  
  # Independent variables (X)
  "x1",  # RUS-Rf
  "x2",  # RUS(-1)-Rf(-1)
  "x3",  # MXUS-Rf
  "x4",  # MEM-Rf
  "x5",  # SMB
  "x6",  # HML
  "x7",  # MOM
  "x8",  # SBGC-Rf
  "x9",  # SBWG-Rf
  "x10", # LHY-Rf
  "x11", # DEFSPR
  "x12", # FRBI-Rf
  "x13", # GSCI-Rf
  "x14", # VIX
  "x15"  # Rf
)

# Assign the new column names
colnames(data) <- new_colnames

# Create a sequence of dates from April 1990 to December 2005
date_sequence <- seq(as.Date("1990-04-01"), as.Date("2005-12-01"), by = "months")

# Add date_sequence as a new column if the lengths match
if (length(date_sequence) == nrow(data)) {
  data <- data.frame(Date = date_sequence, data)
}
install.packages("GGally")
# Load necessary libraries
library(GGally)
library(ggplot2)

# Scatterplot matrix for all variables in relation to y5
ggpairs(data, columns = c("y5", "x1", "x2", "x3", "x4", "x5", "x6", "x7", 
                          "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15"),
        title = "Scatterplot Matrix of Variables with y5",
        upper = list(continuous = wrap("cor", size = 3)),  # Show correlations in upper triangle
        lower = list(continuous = "points")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Full model for y5 including only x1 through x15 as predictors
full_model <- lm(y5 ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15, data = data)


# Display summary of the full model
summary(full_model)

# Stepwise selection of regressors for y5
stepwise_model <- step(full_model, direction = "both")  # "both" adds or removes variables
summary(stepwise_model)  # View the final model

# Diagnostic plots for the stepwise model
par(mfrow = c(2, 2))
plot(stepwise_model)

################################################
################## Question 2 ##################
################################################
# Plot residuals and analyze autocorrelation

# Set up single plot layout for residuals vs. time plot
par(mfrow = c(1, 1))
# Calculate and plot residuals
residuals_stepwise <- residuals(stepwise_model)
plot(residuals_stepwise, type = "l", main = "Residuals vs Time", ylab = "Residuals", xlab = "Time", col = "blue", lwd = 1.5)
abline(h = 0, col = "red", lty = 2)  # Add horizontal line at zero for reference

# Set up 2x1 layout for ACF and PACF of residuals
par(mfrow = c(2, 1))
acf(residuals_stepwise, main = "ACF of Residuals", col = "blue", lwd = 2)
pacf(residuals_stepwise, main = "PACF of Residuals", col = "blue", lwd = 2)

# Define and analyze the timeseries y5
y5 <- ts(data$y5, frequency = 12, start = c(1990, 4))

# Set up 2x1 layout for ACF and PACF of y5
par(mfrow = c(2, 1))
acf(y5, main = "ACF of y5", col = "darkgreen", lwd = 2)
pacf(y5, main = "PACF of y5", col = "darkgreen", lwd = 2)

# Perform Box-Pierce and Ljung-Box tests on y5
box_pierce_test <- Box.test(y5, type = "Box-Pierce")
ljung_box_test <- Box.test(y5, type = "Ljung-Box")

# Display test results
print("Box-Pierce Test Result for y5:")
print(box_pierce_test)
print("Ljung-Box Test Result for y5:")
print(ljung_box_test)

# AR (1) Model for y5
###################

# Prepare the external regressors matrix
x_reg <- cbind(data$x1, data$x2, data$x4, data$x5, data$x6, data$x7, data$x13, data$x14, data$x15)

# Fit the AR(1) model for y5 with external regressors
model_ar <- arima(y5, order = c(1, 0, 0), xreg = x_reg)
print(model_ar)

# Extract residuals and perform unit root test
residuals_ar <- residuals(model_ar)
ur.df(residuals_ar, type = "trend", lags = 1)

# Residual diagnostics plots: ACF, PACF, and QQ plot
par(mfrow = c(3, 1))
acf(residuals_ar, main = "ACF of Residuals")
pacf(residuals_ar, main = "PACF of Residuals")
qqnorm(residuals_ar)
qqline(residuals_ar, col = "red")

# Check for autocorrelation in squared residuals using ACF and PACF
par(mfrow = c(2, 1))
acf(residuals_ar^2, main = "ACF of Squared Residuals")
pacf(residuals_ar^2, main = "PACF of Squared Residuals")

# Ljung-Box test for white noise in residuals (use multiple lags for robustness)
Box.test(residuals_ar, lag = 10, type = "Ljung-Box")
Box.test(residuals_ar^2, lag = 10, type = "Ljung-Box")  # Check for ARCH effects

# Reset plotting layout to single plot
par(mfrow = c(1, 1))

# AR(2) Model
###################
# Prepare the external regressors matrix
x_reg <- cbind(data$x1, data$x2, data$x4, data$x5, data$x6, data$x7, data$x13, data$x14, data$x15)

# Fit AR(2) model
model_ar2 <- arima(y5, order = c(2, 0, 0), xreg = x_reg)
print(model_ar2)

# Extract residuals and perform unit root test
residuals_ar2 <- residuals(model_ar2)
ur.df(residuals_ar2, type = "trend", lags = 2)

# Residual diagnostics plots
par(mfrow = c(3, 1))
acf(residuals_ar2, main = "ACF of Residuals - AR(2)")
pacf(residuals_ar2, main = "PACF of Residuals - AR(2)")
qqnorm(residuals_ar2, main = "Normal Q-Q Plot - AR(2)")
qqline(residuals_ar2, col = "red")

# Check squared residuals
par(mfrow = c(2, 1))
acf(residuals_ar2^2, main = "ACF of Squared Residuals - AR(2)")
pacf(residuals_ar2^2, main = "PACF of Squared Residuals - AR(2)")

# Ljung-Box tests
Box.test(residuals_ar2, lag = 10, type = "Ljung-Box")
Box.test(residuals_ar2^2, lag = 10, type = "Ljung-Box")

# Reset plot layout
par(mfrow = c(1, 1))

# MA(1) Model
###################
# Fit MA(1) model
model_ma1 <- arima(y5, order = c(0, 0, 1), xreg = x_reg)
print(model_ma1)

# Extract residuals and perform unit root test
residuals_ma1 <- residuals(model_ma1)
ur.df(residuals_ma1, type = "trend", lags = 1)

# Residual diagnostics plots
par(mfrow = c(3, 1))
acf(residuals_ma1, main = "ACF of Residuals - MA(1)")
pacf(residuals_ma1, main = "PACF of Residuals - MA(1)")
qqnorm(residuals_ma1, main = "Normal Q-Q Plot - MA(1)")
qqline(residuals_ma1, col = "red")

# Check squared residuals
par(mfrow = c(2, 1))
acf(residuals_ma1^2, main = "ACF of Squared Residuals - MA(1)")
pacf(residuals_ma1^2, main = "PACF of Squared Residuals - MA(1)")

# Ljung-Box tests
Box.test(residuals_ma1, lag = 10, type = "Ljung-Box")
Box.test(residuals_ma1^2, lag = 10, type = "Ljung-Box")

# Reset plot layout
par(mfrow = c(1, 1))

# ARMA(1,1) Model
###################
# Fit ARMA(1,1) model
model_arma11 <- arima(y5, order = c(1, 0, 1), xreg = x_reg)
print(model_arma11)

# Extract residuals and perform unit root test
residuals_arma11 <- residuals(model_arma11)
ur.df(residuals_arma11, type = "trend", lags = 1)

# Residual diagnostics plots
par(mfrow = c(3, 1))
acf(residuals_arma11, main = "ACF of Residuals - ARMA(1,1)")
pacf(residuals_arma11, main = "PACF of Residuals - ARMA(1,1)")
qqnorm(residuals_arma11, main = "Normal Q-Q Plot - ARMA(1,1)")
qqline(residuals_arma11, col = "red")

# Check squared residuals
par(mfrow = c(2, 1))
acf(residuals_arma11^2, main = "ACF of Squared Residuals - ARMA(1,1)")
pacf(residuals_arma11^2, main = "PACF of Squared Residuals - ARMA(1,1)")

# Ljung-Box tests
Box.test(residuals_arma11, lag = 10, type = "Ljung-Box")
Box.test(residuals_arma11^2, lag = 10, type = "Ljung-Box")

# Reset plot layout
par(mfrow = c(1, 1))

## Question 2b
##################

# Plot the monthly returns
plot(y5, type = "l")

# Perform the Ljung-Box test on the return series and its squared values
Box.test(y5, lag = 12, type = "Ljung")
Box.test(y5^2, lag = 12, type = "Ljung")

install.packages("fGarch")
library(fGarch)

# Fit the ARCH model with external regressors using garchFit
m2arch <- garchFit(~arma(1,1) + garch(1,0), data = y5, xreg = x_reg,cond.dist="std", trace = FALSE)
summary(m2arch)
plot(m2arch)

# Fit the GARCH model with external regressors using garchFit
m2garch <- garchFit(~arma(1,1) + garch(1,1), data = y5, xreg = x_reg, cond.dist="std", trace = FALSE)
summary(m2garch)
plot(m2garch)

