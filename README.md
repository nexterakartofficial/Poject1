8TH
x <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131)
y <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48)

# Linear regression model
relation <- lm(y ~ x)
summary(relation)

# Prediction for height 170cm
df <- data.frame(x = 170)
result <- predict(relation, df)
print(result)

# PNG file mein plot save karo
png(file = "linear_regression.png")

plot(y ~ x,
     col  = "blue",
     main = "Height & Weight Regression",
     xlab = "Height in cm",
     ylab = "Weight in kg")

abline(relation, col = "red")   # regression line alag line pe

dev.off()

getwd()







9th FORECAST
# Sales data
sales <- c(120,130,150,170,160,180,200,220,210,230,250,270,
           125,135,155,175,165,185,205,225,215,235,255,275)

ts_data <- ts(sales, start = c(2023, 1), frequency = 12)
plot(ts_data)

decomp <- decompose(ts_data, type="additive")
plot(decomp)

ma <- filter(ts_data, rep(1/3,3), sides=2)
plot(ts_data)
lines(ma, col="red")

library(tseries)
adf.test(ts_data)

library(forecast)
model <- auto.arima(ts_data)
future <- forecast(model, h=6)
plot(future)

accuracy(future)


10TH
# Dataset
hours  <- c(1, 2, 3, 4, 5, 6, 7, 8)
result <- c(0, 0, 0, 0, 1, 1, 1, 1)

student_data <- data.frame(hours, result)  # ← 'data' ki jagah 'student_data'

# Model
model <- glm(result ~ hours,
             data   = student_data,         # ← yahan bhi
             family = binomial)
summary(model)

# Probability
predicted_prob <- predict(model, type = "response")
predicted_prob

# Class
predicted_class <- ifelse(predicted_prob > 0.5, 1, 0)
predicted_class

# Plot
plot(student_data$hours, student_data$result,  # ← yahan bhi
     xlab = "Study Hours",
     ylab = "Pass(1)/Fail(0)",
     main = "Logistic Regression",
     )

curve(predict(model,
              data.frame(hours = x),
              type = "response"),
      add = TRUE,
      col = "blue",
      lwd = 2)


      RUN WITH CTRL+A
      CTRL+ENTER
