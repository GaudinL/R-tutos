library(readr)
sales_example <- read_delim("sales example.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ","), trim_ws = TRUE)

View(sales_example)

sales_model1 <- Sales ~ Period + Month:Period
sales_model2 <- Sales ~ Year + Month:Year
sales_reg1 <- lm(sales_model1, data=sales_example)
sales_reg2 <- lm(sales_model2, data=sales_example)
summary(sales_reg1)
summary(sales_reg2)
# We keep the second model
sales_example$Pred_reg <- predict.lm(sales_reg2, data=sales_example)
View(sales_example)
mse_seas <- mean((sales_example$Pred_seas - sales_example$Sales)^2)
rmse_seas <-sqrt(mse_seas)
mse_reg <- mean((sales_example$Pred_reg - sales_example$Sales)^2)
rmse_reg <- sqrt(mse_reg)
# Eh, it's slightly better
