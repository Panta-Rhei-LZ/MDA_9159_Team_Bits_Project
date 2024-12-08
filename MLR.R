#----
library(dplyr)

# Read in dataset from github
data = read.csv("https://raw.githubusercontent.com/Panta-Rhei-LZ/MDA_9159_Team_Bits_Project/refs/heads/main/Team_Bits_Data.csv")

# Remove price=0 entries
data = data[data$PRICE != 0, ]

# Remove rows with NA in all columns except 'YR_RMDL'
data = data %>% filter(!if_any(-YR_RMDL, is.na))

# Remove not useful columns
data = data %>% select(-SSL, -OBJECTID, -GIS_LAST_MOD_DTTM, 
                       -QUALIFIED, -SALE_NUM, -BLDG_NUM,
                       -STYLE_D, -STRUCT_D, -GRADE_D,
                       -CNDTN_D, -EXTWALL_D, -ROOF_D,
                       -INTWALL_D, -USECODE, -HEAT_D,
                       -NUM_UNITS, -STRUCT)
#----
library(lubridate)

# Transform Yes/No for having AC to numerical values
data$AC = ifelse(data$AC == 'Y', 1, 0)

# Add SALEYEAR
data$SALE_YEAR = year(ymd_hms(data$SALEDATE))

# Add SALEYEAR and AYB diff
data$SALE_AYB_DIFF = data$SALE_YEAR - data$AYB

# Add SALEYEAR and EYB diff
data$SALE_EYB_DIFF = data$SALE_YEAR - data$EYB

# Add SALEYEAR and YR_RMDL diff
data$SALE_RMDL_DIFF = data$SALE_YEAR - data$YR_RMDL

# Convert SALEDATE column to numeric values
data$SALEDATE = as.numeric(as.Date(data$SALEDATE))

# Replace NA with column median
data = data.frame(lapply(data, function(column) {
  column_median = median(column, na.rm = TRUE)
  column[is.na(column)] = column_median
  column
}))

set.seed(9159)
# Randomly sample 600 data entries
data_plot = data[sample(nrow(data), 600),]
write.csv(data_plot, "/Users/Zjxi/Desktop/data_plot.csv")

clean_data = data_plot

library(fastDummies)
clean_data = dummy_cols(
  clean_data, 
  select_columns = c("HEAT", "STYLE", "GRADE", "CNDTN", 
                     "EXTWALL", "ROOF", "INTWALL", "AC"), 
  remove_selected_columns = TRUE,
  remove_first_dummy = TRUE
)

data_train = clean_data[1:500, ]  # First 500 rows for training
write.csv(data_train, "/Users/Zjxi/Desktop/data_train.csv")

data_valid = clean_data[501:600, ]  # Last 100 rows for validation
write.csv(data_valid, "/Users/Zjxi/Desktop/data_valid.csv")

#----
# 1, Summary Statistics and Data Visualization
rm(list=ls())
data_plot = read.csv("/Users/Zjxi/Desktop/data_plot.csv")
# this reads the original index as a feature so I drop the 1st column
data_plot = data_plot[, -1]

### summary stat
summary(data_plot)

### correlation matrix
# it shows PRICE are highly correlated with #bathrooms, #rooms, #bedrooms, 
# sale date, GBA (Gross building area), grade, fireplaces, landarea (Land area of property), 
# sale year, sale_ayb_diff, sale_eyb_diff
corr = cor(data_plot, use="complete.obs")
library(corrplot)
corrplot(corr, method='color', tl.color='black', tl.tex=0.5)

### histogram
cols = colnames(data_plot)
par(mfrow=c(5, 6), mar = c(2, 2, 2, 2))
for (col in cols) {
  hist(data_plot[[col]], xlab = col, ylab = "Freq", main=paste("Hist of", col))
}

### inspect categorical variables
categoricals = c("HEAT", "STYLE", "GRADE", "CNDTN", "EXTWALL", "ROOF", "INTWALL", "AC")

par(mfrow=c(2, 4))
for (col in categoricals) {
  hist(data_plot[[col]], xlab = col, ylab = "Freq", main=paste("Hist of", col))
}

for (col in categoricals) {
  cat("\nFrequency table for:", col)
  print(table(data_plot[[col]])) 
}

### pairplot for relationships
# make them separately, a few variables at a time
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(data_plot$BATHRM, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "BATHRM", ylab = "PRICE",
     main = "BATHRM vs PRICE")
plot(data_plot$HF_BATHRM, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "HF_BATHRM", ylab = "PRICE",
     main = "HF_BATHRM vs PRICE")
plot(data_plot$ROOMS, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "ROOMS", ylab = "PRICE",
     main = "ROOMS vs PRICE")
plot(data_plot$BEDRM, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "BEDRM", ylab = "PRICE",
     main = "BEDRM vs PRICE")

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(data_plot$GBA, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "Gross Building Area (GBA)", ylab = "PRICE",
     main = "GBA vs PRICE")
plot(data_plot$LANDAREA, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "LANDAREA", ylab = "PRICE",
     main = "LANDAREA vs PRICE")
plot(data_plot$GRADE, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "Grade", ylab = "PRICE",
     main = "Grade vs PRICE")
plot(data_plot$FIREPLACES, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "Fireplaces", ylab = "PRICE",
     main = "Fireplaces vs PRICE")     

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(data_plot$SALEDATE, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "sale date", ylab = "PRICE",
     main = "sale date vs PRICE")
plot(data_plot$SALE_YEAR, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "SALE_YEAR", ylab = "PRICE",
     main = "sale year vs PRICE")
plot(data_plot$SALE_AYB_DIFF, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "sale ayb diff", ylab = "PRICE",
     main = "sale ayb diff vs PRICE")
plot(data_plot$SALE_RMDL_DIFF, data_plot$PRICE,
     pch = 19, col = adjustcolor('firebrick', alpha = 0.1),
     xlab = "sale rmdl diff", ylab = "PRICE",
     main = "sale rmdl diff vs PRICE")


#----
# 2, Multiple linear regression
rm(list=ls())
data_train = read.csv("/Users/Zjxi/Desktop/data_train.csv")
data_train = data_train[, -1]

lm = lm(PRICE ~ ., data=data_train)
summary(lm)

### check multicolinearity
# found aliases (linearly dependent terms) in a linear model:
# SALE_AYB_DIFF ~ SALE_YEAR - AYB, SALE_EYB_DIFF ~ SALE_YEAR - EYB
# EXTWALL_24 ~ ...
# INTWALL_5 ~ GRADE_12
# ROOF_10 ~ 
alias(lm)

### Residual diagnostics
par(mfrow=c(2,2))
plot(lm)

# bptest: H0: e.v.
library(lmtest)
bptest(lm)

# s.w normality test: H0: observations follow a normal distribution
shapiro.test(resid(lm))

# influential points whose cook's distances > 0.5
lm_cookdist = cooks.distance(lm)
sum(lm_cookdist > 0.5, na.rm = TRUE) # 6

# The first plot shows that equal variance and linearity 
# are moderately violated; 
# the violation of E.V. is also confirmed by the BP test.
# The second plot suggests normality assumption is violated, 
# so does S-W test.
# The fourth plot shows there are 6 influential points.

#---- 
# 3, transform response variable
library(MASS)
par(mfrow=c(1,1))
boxcox(lm)
boxcox_result <- boxcox(lm, lambda = seq(-2, 2, by = 0.05))
boxcox_result$x[which.max(boxcox_result$y)] # == 0.3434343
lambda = 0.35

data_train2 = data_train
data_train2$PRICE <- (data_train2$PRICE^lambda - 1) / lambda

# fit the model
lm_transformed <- lm(PRICE ~ ., data = data_train2)
summary(lm_transformed)

### Residual diagnostics
par(mfrow=c(2,2))
plot(lm_transformed)

# bptest: H0: e.v.
library(lmtest)
bptest(lm_transformed)

# s.w normality test: H0: observations follow a normal distribution
shapiro.test(resid(lm_transformed))

# influential points whose cook's distances > 0.5
lm_transformed_cookdist = cooks.distance(lm_transformed)
sum(lm_transformed_cookdist>0.5, na.rm = TRUE) # == 1

# The first plot shows that equal variance and linearity are not violated; 
# BP test is still rejected but improved.
# The second plot suggests normality assumption is violated, 
# S-W test is still rejected but improved.
# The fourth plot shows there is 1 influential point.

#----
# 4, add non-linear predictors
lm_non_linear = lm(PRICE ~ . + poly(BATHRM, 4) + poly(HF_BATHRM, 3)
                   + poly(BEDRM, 3) + poly(EYB, 2), data = data_train2)
summary(lm_non_linear)

### Residual diagnostics
par(mfrow=c(2,2))
plot(lm_non_linear)

# bptest: H0: e.v.
library(lmtest)
bptest(lm_non_linear)

# s.w normality test: H0: observations follow a normal distribution
shapiro.test(resid(lm_non_linear))

# influential points whose cook's distances > 0.5
lm_non_linear_cookdist = cooks.distance(lm_non_linear)
sum(lm_non_linear_cookdist>0.5, na.rm = TRUE) # == 3

# The first plot shows that equal variance and linearity are not violated; 
# BP test is still rejected but improved.
# The second plot suggests normality assumption is violated, 
# S-W test is still rejected but improved.
# The fourth plot shows there are 3 influential point.


#----
# 5, compute performance metrics a validation set
data_valid = read.csv("/Users/Zjxi/Desktop/data_valid.csv")
data_valid = data_valid[, -1]

### model 1: lm
yp_lm = predict(lm, newdata=data_valid)
mse_lm = mean((data_valid$PRICE - yp_lm)^2) # 215,891,800,787
rmse_lm = sqrt(mse_lm) # 464,641
# Root Mean Squared Logarithmic Error, rmsle
rmsle_lm = sqrt(mean((log(data_valid$PRICE)- log(yp_lm))^2))  #NaN, In log(yp_lm) : NaNs produced


### model 2: lm_transformed 
# to calculate MSE, transform Y back
yp_lm_transformed = predict(lm_transformed, newdata=data_valid)

inv_powerfun = function(y_transformed, lambda) {
  if (lambda == 0) {
    return(exp(y_transformed))
  } else {
    return((lambda * y_transformed + 1)^(1/lambda))
  }
}
yp_lm_transformed = inv_powerfun(yp_lm_transformed, 0.35)
mse_lm_transformed = mean((data_valid$PRICE - yp_lm_transformed)^2) # 137,360,712,175
rmse_lm_transformed = sqrt(mse_lm_transformed) # 370,622
rmsle_lm_transformed = sqrt(mean((log(data_valid$PRICE)- log(yp_lm_transformed))^2))  #0.40676098


### model 3: lm_non_linear
yp_lm_nonlinear = predict(lm_non_linear, newdata=data_valid)
yp_lm_nonlinear = inv_powerfun(yp_lm_nonlinear, 0.35)
mse_lm_nonlinear = mean((data_valid$PRICE - yp_lm_nonlinear)^2) # 112,664,735,519
rmse_lm_nonlinear = sqrt(mse_lm_nonlinear) # 335,655
rmsle_lm_nonlinear = sqrt(mean((log(data_valid$PRICE)- log(yp_lm_nonlinear))^2))  #0.42485906

