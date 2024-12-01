# Draft version for MLR
# I did not follow exactly the same order as "MDA_9159_Team_Bits_Project_RMarkdown.rmd"
# for example, I performed EDA before creating dummy variables; otherwise the plots for 
# data visualization are hard to read

# comments for the current pre-processing steps: 
# 1, The variables SALE_AYB_DIFF and SALE_EYB_DIFF 
# are redundant in the presence of the variables SALE_YEAR, AYB, and EYB 
# Perfect multi-collinearity
# 2, consider removing SALE_YEAR given the presence of SALEDATE
# 3, missing data imputation should happen after train/test, split 

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

head(data)

#----
library(lubridate)
library(fastDummies)

# Transform Yes/No for having AC to numerical values
data$AC = ifelse(data$AC == 'Y', 1, 0)

#----
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

#----
# Replace NA with column median
## comment: missing data imputation should happen after train/test split
data = data.frame(lapply(data, function(column) {
  column_median = median(column, na.rm = TRUE)
  column[is.na(column)] = column_median
  column
}))

#----
set.seed(9159)

# Randomly sample 600 data entries for our project
clean_data = data[sample(nrow(data), 600),]

data_train = clean_data[1:500, ]  # First 500 rows for training
data_valid = clean_data[501:600, ]  # Last 100 rows for validation

#----
#Miles's part
#write.csv(data_train, "/Users/Zjxi/Desktop/data_train.csv")
#write.csv(data_valid, "/Users/Zjxi/Desktop/data_valid.csv")
rm(list=ls())
data_train = read.csv("/Users/Zjxi/Desktop/data_train.csv", )
# this reads the original index as a feature so I drop the 1st column
data_train = data_train[, -1]

# Summary Statistics and Data Visualization
### summary stat
summary(data_train)

### correlation matrix
# it shows PRICE are highly correlated with #bathrooms, #rooms, #bedrooms, 
# sale date, GBA (Gross building area), grade, fireplaces, landarea (Land area of property), 
# sale year, sale_ayb_diff, sale_eyb_diff
corr = cor(data_train, use="complete.obs")
library(corrplot)
corrplot(corr)

### histogram
cols = colnames(data_train)
par(mfrow=c(5, 6), mar = c(2, 2, 2, 2))
for (col in cols) {
  hist(data_train[[col]], xlab = col, ylab = "Freq", main=paste("Hist of", col))
}

### inspect categorical variables
categoricals = c("HEAT", "STYLE", "GRADE", "CNDTN", "EXTWALL", "ROOF", "INTWALL", "AC")

par(mfrow=c(2, 4))
for (col in categoricals) {
  hist(data_train[[col]], xlab = col, ylab = "Freq", main=paste("Hist of", col))
}

for (col in categoricals) {
  cat("\nFrequency table for:", col)
  print(table(data_train[[col]])) 
}



### pairplot for relationships
# hard to read
# Save to PDF
# pdf("scatterplot_matrix.pdf", width = 12, height = 12)  
# pairs(data_train, main="Scatterplot matrix")
# dev.off()  # Close the PDF device

### create dummy variables for categorical features
data_train = dummy_cols(
  data_train, 
  select_columns = c("HEAT", "STYLE", "GRADE", "CNDTN", 
                     "EXTWALL", "ROOF", "INTWALL", "AC"), 
  remove_selected_columns = TRUE,
  remove_first_dummy = TRUE
)

# Multiple linear regression
lm = lm(PRICE ~ ., data=data_train)
summary(lm)
mean(summary(lm)$residuals^2)

### check multicolinearity
# find aliases (linearly dependent terms) in a linear model
# The variables SALE_AYB_DIFF and SALE_EYB_DIFF 
# are redundant in the presence of the variables SALE_YEAR, AYB, and EYB
alias(lm)

### rerun MLR, excluding SALE_AYB_DIFF and SALE_EYB_DIFF this time
# this yields the same result
lm = lm(PRICE ~ .-SALE_AYB_DIFF -SALE_EYB_DIFF, data=data_train)
summary(lm)

### Residual diagnostics
par(mfrow=c(2,2))
plot(lm)

# The first plot shows that equal variance and linearity are violated; 
# the violation of E.V. is also confirmed by the BP test.
# The second plot suggests normality assumption is only moderately violated, 
# but S-W test shows it is also violated.
# The fourth plot shows there are 3 influential points.

# bptest: H0: e.v.
library(lmtest)
bptest(lm)

# s.w normality test: H0: observations follow a normal distribution
shapiro.test(resid(lm))

# influential points whose cook's distances > 0.5
infl = c(7, 231, 317)
data_train[infl, ]

### remedy 1: transform response variable
library(MASS)
par(mfrow=c(1,1))
boxcox(lm)

boxcox_result <- boxcox(lm, lambda = seq(-2, 2, by = 0.05))
max_lambda <- boxcox_result$x[which.max(boxcox_result$y)] # ==0.3434343

# apply the transformation
data_train2 = data_train
data_train2$PRICE <- (data_train2$PRICE^max_lambda - 1) / max_lambda

# refit the model
lm_transformed <- lm(PRICE ~ .-SALE_AYB_DIFF -SALE_EYB_DIFF, data = data_train2)
summary(lm_transformed)

### Residual diagnostics
par(mfrow=c(2,2))
plot(lm_transformed)

# The first plot shows that equal variance and linearity are not violated; 
# BP test is not rejected.
# The second plot suggests normality assumption is only moderately violated, 
# but S-W test is still rejected
# The fourth plot shows there are no influential points whose cook's distances > 0.5.

# bptest: H0: e.v.
library(lmtest)
bptest(lm_transformed)

# s.w normality test: H0: observations follow a normal distribution
shapiro.test(resid(lm_transformed))

# influential points whose cook's distances > 0.5
lm_transformed_cookdist = cooks.distance(lm_transformed)
# no influential point
hist(lm_transformed_cookdist)


