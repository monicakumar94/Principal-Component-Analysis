

# PCA - Principal Component Analysis

## Read in Cereals data
cereals.df <- read.csv("Cereals.csv")
str(cereals.df)

### compute PCs on two dimensions
pcs <- prcomp(data.frame(cereals.df$calories, cereals.df$rating)) 
summary(pcs) 
pcs$rot # rotation matrix
scores <- pcs$x
head(scores, 5)

### PCA on 13 variables
pcs13 <- prcomp(na.omit(cereals.df[,-c(1:3)])) 
summary(pcs13)
pcs13$rot

### PCA using Normalized variables
pcs.cor <- prcomp(na.omit(cereals.df[,-c(1:3)]), scale. = T)
summary(pcs.cor)
pcs.cor$rot

housing.df <- read.csv("BostonHousing.csv")
head(housing.df)

### Create Training and Validation sets
set.seed(1)  
train.index <- sample(c(1:dim(housing.df)[1]), 
                      0.6*dim(housing.df)[1])  
valid.index <- setdiff(c(1:dim(housing.df)[1]), train.index)  
train.df <- housing.df[train.index, ]
valid.df <- housing.df[valid.index, ]


# 2. Regression
reg <- lm(MEDV ~ CRIM + CHAS + RM, data = train.df)
summary(reg)

# 3. generate prediction
reg$coef %*% c(1, 0.1, 0, 6)

# 4. Relationship among INDUS, NOX, and TAX
cor(train.df[,c("INDUS", "NOX", "TAX")])

# 5. Correlation among numerical variables
cor(train.df)


# 6. Exhaustive Search regression

subset_search <- regsubsets(MEDV ~ .,
                            data = train.df,
                            nbest = 1,
                            nvmax = dim(train.df)[2],
                            method = "exhaustive")
sum <- summary(subset_search)
sum$which
sum$adjr2