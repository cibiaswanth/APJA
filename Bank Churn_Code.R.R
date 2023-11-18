#Section-1 [To open the csv file:]
setwd(dirname(file.choose()))
getwd()
b <- read.csv("Dis2.csv", stringsAsFactors = FALSE)
str(b)
# section-2 [Checking the missing values:]
library(Amelia)
library(funModeling)
library(tidyverse)
library(Hmisc)
library(ggplot2)
library(forcats)
library(dplyr)
apply(b, MARGIN = 2, FUN = function(x) sum(is.na(x)))
missmap(b, col = c("black", "green"), legend = FALSE)
str(b)
st <-b

boxplot(b,main="Boxplot of All variables",xlab="Dependent and Independent Variables",ylab="count",col="Bisque")

#section 3
# Load required libraries
library(tidyverse)

df_min_max_scaled <- b

# Min Max  technique:
for (column in colnames(df_min_max_scaled)) {
  df_min_max_scaled[[column]] <- (df_min_max_scaled[[column]] - min(df_min_max_scaled[[column]])) / (max(df_min_max_scaled[[column]]) - min(df_min_max_scaled[[column]]))
}
# View normalized data
print(df_min_max_scaled)

# Visualize the Min Max scaling through box plot:
set.seed(42)
df_min_max_scaled <- as.data.frame(matrix(runif(640), nrow = 40, ncol = 17, 
                                          dimnames = list(NULL, c("Age","Jt","Mtl",
                                                                  "Et","Df","Yb","Hl",
                                                                  "Pl","Ct","Dy","Mn",
                                                                  "Lc","Cp","Pd","Pc","Pm","Tm"))))
boxplot(df_min_max_scaled,main="Min Max scaling",xlab=" Dependent and Independent Variables",ylab="count",col="#1f77b4")

#Z-Score-standard Deviation:
London.z1 <- apply(b, MARGIN = 2, FUN = function(x) (x - mean(x))/sd(x))
London.z2 <- apply(b, MARGIN = 2, FUN = function(x) (x - mean(x))/(2*sd(x)))
boxplot(London.z1,main= "Standard deviation 1",xlab="Independent Variables",ylab="count",col="#9467bd")
boxplot(London.z2,main= "Standard deviation 2",xlab="Independent Variables",ylab="count",col="#9467bd")

#Soft max Scaling:
library(DMwR2)
#insyahelp(SoftMax)

sts <- apply(b, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 1, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 1",col="#ff7f0e",xlab="Independent Variables")

sts <- apply(b, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 2, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 2",col="#ff7f0e",xlab="Independent Variables")

sts <- apply(b, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 3, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 3",col="#ff7f0e",xlab="Independent Variables")

sts <- apply(b, MARGIN = 2, FUN = function(x) (SoftMax(x,lambda = 4, mean(x), sd(x))))
boxplot (sts, main = "Soft Max, lambda = 4",col="#ff7f0e",xlab="Independent Variables")

st <-b
#ks normality test:
ks.test(df_min_max_scaled$Age,"pnorm", mean(df_min_max_scaled$Age), sd(df_min_max_scaled$Age))
ks.test(df_min_max_scaled$Jt,"pnorm",mean(df_min_max_scaled$Jt), sd(df_min_max_scaled$Jt))
ks.test(df_min_max_scaled$Mtl,"pnorm", mean(df_min_max_scaled$Mtl), sd(df_min_max_scaled$Mtl))
ks.test(df_min_max_scaled$Et,"pnorm", mean(df_min_max_scaled$Et), sd(df_min_max_scaled$Et))
ks.test(df_min_max_scaled$Df,"pnorm", mean(df_min_max_scaled$Df), sd(df_min_max_scaled$Df))
ks.test(df_min_max_scaled$Yb,"pnorm", mean(df_min_max_scaled$Yb), sd(df_min_max_scaled$Yb))
ks.test(df_min_max_scaled$Hl,"pnorm", mean(df_min_max_scaled$Hl), sd(df_min_max_scaled$Hl))
ks.test(df_min_max_scaled$Pl,"pnorm", mean(df_min_max_scaled$Pl), sd(df_min_max_scaled$Pl))
ks.test(df_min_max_scaled$Ct,"pnorm", mean(df_min_max_scaled$Ct), sd(df_min_max_scaled$Ct))
ks.test(df_min_max_scaled$Dy,"pnorm", mean(df_min_max_scaled$Dy), sd(df_min_max_scaled$Dy))
ks.test(df_min_max_scaled$Mn,"pnorm", mean(df_min_max_scaled$Mn), sd(df_min_max_scaled$Mn))
ks.test(df_min_max_scaled$Lc,"pnorm", mean(df_min_max_scaled$Lc), sd(df_min_max_scaled$Lc))
ks.test(df_min_max_scaled$Cp,"pnorm", mean(df_min_max_scaled$Cp), sd(df_min_max_scaled$Cp))
ks.test(df_min_max_scaled$Pd,"pnorm", mean(df_min_max_scaled$Pd), sd(df_min_max_scaled$Pd))
ks.test(df_min_max_scaled$Pc,"pnorm", mean(df_min_max_scaled$Pc), sd(df_min_max_scaled$Pc))
ks.test(df_min_max_scaled$Pm,"pnorm", mean(df_min_max_scaled$Pm), sd(df_min_max_scaled$Pm))
ks.test(df_min_max_scaled$Tm,"pnorm",mean(df_min_max_scaled$Tm),sd(df_min_max_scaled$Tm))

#Histogram for normality of dependent var:
histogram(x=df_min_max_scaled$Tm,xlab="Term_Deposit",main="Distribution of Dependent Variable")

#Co-relation Map:
library(corrgram)
corrgram(df_min_max_scaled, order=FALSE, cor.method = "pearson", lower.panel=panel.cor,
         upper.panel=panel.pie, text.panel=panel.txt, main="Pearson Co relation Method")
title(xlab = "Correlation between Dependent and Independent variables", font.lab = 2)

#Correlations among numeric variables :
cor.matrix <- cor(df_min_max_scaled, use = "pairwise.complete.obs", method = "pearson")
round(cor.matrix, digits = 2)
cor.df <- as.data.frame(cor.matrix)
View(cor.df)
round(cor.df,2)

#Histogram 
plot_num(st,bins=30)


#Term_deposit count:
ggplot(data = b, aes(x = Term_deposit, fill = Term_deposit)) +
  geom_bar() +
  labs(title = "Histogram of Term Deposit",x = "Term Deposit",y = "Count")


#Age vs Balance:
d <- as.data.frame(st)
a_colors<- c("#0072B2", "#D55E00", "#009E73", "#E69F00", "#F0E442", "#CC79A7")
ggplot(d, aes(x = Age, y = Y_balance)) +
  geom_line(color = a_colors[1]) +
  geom_point(color = a_colors[4]) +
  labs(title = "Age vs Balance",
       x = "Age",
       y = "Balance") +
  scale_x_continuous(breaks = seq(20, 90, 5), limits = c(20, 90)) +
  scale_y_continuous(breaks = seq(0, 80000, 10000), limits = c(0, 80000)) +
  theme_classic()

#Job_type vs balance:
ggplot(d, aes(x = Job_type, y = Y_balance, fill = Job_type)) +
  geom_bar(stat = "identity") +
  labs(title = "Job_Type vs Balance",
       x = "Job_Type",
       y = "Balance") +
  scale_y_continuous(breaks = seq(0,2000000,500000), limits=c(0,2000000))+
  theme(legend.position = "right", 
        legend.title = element_text(size = 12, face = "bold"), 
        legend.text = element_text(size = 10), 
        axis.text = element_text(size = 10),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
        plot.margin = unit(c(1, 5, 1, 1), "lines"))

#Lc_duration vs Pm_campaign
ggplot(d, aes(x = Pm_campaign, y = Lc_duration,fill = Pm_campaign)) +
  geom_bar(stat = "identity") +
  labs(title = "Call Duration in seconds According to Previous Outcome",
       x = "Previous Outcome",
       y = "Call Duration") +
  scale_y_continuous(breaks = seq(0,350,50), limits=c(0,350))
  theme_minimal()
  
#Personal loan Vs Job_type:
  ggplot(b, 
         aes(x = Job_type, 
             fill = P_loan)) + 
    geom_bar(position = position_dodge(preserve = "single"), color = "black") +
    scale_fill_manual(values = c("#619cff", "#f8766d")) +
    labs(title = "Personal loan vs Job_type")+theme(legend.position = "right", 
         axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
         plot.margin = unit(c(1, 5, 1, 1), "lines"))
  
#housing loan vs Job_type:
  my_colors <- c("#009E73", "#56B4E9")
  
  ggplot(b, aes(x = Job_type, fill = Housing_loan)) +
    geom_bar(position = position_dodge(preserve = "single")) +
    labs(title = "Housing loan vs Job_type", fill = "Housing Loan") +
    scale_fill_manual(values = my_colors) +theme(legend.position = "right",
          legend.title = element_text(size = 12, face = "bold"),
          legend.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          plot.margin = unit(c(1, 5, 1, 1), "lines"),
          axis.text = element_text(size = 10))
  

#Term_deposit vs Age:
  ggplot(data = b, aes(x = Age, fill = Term_deposit)) +
    geom_bar() +
    labs(x = "Age", y = "Count", fill = "Term Deposit") +  labs(title = "Term deposit vs Age ")+
    scale_x_continuous(breaks = seq(20, 90, 2), limits = c(20, 90)) +
    scale_fill_manual(values = c("#666666", "#619cff"))+theme(legend.position = "right", 
          legend.title = element_text(size = 12, face = "bold"), 
          legend.text = element_text(size = 10), 
          axis.text = element_text(size = 10))
  

#Term_deposit vs Job_type:
  ggplot(data = b, aes(x = Job_type, fill = Term_deposit)) +
    geom_bar() +
    labs(x = "Job type", y = "Count", fill = "Term Deposit") +
    labs(title = "Term deposit vs Job Type") +
    scale_fill_manual(values = c("#666666", "#619cff")) +
    theme(legend.position = "right", 
          legend.title = element_text(size = 12, face = "bold"), 
          legend.text = element_text(size = 10), 
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          plot.margin = unit(c(1, 5, 1, 1), "lines"))
  
 

#housing loan vs Job_type vs Age:  
 df <- b %>%
    filter(Job_type %in% c("technician", "management", "retired", "housemaid"),
           Housing_loan %in% c("yes", "no"))
  ggplot(df, aes(x = Age, fill = Housing_loan)) +
    geom_histogram(binwidth = 5, position = "dodge") +
    facet_wrap(~ Job_type, ncol = 2) +
    scale_fill_manual(values = c("#619cff", "#f8766d")) +
    labs(x = "Age", y = "Count", fill = "Housing Loan Vs Job Type") +
    scale_x_continuous(breaks = seq(20, 90, 5), limits = c(20, 90)) +
    theme_classic() +
    theme(legend.position = "top", 
          legend.title = element_text(size = 12, face = "bold"), 
          legend.text = element_text(size = 10), 
          axis.text = element_text(size = 10))


#converting Categorical values into numerical:
st$Default <- ifelse(st$Default=="yes",1,0)
#st$Default <- as.integer(st$Default)
st$Default <- as.numeric(st$Default)

st$Housing_loan <- ifelse(st$Housing_loan=="yes",1,0)
#st$Housing_loan <- as.integer(st$Housing_loan)
st$Housing_loan <- as.numeric(st$Housing_loan)

st$P_loan <- ifelse(st$P_loan=="yes",1,0)
#st$P_loan <- as.integer(st$P_loan)
st$P_loan <- as.numeric(st$P_loan)

st$Term_deposit <- ifelse(st$Term_deposit=="yes",1,0)
#st$Term_deposit <- as.integer(st$Term_deposit)
st$Term_deposit <- as.numeric(st$Term_deposit)
view(st$Term_deposit)

#Contact:
st$Contact <- ifelse(st$Contact=="cellular",1,st$Contact)
st$Contact <- ifelse(st$Contact=="unknown",2,st$Contact)
st$Contact <- ifelse(st$Contact=="telephone",3,st$Contact)
st$Contact <- as.integer(st$Contact)
#st$Contact <- as.numeric(st$Contact)

#Marital:
st$marital <- ifelse(st$marital=="married",1,st$marital)
st$marital <- ifelse(st$marital=="single",2,st$marital)
st$marital <- ifelse(st$marital=="divorced",3,st$marital)
st$marital <- as.integer(st$marital)
#st$marital <- as.numeric(st$marital)

#Education Type:
st$Education_type <- ifelse(st$Education_type=="unknown",1,st$Education_type)
st$Education_type <- ifelse(st$Education_type=="secondary",2,st$Education_type)
st$Education_type <- ifelse(st$Education_type=="primary",3,st$Education_type)
st$Education_type <- ifelse(st$Education_type=="tertiary",4,st$Education_type)
st$Education_type <- as.integer(st$Education_type)
#st$Education_type <- as.numeric(st$Education_type)

#Pm_Campaign:
st$Pm_campaign <- ifelse(st$Pm_campaign=="failure",1,st$Pm_campaign)
st$Pm_campaign <- ifelse(st$Pm_campaign=="success",2,st$Pm_campaign)
st$Pm_campaign <- ifelse(st$Pm_campaign=="other",3,st$Pm_campaign)
st$Pm_campaign <- ifelse(st$Pm_campaign=="unknown",4,st$Pm_campaign)
st$Pm_campaign <- as.integer(st$Pm_campaign)
#st$Pm_campaign <- as.numeric(st$Pm_campaign)

#Job_type:
st$Job_type <- ifelse(st$Job_type=="technician",1,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="management",2,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="housemaid",3,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="retired",4,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="services",5,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="admin.",6,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="unknown",7,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="unemployed",8,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="entrepreneur",9,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="student",10,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="blue-collar",11,st$Job_type)
st$Job_type <- ifelse(st$Job_type=="self-employed",12,st$Job_type)
st$Job_type <- as.integer(st$Job_type)
#st$Job_type <- as.numeric(st$Job_type)

#Month:
st$Month <- ifelse(st$Month=="jan",1,st$Month)
st$Month <- ifelse(st$Month=="feb",2,st$Month)
st$Month <- ifelse(st$Month=="mar",3,st$Month)
st$Month <- ifelse(st$Month=="apr",4,st$Month)
st$Month <- ifelse(st$Month=="may",5,st$Month)
st$Month <- ifelse(st$Month=="jun",6,st$Month)
st$Month <- ifelse(st$Month=="jul",7,st$Month)
st$Month <- ifelse(st$Month=="aug",8,st$Month)
st$Month <- ifelse(st$Month=="sep",9,st$Month)
st$Month <- ifelse(st$Month=="oct",10,st$Month)
st$Month <- ifelse(st$Month=="nov",11,st$Month)
st$Month <- ifelse(st$Month=="dec",12,st$Month)
st$Month <- as.integer(st$Month)
#st$Month <- as.numeric(st$Month)
str(st)
summary(st)

#SVM Classification:
# Load necessary libraries
library(e1071)
library(caret)

# Convert the target variable to a factor
st$Term_deposit <- as.factor(st$Term_deposit)
#view(st$Term_deposit)
# Split data into train and test sets
set.seed(123)
train_index <- createDataPartition(st$Term_deposit, p = 0.9, list = FALSE)
train_data <- st[train_index, ]
test_data <- st[-train_index, ]

# Train the SVM model
svm_model <- svm(Term_deposit ~ ., data = train_data, kernel = "linear", cost = 1, gamma = 1)

# Model performance on train data
train_pred <- predict(svm_model, newdata = train_data)
train_acc <- mean(train_pred == train_data$Term_deposit)
train_cm <- confusionMatrix(train_data$Term_deposit, train_pred)

# Model performance on test data
test_pred <- predict(svm_model, newdata = test_data)
test_acc <- mean(test_pred == test_data$Term_deposit)
test_cm <- confusionMatrix(test_data$Term_deposit, test_pred)

# Results
cat(paste0("Train Accuracy: ", train_acc*100, "%\n"))
cat(paste0("Test Accuracy: ", test_acc*100, "%\n"))
print(train_cm)
print(test_cm)

# Print selected variables in final model
summary(svm_model)

# Print selected independent variables with positive impact
coef(svm_model)[abs(coef(svm_model)) > 0.01]


# Random Forest classification:
library(randomForest)
library(caret)

st$Term_deposit <- as.factor(st$Term_deposit)  
view(st$Term_deposit)

# Split data into train and test sets
set.seed(123)
train_index <- createDataPartition(st$Term_deposit, p = 0.9, list = FALSE)
train_data <- st[train_index, ]
test_data <- st[-train_index, ]

# Train a random forest model
rf_model <- randomForest(Term_deposit ~ ., 
                         data = train_data, 
                         ntree = 10000,  # increase number of trees
                         mtry = 4,  # set the number of features randomly sampled as 3
                         maxnodes = 100,  # set the maximum number of nodes to 20
                         importance = TRUE)

# Model performance on train data
train_pred <- predict(rf_model, newdata = train_data)
train_acc <- mean(train_pred == train_data$Term_deposit)
train_cm <- confusionMatrix(train_data$Term_deposit, train_pred)

# Model performance on test data
test_pred <- predict(rf_model, newdata = test_data)
test_acc <- mean(test_pred == test_data$Term_deposit)
test_cm <- confusionMatrix(test_data$Term_deposit, test_pred)

# Print model performance metrics
cat(paste0("Train Accuracy: ", train_acc * 100, "%\n"))
cat(paste0("Test Accuracy: ", test_acc * 100, "%\n"))
print(train_cm)
print(test_cm)

summary(rf_model)
# Feature importance plot
varImpPlot(rf_model)

#Decision tree
# Load necessary libraries
library(caret)
library(rpart)
library(rpart.plot)

# Make sure target variable is a factor
st$Term_deposit <- as.factor(st$Term_deposit)

# Split data into train and test sets
set.seed(123)
train_index <- createDataPartition(st$Term_deposit, p = 0.8, list = FALSE)
train_data <- st[train_index, ]
test_data <- st[-train_index, ]

# Tune hyper-parameters using cross-validation
ctrl <- trainControl(method = "cv", number = 5)
grid <- expand.grid(cp = seq(0, 0.05, 0.005))
dt_model <- train(Term_deposit ~ ., data = train_data, method = "rpart",
                  trControl = ctrl, tuneGrid = grid)

# Model performance on train data
train_pred <- predict(dt_model, newdata = train_data)
train_acc <- mean(train_pred == train_data$Term_deposit)
train_cm <- confusionMatrix(train_data$Term_deposit, train_pred)

# Model performance on test data
test_pred <- predict(dt_model, newdata = test_data)
test_acc <- mean(test_pred == test_data$Term_deposit)
test_cm <- confusionMatrix(test_data$Term_deposit, test_pred)

cat(paste0("Train Accuracy: ", train_acc * 100, "%\n"))
cat(paste0("Test Accuracy: ", test_acc * 100, "%\n"))

print(train_cm)
print(test_cm)

#summary
summary(dt_model)
# Visualize decision tree
prp(dt_model$finalModel)

# Evaluate variable importance
varImp(dt_model$finalModel)

# Print the final decision tree model
print(dt_model$finalModel)

