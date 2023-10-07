# 099
代码网页地址：https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data
以下是R代码：
library(reshape2)
library(dplyr)     
library(ggplot2)  
library(corrplot)

#Load Dataset
data<-read.csv("/Users/guanguan/Desktop/dissertation/dft_road_casualty_statistics.csv") 

#Intercept the required data columns
data <- data[data[,"accident_year"] >= 2004 & data[,"accident_year"] <= 2020, ]

# remove the same value column
data <- subset(data, select = -c(lsoa_of_casualty, casualty_imd_decile, casualty_home_area_type, pedestrian_road_maintenance_worker, 
                                 accident_index, accident_reference, vehicle_reference))

# Fill in the missing values of each column as the multitude of the corresponding columns
modes <- sapply(data, function(col) {
  mode_value <- as.numeric(names(sort(table(col), decreasing = TRUE)[1]))
  return(mode_value)
})

for (col in colnames(data)) {
  data[[col]][data[[col]] == -1] <- modes[col]
}

#Check for missing values
sum(is.na(data))

#
ncol(data)

#View attributes of the data
summary(data)

mean(data$age_of_casualty, trim = 0.1)
mean(data$age_of_casualty, trim = 0.2)
median(data$age_of_casualty)

#IQR
quantile(data$age_of_casualty)


  #All casualty data
new_data <- data %>%
  group_by(accident_year) %>%
  summarize(total_casualties = n())

  # Create bar and line charts
ggplot(new_data, aes(x = factor(accident_year), y = total_casualties)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_line(color = "red", linewidth = 1, aes(group = 1)) +  # 添加折线
  labs(title = "Accident years and trends in casualties", x = "Year", y = "Casualties") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#bar chart
barplot(table(data$age_of_casualty), beside = TRUE)
barplot(table(data$age_of_casualty, data$sex_of_casualty),beside=TRUE)
barplot(table(data$age_of_casualty, data$accident_year),xlab="Year",ylab="Quantity",main="The quantity of casualty divided into different years",beside=TRUE)


#density map
plot(density(data$age_of_casualty))
plot(density(data$age_of_casualty,from=0),main = "Empirical Density of Age")

#Histgram直方图
hist(data$age_of_casualty,main="Histgram of Age")

#直方图+密度图
hist(data$age_of_casualty,freq = FALSE)
lines(density(data$age_of_casualty),main="Histgram of Ages",col="red")

#boxplot
boxplot(data$age_of_casualty)

# cor and cov heatmap
numeric_data <- data[, sapply(data, is.numeric)]
correlation_matrix <- cor(numeric_data)
melted_matrix = melt(correlation_matrix)
ggplot(data = melted_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +  # 设置颜色映射
  labs(title = "Correlation Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

covarience_matrix = cov(numeric_data)
covarience_matrix = melt(covarience_matrix)
ggplot(data = covarience_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red") +  # 设置颜色映射
  labs(title = "Covarience Heatmap", x = "", y = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



 #probability distributions
y_cdf <- pnorm(data$age_of_casualty, mean=0, sd=1)
 # 绘制累积分布函数
plot(data$age_of_casualty, y_cdf, type="l", col="red", lwd=2, xlab="x", ylab="Cumulative Probability", main="Cumulative distribution function of normal distribution")


#Probability Density Function, PDF
y <- dnorm(data$age_of_casualty, mean=0, sd=1)
 # 绘制概率密度函数
plot(data$age_of_casualty, y, type="l", col="blue", lwd=2, xlab="x", ylab="Density", main="The probability density function of a normal distribution")




class(data$age_of_casualty)

# calculate ECDF
ecdf_data <- ecdf(data$age_of_casualty)
plot(ecdf_data, main = "Empirical Cumulative Distribution Function", xlab = "X", ylab = "ECDF")

#---------------------------------------------------------------------------------------

#查看性别分布
data$sex_of_casualty <- factor(data$sex_of_casualty,
                               levels = c("1", "2", "9"),
                               labels = c("Male", "Female", "Unknown"))

manual_colors <- c("Male" = "blue", "Female" = "pink", 
                   "Unknown" = "gray")

ggplot(data, aes(x = sex_of_casualty, fill = sex_of_casualty)) +
  geom_bar() +
  labs(title = "Gender Distribution of Casualties",
       x = "Gender of Casualty",
       y = "Number of Cases") +
  scale_x_discrete(labels = c("Male", "Female", "Unknown")) +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))



# 不同性别的伤亡类型分布
gender_casualty_type <- data %>%
  group_by(sex_of_casualty, casualty_type) %>%
  summarise(num_cases = n(),.groups = "drop") %>%
  ggplot(aes(x = casualty_type, y = num_cases, fill = sex_of_casualty)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Distribution of Casualty Types by Gender",
       x = "Casualty Type",
       y = "Number of Cases",
       fill = "Gender") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_discrete(labels = c("Male", "Female", "Unknown"))+ 
  theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
print(gender_casualty_type)

#表格。查看伤亡类型下的伤亡数量
casualty_counts <- data %>%
  group_by(casualty_type) %>%
  summarise(num_cases = n(), .groups = "drop")

print(casualty_counts, n=100)






#查看年龄分布
ggplot(data, aes(x = age_of_casualty)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Age Distribution of Casualties",
       x = "Age of Casualty",
       y = "Number of Cases") +
  theme_minimal() +
  theme(legend.position = "top") +
  theme(plot.title = element_text(hjust = 0.5))


# 不同伤亡严重程度的年龄分布
data$casualty_severity <- factor(data$casualty_severity,
                                 levels = c("1", "2", "3"),
                                 labels = c("Fatal", "Serious", "Slight"))

manual_colors <- c("Fatal" = "blue", "Serious" = "pink","sight" = "gray")

 #按伤亡严重程度划分的年龄分布
ggplot(data, aes(x = age_of_casualty, fill = casualty_severity)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Age Distribution by Casualty Severity",
       x = "Age of Casualty",
       y = "Number of Cases",
       fill = "Casualty Severity") +
  theme_minimal()+
  theme(legend.position = "top") 


# 年龄和伤亡严重程度的关系
ggplot(data, aes(x = age_band_of_casualty, fill = casualty_severity)) +
  geom_bar(position = "dodge") +
  labs(title = "Relationship between Age Band and Casualty Severity",
       x = "Age Band of Casualty",
       y = "Number of Cases",
       fill = "Casualty Severity") +
  theme_minimal() +
  theme(legend.position = "top")



# 不同伤亡类型的分布
data$casualty_type <- as.character(data$casualty_type)

data$casualty_type <- factor(data$casualty_type,
                             levels = c("0", "1", "2","3","4","5","8","9","10","11","16","17","18","19","20","21","22","23","90","97","98","99","103","104","105","106","108","109","110","113"),
                             labels = c("Pedestrian", "Cyclist", "Motorcycle 50cc and under rider or passenger","Motorcycle 125cc and under rider or passenger","Motorcycle over 125cc and up to 500cc rider or  passenger",
                                        "Motorcycle over 500cc rider or passenger","Taxi/Private hire car occupant","Car occupant","Minibus (8 - 16 passenger seats) occupant","Bus or coach occupant (17 or more pass seats)",
                                        "Horse rider","Agricultural vehicle occupant","Tram occupant","Van / Goods vehicle (3.5 tonnes mgw or under) occupant","Goods vehicle (over 3.5t. and under 7.5t.) occupant",
                                        "Goods vehicle (7.5 tonnes mgw and over) occupant","Mobility scooter rider","Electric motorcycle rider or passenger","Other vehicle occupant","Motorcycle - unknown cc rider or passenger",
                                        "Goods vehicle (unknown weight) occupant","Unknown vehicle type (self rep only)","Motorcycle - Scooter (1979-1998)","Motorcycle (1979-1998)","Motorcycle - Combination (1979-1998)",
                                        "Motorcycle over 125cc (1999-2004)","Taxi (excluding private hire cars) (1979-2004)","Car (including private hire cars) (1979-2004)","Minibus/Motor caravan (1979-1998)",
                                        "Goods over 3.5 tonnes (1979-1998)"))

ggplot(data, aes(x = casualty_type)) +
  geom_bar() +
  labs(title = "Distribution of Casualty Types",
       x = "Casualty Type",
       y = "Number of Cases") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

# 不同伤亡类型的年龄分布
ggplot(data, aes(x = casualty_type, y = age_of_casualty, color = casualty_type)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Casualty Type",
       x = "Casualty Type",
       y = "Age of Casualty",
       color = "Casualty Type") +
  theme_minimal() +
  theme(legend.position = "top") + 
  theme(axis.text.x = element_text(angle=90, vjust = 0.5, hjust=1),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))


#----报告分水岭1--（上面已经写好了yeah yeah）--------#--------------------------------------------
# 不同年份的伤亡类型趋势
casualty_trends <- data %>%
  group_by(accident_year, casualty_type) %>%
  summarise(num_cases = n(),.groups = "drop") %>%
  ggplot(aes(x = accident_year, y = num_cases, color = casualty_type)) +
  geom_line() +
  labs(title = "Trends in Casualty Types Over Years",
       x = "Year",
       y = "Number of Cases",
       color = "Casualty Type") +
  theme_minimal() +
  theme(legend.position = "right")
print(casualty_trends)


# 不同年份的伤亡程度趋势
casualty_trends_year <- data %>%
  group_by(accident_year, casualty_severity) %>%
  summarise(num_cases = n(),.groups = "drop") %>%
  ggplot(aes(x = accident_year, y = num_cases, color = casualty_severity)) +
  geom_line() +
  labs(title = "Trends in Casualty Severity Over Years",
       x = "Year",
       y = "Number of Cases",
       color = "Casualty Severity") +
  theme_minimal() +
  theme(legend.position = "top")
print(casualty_trends_year)

#表格。查看伤亡类型下的伤亡数量
casualty_trends_year_4 <- data %>%
  group_by(casualty_type,accident_year) %>%
  summarise(num_cases = n(), .groups = "drop")

print(casualty_trends_year_4, n=330)




# 绘制相关性矩阵
cor_matrix <- data %>%
  select_if(is.numeric) %>%
  cor()

corrplot(cor_matrix, method = "color")


# 分组统计分析
summary_table <- data %>%
  group_by(casualty_severity, sex_of_casualty) %>%
  summarise(
    avg_age = mean(age_of_casualty, na.rm = TRUE),
    num_cases = n(),.groups = "drop"
  )
print(summary_table)

#--------MODEL-------MODEL--------ＭＯＤＥＬ-----ＭＯＤＥＬ----model--------model---------------------------------------
  
# 使用线性回归模型
model <- lm(casualty_severity ~ age_of_casualty + sex_of_casualty + casualty_type, data = data)
# 查看回归模型摘要
summary(model)

#------------------------------------------------逻辑回归-----------------------------------------------------------------
# 选择建模所需的字段
model_data <- data %>%
  select(casualty_severity, age_of_casualty, sex_of_casualty, pedestrian_location, pedestrian_movement, car_passenger)

# 将分类变量转化为哑变量（虚拟变量）
model_data <- model_data %>%
  mutate(sex_of_casualty = factor(sex_of_casualty),
         pedestrian_location = factor(pedestrian_location),
         pedestrian_movement = factor(pedestrian_movement),
         car_passenger = factor(car_passenger))

# 拆分数据集为训练集和测试集
set.seed(123)  # 设置随机种子以保证结果可重复
train_index <- sample(1:nrow(model_data), 0.7*nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# 建立逻辑回归模型
logistic_model <- glm(casualty_severity ~ ., data = train_data, family = "binomial")

# 查看模型摘要
summary(logistic_model)

# 预测测试集
predicted_probabilities <- predict(logistic_model, newdata = test_data, type = "response")

# 将概率转化为分类
predicted_classes <- ifelse(predicted_probabilities > 0.5, "Slight", "Serious/Fatal")

# 评估模型性能
confusion_matrix <- table(test_data$casualty_severity, predicted_classes)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# 输出混淆矩阵和准确率
confusion_matrix
accuracy


#------------------------随机森林------------------------------------------
# 安装并加载必要的包
#install.packages("randomForest")
#library(randomForest)

# 假设你的数据框叫做 "data"
# 假设你的目标变量是 "casualty_severity"，并将其转换成因子型变量
#data$casualty_severity <- factor(data$casualty_severity)

# 划分训练集和测试集
#set.seed(123)  # 设置随机种子以保证结果的可重复性
#sample_index <- sample(1:nrow(data), 0.7 * nrow(data))
#train_data <- data[sample_index, ]
#test_data <- data[-sample_index, ]

# 训练随机森林模型

#rf_model <- randomForest(casualty_severity ~ ., data = train_data, ntree = 30) # 或者其他更小的数值

# 预测
#predicted <- predict(rf_model, test_data)

# 评估模型性能
#confusion_matrix <- table(predicted, test_data$casualty_severity)
#accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)

# 打印混淆矩阵和准确率
#print(confusion_matrix)
#print(accuracy)



#-----------------随机森林 -------------------------------------
library(randomForest)
library(caret)
library(pROC)

features <- c('accident_year', 'casualty_class', 'sex_of_casualty', 'age_band_of_casualty', 'pedestrian_location', 'pedestrian_movement', 'car_passenger', 'bus_or_coach_passenger', 'casualty_type')
target <- 'casualty_severity'


# 数据拆分为训练集和测试集
set.seed(42)
train_index <- sample(1:nrow(data), 0.9*nrow(data))

data_train <- data[train_index, ]
data_test <- data[-train_index, ]


X_train = data_train[features]
Y_train = data_train[[target]]

X_test = data_test[features]
Y_test = data_test[[target]]


# 创建和训练随机森林模型
rf_model <- randomForest(x = X_train, y = factor(Y_train), ntree=30, importance = TRUE)


# 预测
y_pred <- predict(rf_model, X_test)


# 评估模型性能
accuracy <- sum(y_pred == Y_test) / length(Y_test)
print(paste("Accuracy:", accuracy))

# 计算混淆矩阵
y_pred <- factor(y_pred)
Y_test <- factor(Y_test)
levels(y_pred) <- levels(Y_test)
conf_matrix <- confusionMatrix(y_pred, Y_test)

print(conf_matrix)

TP <- conf_matrix$table[2,2]
FP <- conf_matrix$table[1,2]


TN <- conf_matrix$table[1,1]
FN <- conf_matrix$table[2,1]

precision <- TP / (TP + FP)

# 计算 Recall
recall <- TP / (TP + FN)

# 计算 F1 Score
f1_score <- 2 * (precision * recall) / (precision + recall)

# 输出结果
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))





# 计算 Precision、Recall 和 F1 Score
precision <- conf_matrix$byClass['Pos Pred Value']
recall <- conf_matrix$byClass['Sensitivity']
f1_score <- 2 * (precision * recall) / (precision + recall)

# 计算 ROC 曲线和 AUC
roc_curve <- roc(y_test, y_pred_prob[,2])
auc_score <- auc(roc_curve)

# 计算回归指标
mse <- mean((y_pred - y_test)^2)
rmse <- sqrt(mse)

# 提取特征重要性
feature_importance <- importance(rf_model)

# 输出结果
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))
print(paste("AUC:", auc_score))
print(paste("MSE:", mse))
print(paste("RMSE:", rmse))
print("Feature Importance:")
print(feature_importance)
