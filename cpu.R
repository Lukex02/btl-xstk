# Install package for first run
# install.packages("stringr")
# install.packages("tidyr")
# install.packages("dplyr")
# install.packages("zoo")
# install.packages("Metrics")
# install.packages("caret")
# install.packages("MASS")
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("mltools")
# install.packages("DescTools")
# install.packages("plotly")

# Load packages
library(stringr)
library(tidyr)
library(dplyr)
library(zoo)
library(Metrics)
library(caret)
library(MASS)
library(ggplot2)
library(reshape2)
library(mltools)
library(DescTools)
library(plotly)

# Read datas
CPU = read.csv("./cpus.csv", header = TRUE, na.strings = c("N/A", ""))
CPUAnalyze = CPU[,c("Product_Collection", "Vertical_Segment"
                     , "Processor_Number", "Status", "Launch_Date"
                     , "Lithography", "Recommended_Customer_Price"
                     , "nb_of_Cores", "nb_of_Threads"
                     , "Processor_Base_Frequency", "Cache"
                     , "Bus_Speed", "TDP", "Embedded_Options_Available"
                     , "Max_Memory_Size", "Memory_Types"
                     , "Max_nb_of_Memory_Channels", "T"
                     , "Intel_Hyper_Threading_Technology_"
                     , "Intel_Virtualization_Technology_VTx_"
                     , "Intel_64_", "Instruction_Set"
                     , "Idle_States", "Thermal_Monitoring_Technologies"
                     , "Execute_Disable_Bit")]
# Print summary datas
# print(summary(CPUAnalyze))

########### Tiền xử lý dữ liệu ###########
# Filtering variables
na <- sapply(CPUAnalyze, function(x) sum(is.na(x)) / length(x))
filterColumns <- names(na[na <= 0.25])

# Create new data
CPUFilter <- CPUAnalyze[, filterColumns]
#print(summary(CPUFilter))

### Non handling data
# Vertical_Segment
# Status

### Data handling
# Product_Collection
# CPUFilter$Processor_Collection <- NULL

# Processor_Number
CPUFilter$Processor_Number <- NULL

# Launch_Date
CPUFilter$Launch_Date <- NULL

# Lithography
CPUFilter$Lithography <- as.double(gsub(" nm", "", CPUFilter$Lithography))
# Replace NA with median value
CPUFilter$Lithography[is.na(CPUFilter$Lithography)] = median(CPUFilter$Lithography, na.rm = T)

# Recommended_Customer_Price
CPUFilter$Recommended_Customer_Price <- as.double(sub("\\$", "", CPUFilter$Recommended_Customer_Price))
CPUFilter$Recommended_Customer_Price <- as.double(CPUFilter$Recommended_Customer_Price)
# Replace NA with median value
CPUFilter$Recommended_Customer_Price[is.na(CPUFilter$Recommended_Customer_Price)] = median(CPUFilter$Recommended_Customer_Price, na.rm = T)

# nb_of_Cores
CPUFilter$nb_of_Cores[is.na(CPUFilter$nb_of_Cores)] = median(CPUFilter$nb_of_Cores, na.rm = T)

# nb_of_Threads
CPUFilter$nb_of_Threads[is.na(CPUFilter$nb_of_Threads)] = median(CPUFilter$nb_of_Threads, na.rm = T)

# Processor_Base_Frequency
CPUFilter$Processor_Base_Frequency <- as.double(gsub(" GHz", "", CPUFilter$Processor_Base_Frequency))
# Replace NA with median value
CPUFilter$Processor_Base_Frequency[is.na(CPUFilter$Processor_Base_Frequency)] = median(CPUFilter$Processor_Base_Frequency, na.rm = T)

# Cache
CPUFilter$Cache <- as.double(gsub(" MB .*", "", CPUFilter$Cache))
# Replace NA with median value
CPUFilter$Cache[is.na(CPUFilter$Cache)] = median(CPUFilter$Cache, na.rm = T)
#print(CPUFilter$Cache)

# Bus_Speed
CPUFilter$Bus_Speed <- NULL

# TDP
CPUFilter$TDP <- as.double(gsub(" W", "", CPUFilter$TDP))
# Replace NA with median value
CPUFilter$TDP[is.na(CPUFilter$TDP)] = median(CPUFilter$TDP, na.rm = T)
#print(CPUFilter$Cache)

# Embedded_Options_Available
CPUFilter$Embedded_Options_Available <- NULL

# Conflict_Free
CPUFilter$Conflict_Free <- NULL

# T
CPUFilter$T <- as.double(gsub(" Â°C", "", CPUFilter$T))
# Replace NA with median value
CPUFilter$T[is.na(CPUFilter$T)] = median(CPUFilter$T, na.rm = T)
#print(CPUFilter$Cache)

# Intel_Hyper_Threading_Technology_
# CPUFilter$Intel_Hyper_Threading_Technology_ <- ifelse(CPUFilter$Intel_Hyper_Threading_Technology_ == 'Yes', 1, 0)

# Intel_Virtualization_Technology_VTx_
# CPUFilter$Intel_Virtualization_Technology_VTx_ <- ifelse(CPUFilter$Intel_Virtualization_Technology_VTx_ == 'Yes', 1, 0)

# Intel_64_
# CPUFilter$Intel_64_ <- ifelse(CPUFilter$Intel_64_ == 'Yes', 1, 0)

# Instruction_Set
CPUFilter$Instruction_Set <- NULL

# Idle_States
# CPUFilter$Idle_States <- ifelse(CPUFilter$Idle_States == 'Yes', 1, 0)

# Execute_Disable_Bit
# CPUFilter$Execute_Disable_Bit <- ifelse(CPUFilter$Execute_Disable_Bit == 'Yes', 1, 0)

numericalCol = c("Lithography",
                 "Recommended_Customer_Price",
                 "nb_of_Cores",
                 "nb_of_Threads",
                 "Processor_Base_Frequency",
                 "Cache",
                 "TDP",
                 "T"
                 )
nameCol = c("Vertical_Segment",
            "Product_Collection")
boolCol = c("Intel_Hyper_Threading_Technology_",
            "Intel_Virtualization_Technology_VTx_",
            "Intel_64_",
            "Idle_States",
            "Execute_Disable_Bit")
########### Thống kê tả ###########
# Thống kê của dữ liệu số
numCount <- length(CPUFilter[[1]])
numMean <- apply(CPUFilter[,numericalCol], 2, mean)
numMedian <- apply(CPUFilter[,numericalCol], 2, median)
numSd <- apply(CPUFilter[,numericalCol], 2, sd)
numMin <- apply(CPUFilter[,numericalCol], 2, min)
numMax <- apply(CPUFilter[,numericalCol], 2, max)

summaryNumeric <- data.frame(numCount, numMean, numMedian, numSd, numMin, numMax)
colnames(summaryNumeric) <- c("Count", "Mean", "Median", "Sd", "Min", "Max")
print(summaryNumeric)

for (i in 1:length(numericalCol)) {
  histData <- CPUFilter[[numericalCol[i]]]
  hist(histData,
       main = paste("Histogram of", names(CPUFilter)[which(names(SCPUFilter)==numericalCol[i])]),
       labels = TRUE,
       col = "darkmagenta",
       xlab = names(CPUFilter)[which(names(CPUFilter)==numericalCol[i])],
       breaks = 10)
}
# Thống kê dữ liệu tên/mã hiệu
for (i in 1:length(nameCol)) {
  barData <- table(CPUFilter[[nameCol[i]]])
  barplot(barData,
          xlab = names(CPUFilter)[which(names(CPUFilter) == nameCol[i])],
          ylab = "Frequency",
          main = paste("Barplot of", names(CPUFilter)[which(names(CPUFilter)==nameCol[i])]),
          )
}
# Thống kê dữ liệu bool
for (i in 1:length(boolCol)) {
  pieData <- table(CPUFilter[[boolCol[i]]])
  # pieData <- c(pieData[[1]], pieData[[2]])
  # print(c(pieData[[1]], pieData[[2]]))
  pie(pieData, col = c("red", "green"),
      labels = names(pieData),
      radius = -1,
      main = boolCol[i],
      col.main = "black")
}
# Hệ số tương quan
corTable <- cor(CPUFilter[numericalCol])

# Thống kê các cặp
# Cores - Threads
ggplot(data = CPUFilter, aes(x = nb_of_Cores, y = nb_of_Threads)) +
  geom_point() +
  geom_smooth(method = "gam") +
  labs(title = "Number of Cores - Threads",
       x = "number of Cores",
       y = "number of Threads")
# Quan sát đồ thị ta có thể thấy 1 core ~ 2 thread (tức dual core) chiếm đa số
# Phần nhỏ ỏ phía sau là do số nhân của dòng Server (>~60 cores) không có số Threads tương ứng được thay thế bằng median của Threads

# TDP - Cores
ggplot(data = CPUFilter, aes(x = nb_of_Cores, y = TDP)) +
  geom_violin() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between number of Cores - TDP",
       x = "number of Cores",
       y = "TDP")

# TDP - Threads
ggplot(data = CPUFilter, aes(x = nb_of_Threads, y = TDP)) +
  geom_violin() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between number of Threads - TDP",
       x = "number of Threads",
       y = "TDP")

# Cache - Cores
ggplot(data = CPUFilter, aes(x = nb_of_Cores, y = Cache)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between number of Cores - Cache",
       x = "number of Cores",
       y = "Cache")

# Cache - Threads
ggplot(data = CPUFilter, aes(x = nb_of_Threads, y = Cache)) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between number of Cores - Cache",
       x = "number of Threads",
       y = "Cache")

# Processor Base Freq - TDP
ggplot(data = CPUFilter, aes(x = Processor_Base_Frequency, y = TDP)) +
  geom_area() +
  geom_smooth(method = "lm") +
  labs(title = "Relationship between number of Base Frequency - TDP",
       x = "Processor Base Frequency",
       y = "TDP")
##----Vertical_Segment ~ numericalCol----##
for (i in 1:length(numericalCol)) {
  print(ggplot(data = CPUFilter, aes(x = Vertical_Segment, y = CPUFilter[,numericalCol[i]])) +
    geom_boxplot(outlier.colour = "blue", outlier.shape=16, outlier.size=1) +
      labs(title = paste("Vertical Segment", numericalCol[i]),
        x = "Vertical Segment",
        y = numericalCol[i]))
}

########### Thống kê suy diễn ###########
# Anova one way nb_of_Cores ~ Vertical_Segment
anova <- aov(nb_of_Cores~ Vertical_Segment, data=CPUFilter)
summary(anova)
# TukeyHSD(anova)
par(mfrow=c(1,1))
plot(TukeyHSD(anova))

# Mô hình hồi quy tuyến tính
# Recommended_Customer_Price ~ Processor_Base_Frequency + Lithography + nb_of_Cores + nb_of_Threads + Cache + TDP + T
logPrice <- log(CPUFilter$Recommended_Customer_Price)
logBase <- log(CPUFilter$Processor_Base_Frequency)
logLith <- log(CPUFilter$Lithography)
lognbCor <- log(CPUFilter$nb_of_Cores)
lognbThr <- log(CPUFilter$nb_of_Threads)
logCache <- log(CPUFilter$Cache)
logTDP <- log(CPUFilter$TDP)
logT <- log(CPUFilter$T)
log
logDf <- data.frame(logPrice, logBase, logLith, lognbCor, lognbThr, logCache, logTDP, logT)
colnames(logDf) <- c("Recommended_Customer_Price"
                     , "Processor_Base_Frequency"
                     , "Lithography"
                     , "nb_of_Cores"
                     , "nb_of_Threads"
                     , "Cache"
                     , "TDP"
                     , "T")

# All in
lmModel_1 <- lm(Recommended_Customer_Price ~ Processor_Base_Frequency + Lithography + nb_of_Cores + nb_of_Threads + Cache + TDP + T, data = logDf)
summary(lmModel_1)
# Chưa thể bác bỏ H0 vì còn T

# Bỏ T
lmModel_2 <- lm(Recommended_Customer_Price ~ Processor_Base_Frequency + Lithography + nb_of_Cores + nb_of_Threads + Cache + TDP, data = logDf)
summary(lmModel_2)

# So sánh
# p <<< 0,5 => Bác bỏ H0 => Ưu tiên nhiều biến
# P > 0,5 => Ko bác bỏ H0 => Ưu tiêu ít biến

anova(lmModel_1, lmModel_2)
# Pr > 0.05 => Không thể bác bỏ H0 => Hai mô hình có ý nghĩa thống kê tương đương
# => Chọn mô hình 2 do ít biến hơn nhưng cùng ý nghĩa thống kê

# Đồ thị sai số hồi quy
par(mfrow=c(2,2))
plot(lmModel_2,)

# Dự đoán
observerFrame <- data.frame(logDf[, c("Recommended_Customer_Price"
                                      , "Processor_Base_Frequency"
                                      , "Lithography"
                                      , "nb_of_Cores"
                                      , "nb_of_Threads"
                                      , "Cache"
                                      , "TDP"
                                      , "T")])

prediction <- predict(lmModel_1, interval = "confidence")
print(colMeans(prediction)) # Log(Price)
print(exp(colMeans(prediction))) # Dự đoán trên mean của bộ dữ liệu chuyển từ log

predictedPrice <- predict(lmModel_1) # log(Price)
predictData <- data.frame(Recommended_Customer_Price = logDf$Recommended_Customer_Price, prediction = predictedPrice)

ggplot(predictData, aes(x = Recommended_Customer_Price, y = prediction)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "Recommended Customer Price", y = "Predicted Price", title = "Prediction of Recommended Customer Price")

MAE <- mean(abs(predictedPrice -logDf$Recommended_Customer_Price))
MSE <- mean((predictedPrice - logDf$Recommended_Customer_Price) ^2)

SSR <- sum((predictedPrice - mean(logDf$Recommended_Customer_Price)) ^2)
SST <- sum((logDf$Recommended_Customer_Price - mean(logDf$Recommended_Customer_Price)) ^2)
RSquared <- SSR/SST

print(paste("Sai số tuyệt đối trung bình:", MAE))
print(paste("Sai số toàn phương trung bình:", MSE))
print(paste("R Bình Phương:", RSquared))
