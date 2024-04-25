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
GPU = read.csv("./gpus.csv", header = TRUE, na.strings = c("N/A"))
GPUAnalyze = GPU[,c("Architecture","Core_Speed","DVI_Connection"
                     ,"Dedicated","Direct_X","HDMI_Connection"
                     ,"Integrated","L2_Cache","Manufacturer"
                     ,"Max_Power","Memory","Memory_Bandwidth"
                     ,"Memory_Bus","Memory_Speed","Memory_Type"
                     ,"Name","Notebook_GPU","Open_GL","PSU"
                     ,"Pixel_Rate","Power_Connector","Process"
                     ,"ROPs","Release_Date","Resolution_WxH"
                     ,"SLI_Crossfire","Shader","TMUs","Texture_Rate"
                     ,"VGA_Connection")]
# Print raw summary datas
# print(summary(GPUAnalyze))

########### Tiền xử lý dữ liệu ###########
# Filtering variables
na <- sapply(GPUAnalyze, function(x) sum(is.na(x)) / length(x))
filterColumns <- names(na[na <= 0.25])

# Create new data
GPUFilter <- GPUAnalyze[, filterColumns]
# print(summary(GPUFilter))

#### No need to adjust column

# Memory_Type

### Column needs to fix
# Open_GL
GPUFilter$Open_GL <- NULL

# Power_Connector
GPUFilter$Power_Connector <- NULL

# Name
GPUFilter$Name <- NULL

# Architecture
GPUFilter$Architecture <- NULL

# VGA_Connection
GPUFilter$VGA_Connection <- NULL

# DVI_Connection
GPUFilter$DVI_Connection <- NULL

# HDMI_Connection
GPUFilter$HDMI_Connection <- NULL

# Manufacturer
GPUFilter$Manufacturer[is.na(GPUFilter$Manufacturer)] = median(GPUFilter$Manufacturer, na.rm = T)

# Core_Speed
GPUFilter$Core_Speed <- as.double(gsub(" MHz", "", GPUFilter$Core_Speed))
# Replace NA with median value
GPUFilter$Core_Speed[is.na(GPUFilter$Core_Speed)] = median(GPUFilter$Core_Speed, na.rm = T)

# Dedicated
GPUFilter$Dedicated <- ifelse(GPUFilter$Dedicated == 'Yes', 1, 0)

# Integrated
GPUFilter$Integrated <- ifelse(GPUFilter$Integrated == 'Yes', 1, 0)

# L2_Cache
GPUFilter$L2_Cache <- as.double(gsub("KB", "", GPUFilter$L2_Cache))
# Replace NA with median value
GPUFilter$L2_Cache[is.na(GPUFilter$L2_Cache)] = median(GPUFilter$L2_Cache, na.rm = T)

# Direct_X
GPUFilter$Direct_X <- NULL

# Max_Power
GPUFilter$Max_Power <- as.double(gsub(" Watts", "", GPUFilter$Max_Power))
# Replace NA with median value
GPUFilter$Max_Power[is.na(GPUFilter$Max_Power)] = median(GPUFilter$Max_Power, na.rm = T)

# Memory
GPUFilter$Memory <- as.double(gsub(" MB", "", GPUFilter$Memory))
# Replace NA with median value
GPUFilter$Memory[is.na(GPUFilter$Memory)] = median(GPUFilter$Memory, na.rm = T)

# Memory_Bandwith
GPUFilter$Memory_Bandwidth <- as.double(gsub("GB/sec", "", GPUFilter$Memory_Bandwidth))
# Replace NA with median value
GPUFilter$Memory_Bandwidth[is.na(GPUFilter$Memory_Bandwidth)] = median(GPUFilter$Memory_Bandwidth, na.rm = T)

# Memory_Bus
GPUFilter$Memory_Bus <- as.double(gsub(" Bit", "", GPUFilter$Memory_Bus))
# Replace NA with median value
GPUFilter$Memory_Bus[is.na(GPUFilter$Memory_Bus)] = median(GPUFilter$Memory_Bus, na.rm = T)

# Memory_Speed
GPUFilter$Memory_Speed <- as.double(gsub(" MHz", "", GPUFilter$Memory_Speed))
# Replace NA with median value
GPUFilter$Memory_Speed[is.na(GPUFilter$Memory_Speed)] = median(GPUFilter$Memory_Speed, na.rm = T)

# Notebook_GPU
GPUFilter$Notebook_GPU <- ifelse(GPUFilter$Notebook_GPU == 'Yes', 1, 0)

# Pixel_Rate
GPUFilter$Pixel_Rate <- as.double(gsub(" GPixel/s", "", GPUFilter$Pixel_Rate))
# Replace NA with median value
GPUFilter$Pixel_Rate[is.na(GPUFilter$Pixel_Rate)] = median(GPUFilter$Pixel_Rate, na.rm = T)

# Process
GPUFilter$Process <- as.double(gsub("nm", "", GPUFilter$Process))
# Replace NA with median value
GPUFilter$Process[is.na(GPUFilter$Process)] = median(GPUFilter$Process, na.rm = T)

# Release_Date
GPUFilter$Release_Date <- NULL

# Resolution_WxH
# Replace NA with mode value (4096x2160 by observation)
GPUFilter$Resolution_WxH[is.na(GPUFilter$Resolution_WxH)] = "4096x2160"
print(GPUFilter$Resolution_WxH)

# SLI_Crossfire
GPUFilter$SLI_Crossfire <- ifelse(GPUFilter$SLI_Crossfire == 'Yes', 1, 0)

# Texture_Rate
GPUFilter$Texture_Rate <- as.double(gsub(" GTexel/s", "", GPUFilter$Texture_Rate))
# Replace NA with median value
GPUFilter$Texture_Rate[is.na(GPUFilter$Texture_Rate)] = median(GPUFilter$Texture_Rate, na.rm = T)

# ROPs
# Replace NA with median value
GPUFilter$ROPs[is.na(GPUFilter$ROPs)] = median(GPUFilter$ROPs, na.rm = T)
GPUFilter$ROPs <- NULL

# Shader
# Replace NA with median value
GPUFilter$Shader[is.na(GPUFilter$Shader)] = median(GPUFilter$Shader, na.rm = T)

# TMUs
# Replace NA with median value
GPUFilter$TMUs[is.na(GPUFilter$TMUs)] = median(GPUFilter$TMUs, na.rm = T)

# print(summary(GPUFilter))
numericalCol = c("Core_Speed",
                 "L2_Cache",
                 "Max_Power",
                 "Memory",
                 "Memory_Bandwidth",
                 "Memory_Bus",
                 "Memory_Speed",
                 "Pixel_Rate",
                 "Process",
                 "Shader",
                 "TMUs",
                 "Texture_Rate")
nameCol = c("Manufacturer",
            "Memory_Type",
            "Resolution_WxH")
boolCol = c("Dedicated",
            "Integrated",
            "Notebook_GPU",
            "SLI_Crossfire")

########### Thống kê tả ###########
# Thống kê dữ liệu số
numCount <- length(GPUFilter[[1]])
numMean <- apply(GPUFilter[,numericalCol], 2, mean)
numMedian <- apply(GPUFilter[,numericalCol], 2, median)
numSd <- apply(GPUFilter[,numericalCol], 2, sd)
numMin <- apply(GPUFilter[,numericalCol], 2, min)
numMax <- apply(GPUFilter[,numericalCol], 2, max)

summaryNumeric <- data.frame(numCount, numMean, numMedian, numSd, numMin, numMax)
colnames(summaryNumeric) <- c("Count", "Mean", "Median", "Sd", "Min", "Max")
print(summaryNumeric)

par(mfrow=c(2,2))
for (i in 1:length(numericalCol)) {
  histData <- GPUFilter[[numericalCol[i]]]
  hist(histData,
       main = paste("Histogram of", names(GPUFilter)[which(names(GPUFilter)==numericalCol[i])]),
       labels = TRUE,
       col = "darkgreen",
       xlab = names(GPUFilter)[which(names(GPUFilter)==numericalCol[i])],
       breaks = 10)
}

# Hệ số tương quan
corTable <- cor(GPUFilter[numericalCol])

# Bảng thống kê cặp của những biến có chỉ số tương quan cao
# Max_Power - Memory_Bandwith
ggplot(data = GPUFilter, aes(x = Max_Power, y = Memory_Bandwidth)) +
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", color = 'red', size = 2) +
  labs(title = "Max Power - Memory Bandwidth",
       x = "Max_Power",
       y = "Memory Bandwdith")

# Texture_Rate - Memory_Bandwith
ggplot(data = GPUFilter, aes(x = Texture_Rate, y = Memory_Bandwidth)) +
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", color = 'red', size = 2) +
  labs(title = "Texture Rate - Memory Bandwidth",
       x = "Texture Rtae",
       y = "Memory Bandwdith")

# Pixel_Rate - Memory_Bandwith
ggplot(data = GPUFilter, aes(x = Pixel_Rate, y = Memory_Bandwidth)) +
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", color = 'red', size = 2) +
  labs(title = "Pixel Rate - Memory Bandwidth",
       x = "Pixel Rate",
       y = "Memory Bandwdith")

# Memory - Memory_Bandwith
ggplot(data = GPUFilter, aes(x = Memory, y = Memory_Bandwidth)) +
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", color = 'red', size = 2) +
  labs(title = "Memory - Memory Bandwidth",
       x = "Memory",
       y = "Memory Bandwdith")

# TMUs - Memory_Bandwith
ggplot(data = GPUFilter, aes(x = TMUs, y = Memory_Bandwidth)) +
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", color = 'red', size = 2) +
  labs(title = "TMUs - Memory Bandwidth",
       x = "TMUs",
       y = "Memory Bandwdith")

# TMUs - Memory_Bus
ggplot(data = GPUFilter, aes(x = TMUs, y = Memory_Bus)) +
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", color = 'red', size = 2) +
  labs(title = "TMUs - Memory Bus",
       x = "TMUs",
       y = "Memory Bus")

# TMUs - Texture_Rate
ggplot(data = GPUFilter, aes(x = TMUs, y = Texture_Rate)) +
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", color = 'red', size = 2) +
  labs(title = "TMUs - Texture Rate",
       x = "TMUs",
       y = "Texture Rate")

# Pixel_Rate - Texture_Rate
ggplot(data = GPUFilter, aes(x = Pixel_Rate, y = Texture_Rate)) +
  geom_point(alpha = 1/20) +
  geom_smooth(method = "lm", color = 'red', size = 2) +
  labs(title = "Pixel Rate - Texture Rate",
       x = "Pixel Rate",
       y = "Texture Rate")

########### Thống kê suy diễn ###########
# Mô hình hồi quy tuyến tính
# Core Speed ~ Memory_Speed + Memory + Memory_Bandwidth + Memory_Bus + Texture_Rate + Pixel_Rate + TMUs
logCoreSpd <- log(GPUFilter$Core_Speed)
logMemSpd <- log(GPUFilter$Memory_Speed)
logMem <- log(GPUFilter$Memory)
logMemBan <- log(GPUFilter$Memory_Bandwidth)
logMemBus <- log(GPUFilter$Memory_Bus)
logTxRa <- log(GPUFilter$Texture_Rate)
logPxRa <- log(GPUFilter$Pixel_Rate)
logTMUs <- log(GPUFilter$TMUs)

logDf <- data.frame(logCoreSpd, logMemSpd, logMem, logMemBan, logMemBus, logTxRa, logPxRa, logTMUs)
colnames(logDf) <- c("Core_Speed"
                     , "Memory_Speed"
                     , "Memory"
                     , "Memory_Bandwidth"
                     , "Memory_Bus"
                     , "Texture_Rate"
                     , "Pixel_Rate"
                     , "TMUs")

# All in
lmModel_1 <- lm(Core_Speed ~ Memory_Speed + Memory + Memory_Bandwidth + Memory_Bus 
                + Texture_Rate + Pixel_Rate + TMUs, data = logDf)
summary(lmModel_1)

# Bỏ Memory_Bandwidth
lmModel_2 <- lm(Core_Speed ~ Memory_Speed + Memory + Memory_Bus
                + Texture_Rate + Pixel_Rate + TMUs, data = logDf)
summary(lmModel_2)

anova(lmModel_1, lmModel_2)

# Đồ thị sai số hồi quy
par(mfrow=c(2,2))
plot(lmModel_2,)

# Dự đoán
observerFrame <- data.frame(logDf[, c("Core_Speed", "Memory_Speed", "Memory"
                                      , "Memory_Bandwidth", "Memory_Bus"
                                      , "Texture_Rate", "Pixel_Rate", "TMUs")])

prediction <- predict(lmModel_1, interval = "confidence")
print(colMeans(prediction)) # Log(Core Speed)
print(exp(colMeans(prediction))) # Dự đoán trên mean của bộ dữ liệu chuyển từ log

predictedCoreSpd <- predict(lmModel_1)
predictData <- data.frame(Core_Speed = logDf$Core_Speed, prediction = predictedCoreSpd)

ggplot(predictData, aes(x = Core_Speed, y = prediction)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue") +
  labs(x = "Core Speed", y = "Predicted Core Speed", title = "Prediction of Core Speed")

MAE <- mean(abs(predictedCoreSpd -logDf$Core_Speed))
MSE <- mean((predictedCoreSpd - logDf$Core_Speed) ^2)

SSR <- sum((predictedCoreSpd - mean(logDf$Core_Speed)) ^2)
SST <- sum((logDf$Core_Speed - mean(logDf$Core_Speed)) ^2)
RSquared <- SSR/SST

print(paste("Sai số tuyệt đối trung bình:", MAE))
print(paste("Sai số toàn phương trung bình:", MSE))
print(paste("R Bình Phương:", RSquared))
