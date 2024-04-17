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

### No need to adjust column
# Architecture
# DVI_Connection
# HDMI_Connection
# Manufacturer
# Memory_Type
# Name
# Open_GL
# Power_Connector
# ROPs
# Shader
# TMUs
# VGA_Connection

### Column needs to fix

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
GPUFilter$Direct_X <- as.double(gsub("DX ", "", GPUFilter$Direct_X))
# Replace NA with median value
GPUFilter$Direct_X[is.na(GPUFilter$Direct_X)] = median(GPUFilter$Direct_X, na.rm = T)

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

print(summary(GPUFilter))