# 01_load_clean.R
# 负责读取数据，清洗并准备分析用的变量

# 1. 加载包
library(readr)
library(dplyr)
library(zoo)

# 2. 读取数据
# 使用相对路径读取数据
csv_path <- "data/WHO-COVID-19-global-data_selected.csv"

if (!file.exists(csv_path)) {
  stop("无法找到数据文件: ", csv_path)
}

df <- read_csv(csv_path)

# 3. 数据清洗与预处理
# 转换日期格式
df$Date_reported <- as.Date(df$Date_reported)

# 确保数值型
df$New_cases <- as.numeric(df$New_cases)
options(scipen = 1000)

# 创建 Policy Group 变量 (用于绘图和分析)
# 逻辑：mandatory=1 -> 1, optional=1 -> 2, nocontrol=1 -> 3, else -> 0
df$policygroup <- ifelse(df$mandatory == 1, 1, 
                         ifelse(df$optional == 1, 2, 
                                ifelse(df$nocontrol == 1, 3, 0)))

# 将 policygroup 转换为因子，方便绘图图例
df$policygroup <- factor(df$policygroup, 
                         levels = c(0, 1, 2, 3),
                         labels = c("Other", "Mandatory", "Optional", "No Control"))

# 4. 保存清洗后的数据
# 使用 .rds 格式保存，保留 R 对象的所有属性（如因子水平、日期格式）
saveRDS(df, "data/clean_data.rds")

message("数据清洗完成，已保存至 data/clean_data.rds")
