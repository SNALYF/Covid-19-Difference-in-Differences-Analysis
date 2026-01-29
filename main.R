# main.R
# R 项目管道主入口脚本

# 设置工作目录为项目根目录 (如果从 RStudio 打开项目，通常不需要这一步，但为了安全起见)
# setwd("...") 

# 检查必要的包
required_packages <- c("readr", "dplyr", "ggplot2", "cowplot", "stargazer", "lmtest", "sandwich", "zoo", "car")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, repos = "https://cloud.r-project.org")

message("==========================================")
message("开始执行数据分析管道")
message("==========================================")

# 1. 数据加载与清洗
message("\n[Step 1/3] 运行数据加载与清洗...")
tryCatch({
  source("scripts/01_load_clean.R")
  message(">> Step 1 完成。")
}, error = function(e) {
  message(">> Step 1 失败: ", e$message)
  stop("无法继续: 数据加载失败")
})

# 2. 描述性分析
message("\n[Step 2/3] 运行描述性分析...")
tryCatch({
  source("scripts/02_descriptive_analysis.R")
  message(">> Step 2 完成。")
}, error = function(e) {
  message(">> Step 2 失败 (可能是绘图包问题): ", e$message)
  message(">> 继续执行后续步骤...")
})

# 3. 回归分析
message("\n[Step 3/3] 运行回归分析...")
tryCatch({
  source("scripts/03_regression_analysis.R")
  message(">> Step 3 完成。")
}, error = function(e) {
  message(">> Step 3 失败: ", e$message)
})

message("\n==========================================")
message("管道执行完成！")
message("请检查 output/ 目录下的结果")
message("==========================================")
