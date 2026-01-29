# 03_regression_analysis.R
# 负责执行 DiD 回归分析并输出结果 (Output to regression/ folder and as images)

library(stargazer)
library(lmtest)
library(sandwich) 
library(dplyr)

# 确保输出目录
if (!dir.exists("output/regression")) dir.create("output/regression", recursive = TRUE)

# 加载数据
if (!file.exists("data/clean_data.rds")) {
  stop("无法找到数据文件: data/clean_data.rds")
}
df <- readRDS("data/clean_data.rds")

# Helper to save Stargazer as Image (Text -> PNG)
save_stargazer_png <- function(models, title, filename_base, type="text") {
  # 1. Save Text
  txt_file <- paste0("output/regression/", filename_base, ".txt")
  capture.output(
    stargazer(models, type = type, title = title),
    file = txt_file
  )
  
  # 2. Convert to Image (Simple Text Plot)
  # 读取刚才保存的文本
  lines <- readLines(txt_file)
  
  png_file <- paste0("output/regression/", filename_base, ".png")
  
  # 根据行数动态调整高度
  n_lines <- length(lines)
  img_height <- max(600, n_lines * 20)
  img_width <- 1000
  
  png(png_file, width = img_width, height = img_height, res = 100)
  par(mar = c(1, 1, 1, 1))
  plot(c(0, 1), c(0, 1), ann = FALSE, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  
  # 在中心绘制文本 (使用等宽字体)
  text(x = 0.5, y = 0.5, 
       paste(lines, collapse = "\n"), 
       cex = 0.8, family = "mono", adj = c(0.5, 0.5))
  
  dev.off()
}

# --- Regression Sets ---

# Set 1: Basic DiD
ndidreg1 <- lm(df$`New_cases/100` ~ df$after * df$mandatory)
ndidreg2 <- lm(df$`Cumulative_cases/100` ~ df$after * df$mandatory)
ndidreg3 <- lm(df$`New_deaths/100` ~ df$after * df$mandatory)
ndidreg4 <- lm(df$`Cumulative_deaths/100` ~ df$after * df$mandatory)

save_stargazer_png(list(ndidreg1, ndidreg2, ndidreg3, ndidreg4), 
                   "Basic DiD Results", "basic_did")

# Set 2: No Control Group Excluded
df1 <- subset(df, nocontrol != 1)
ndidreg5 <- lm(df1$`New_cases/100` ~ df1$after * df1$mandatory)
ndidreg6 <- lm(df1$`Cumulative_cases/100` ~ df1$after * df1$mandatory)
ndidreg7 <- lm(df1$`New_deaths/100` ~ df1$after * df1$mandatory)
ndidreg8 <- lm(df1$`Cumulative_deaths/100` ~ df1$after * df1$mandatory)

save_stargazer_png(list(ndidreg5, ndidreg6, ndidreg7, ndidreg8), 
                   "DiD (Exclude No Control)", "no_control_did")

# Set 3: Optional Policy Analysis
df2 <- subset(df, mandatory != 1)
ndidreg9 <- lm(df2$`New_cases/100` ~ df2$after * df2$optional)
ndidreg10 <- lm(df2$`Cumulative_cases/100` ~ df2$after * df2$optional)
ndidreg11 <- lm(df2$`New_deaths/100` ~ df2$after * df2$optional)
ndidreg12 <- lm(df2$`Cumulative_deaths/100` ~ df2$after * df2$optional)

save_stargazer_png(list(ndidreg9, ndidreg10, ndidreg11, ndidreg12), 
                   "Optional Policy DiD", "optional_policy_did")

# Set 4: Country Specifc
didregn1 <- lm(df$`New_cases/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
didregn2 <- lm(df$`Cumulative_cases/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
didregn3 <- lm(df$`New_deaths/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)
didregn4 <- lm(df$`Cumulative_deaths/100` ~ df$China + df$India + df$Canada + df$UK + df$US + df$Japan + I(df$after*df$mandatory) + I(df$after*df$optional) + df$after)

save_stargazer_png(list(didregn1, didregn2, didregn3, didregn4), 
                   "DiD with Country FE", "country_fe_did")

message("回归结果已保存至 output/regression/ (含 PNG 图片)")
