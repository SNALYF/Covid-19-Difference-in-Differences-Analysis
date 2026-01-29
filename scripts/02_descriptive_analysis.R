# 02_descriptive_analysis.R
# 负责生成描述性统计图表 (Beautified Base R Version)

library(dplyr)

# 加载数据
if (!file.exists("data/clean_data.rds")) {
  stop("无法找到数据文件: data/clean_data.rds")
}
df <- readRDS("data/clean_data.rds")

# 确保输出目录存在
if (!dir.exists("output/img")) dir.create("output/img", recursive = TRUE)

message("正在生成美化图表 (保存至 output/img/)...")

# --- 1. Spaghetti Plots (Beautified) ---
plot_spaghetti <- function(df, y_col, title, ylab) {
  countries <- unique(df$Country)
  # 使用更有质感的颜色 (如 viridis 风格或 RColorBrewer set1)
  # Base R fallback: rainbow is okay, but let's try slightly darker/richer
  colors <- rainbow(length(countries), s = 0.7, v = 0.8, alpha = 0.6) 
  
  x_range <- range(df$Date_reported, na.rm = TRUE)
  y_range <- range(df[[y_col]], na.rm = TRUE)
  
  if(any(is.infinite(y_range))) return()
  
  # 设置边距
  par(mar = c(5, 5, 4, 2) + 0.1, bg = "white") # 白色背景
  
  plot(NULL, xlim = x_range, ylim = y_range, 
       main = "", xlab = "", ylab = "", 
       axes = FALSE, frame.plot = FALSE) # 自定义坐标轴
  
  # 添加网格
  grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
  
  # 坐标轴
  axis(1, col = "gray50", col.axis = "gray30", cex.axis = 0.8, format = "%b") # 日期
  axis(2, col = "gray50", col.axis = "gray30", cex.axis = 0.8, las = 1) # Y轴水平文字
  
  # 标题与标签
  title(main = title, col.main = "#333333", font.main = 2)
  title(ylab = ylab, col.lab = "gray40", line = 3.5)
  title(xlab = "Date", col.lab = "gray40", line = 2)
  
  for (i in seq_along(countries)) {
    country_data <- df[df$Country == countries[i], ]
    country_data <- country_data[order(country_data$Date_reported), ]
    
    if (nrow(country_data) > 0) {
      lines(country_data$Date_reported, country_data[[y_col]], 
            col = colors[i], lwd = 1.8)
    }
  }
}

# 批量生成 Spaghetti Plots
png("output/img/daily_new_cases.png", width=1200, height=800, res=100)
plot_spaghetti(df, "New_cases", "Daily New Cases by Country", "New Cases")
dev.off()

png("output/img/daily_new_cases_per_100.png", width=1200, height=800, res=100)
plot_spaghetti(df, "New_cases/100", "Daily New Cases / 100 by Country", "New Cases / 100")
dev.off()

png("output/img/daily_new_deaths.png", width=1200, height=800, res=100)
plot_spaghetti(df, "New_deaths", "Daily New Deaths by Country", "New Deaths")
dev.off()

png("output/img/cumulative_cases.png", width=1200, height=800, res=100)
plot_spaghetti(df, "Cumulative_cases", "Cumulative Cases by Country", "Cumulative Cases")
dev.off()

png("output/img/cumulative_deaths.png", width=1200, height=800, res=100)
plot_spaghetti(df, "Cumulative_deaths", "Cumulative Deaths by Country", "Cumulative Deaths")
dev.off()


# --- 2. Policy Impact Graphs (Beautified) ---

plot_policy_smooth <- function(df, y_col, title, vline_red) {
  groups <- c("Mandatory", "Optional", "No Control") 
  # 美化配色
  color_map <- c("Mandatory" = "#E41A1C", "Optional" = "#377EB8", "No Control" = "#4DAF4A")
  
  x_range <- range(df$day, na.rm = TRUE)
  y_range <- range(df[[y_col]], na.rm = TRUE)
  
  par(mar = c(5, 5, 4, 2) + 0.1, bg = "white")
  
  plot(NULL, xlim = x_range, ylim = y_range, 
       main = "", xlab = "", ylab = "",
       axes = FALSE, frame.plot = FALSE)
       
  grid(col = "lightgray", lty = "dotted")
  
  axis(1, col = "gray50", col.axis = "gray30", cex.axis = 0.8)
  axis(2, col = "gray50", col.axis = "gray30", cex.axis = 0.8, las = 1)
  
  title(main = title, col.main = "#333333", font.main = 2)
  title(ylab = ylab_name(y_col), col.lab = "gray40", line = 3.5)
  title(xlab = "Days since outbreak", col.lab = "gray40", line = 2)
  
  # 垂直线
  abline(v = 0, lty = 2, col = "gray40", lwd=1.5)
  abline(v = vline_red, lty = 2, col = "#E41A1C", lwd=1.5)
  text(vline_red, y_range[2], "Policy Start", col="#E41A1C", pos=4, cex=0.8)
  
  # 循环绘制
  for (grp in groups) {
    sub_data <- df[df$policygroup == grp, ]
    sub_data <- sub_data[!is.na(sub_data[[y_col]]) & !is.na(sub_data$day), ]
    
    if (nrow(sub_data) > 10) {
      sub_data <- sub_data[order(sub_data$day), ]
      
      fit <- tryCatch({
        loess(formula(paste0("`", y_col, "` ~ day")), data = sub_data, span = 0.5)
      }, error = function(e) return(NULL))
      
      if (!is.null(fit)) {
        pred_x <- seq(min(sub_data$day), max(sub_data$day), length.out = 200)
        pred <- predict(fit, newdata = data.frame(day = pred_x), se = TRUE)
        
        y_fit <- pred$fit
        se_fit <- pred$se.fit
        ci_upper <- y_fit + 1.96 * se_fit
        ci_lower <- y_fit - 1.96 * se_fit
        
        col_base <- color_map[grp]
        col_fill <- adjustcolor(col_base, alpha.f = 0.2)
        
        valid_idx <- !is.na(y_fit)
        polygon(c(pred_x[valid_idx], rev(pred_x[valid_idx])), 
                c(ci_upper[valid_idx], rev(ci_lower[valid_idx])),
                col = col_fill, border = NA)
        lines(pred_x[valid_idx], y_fit[valid_idx], col = col_base, lwd = 3)
      }
    }
  }
  
  legend("topleft", legend = groups, col = color_map[groups], lwd = 3, bty = "n", cex=0.9, title="Policy Group")
}

ylab_name <- function(col) {
  if (col == "New_cases/100") return("New cases / 100")
  if (col == "Cumulative_cases/100") return("Cumulative cases / 100")
  if (col == "New_deaths/100") return("New deaths / 100")
  if (col == "Cumulative_deaths/100") return("Cumulative deaths / 100")
  return(col)
}

# 批量生成 Policy Graphs
png("output/img/policy_impact_cases.png", width=1200, height=800, res=100)
plot_policy_smooth(df, "New_cases/100", "Policy Impact: Daily Cases", 50)
dev.off()

png("output/img/policy_impact_cum_cases.png", width=1200, height=800, res=100)
plot_policy_smooth(df, "Cumulative_cases/100", "Policy Impact: Cumulative Cases", 50)
dev.off()

png("output/img/policy_impact_deaths.png", width=1200, height=800, res=100)
plot_policy_smooth(df, "New_deaths/100", "Policy Impact: Daily Deaths", 50)
dev.off()

png("output/img/policy_impact_cum_deaths.png", width=1200, height=800, res=100)
plot_policy_smooth(df, "Cumulative_deaths/100", "Policy Impact: Cumulative Deaths", 120)
dev.off()

message("所有美化图表已生成至 output/img/")
