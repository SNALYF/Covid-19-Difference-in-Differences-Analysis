library(ggplot2)
library(dplyr) # 确保可用 sym

#' 绘制时间序列图
#'
#' @param data 数据框
#' @param y_var Y轴变量名 (字符串)
#' @param group_var 分组变量名 (字符串)
#' @param date_var 日期变量名 (字符串，默认为 Date_reported)
#' @param y_label Y轴标签
#' @param title 标题
#'
#' @return ggplot 对象
plot_time_series <- function(data, y_var, group_var = "Country", date_var = "Date_reported", 
                             y_label = NULL, title = NULL) {
  
  # 打印列名以进行调试
  # message("Debug plot_time_series: columns in data: ", paste(colnames(data), collapse=", "))
  
  p <- ggplot(data, aes(x = !!sym(date_var), 
                        y = !!sym(y_var), 
                        group = !!sym(group_var), 
                        color = !!sym(group_var))) + 
    geom_line() +
    scale_x_date(date_breaks = "months", date_labels = "%b") +
    theme_minimal()
  
  if (!is.null(y_label)) {
    p <- p + ylab(y_label)
  }
  
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }
  
  return(p)
}
