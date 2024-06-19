library(ggplot2)
library(reshape2)
library(readr)


df <- read_csv("/Users/alfred/Downloads/quartely index results.csv", show_col_types = FALSE)


names(df)[1] <- "YEARQ"


df$YEARQ <- as.Date(gsub("Q([1-4])", "-\\1-01", as.character(df$YEARQ)), format="%Y-%m-%d")


df <- df[, !grepl("^\\.\\.\\.", names(df))]


df <- df[df$YEARQ >= as.Date("2000-01-01"), ]


for (location in names(df)[-1]) { 
  
  df[[location]] <- as.numeric(as.character(df[[location]]))
  
  
  df_location <- data.frame(YEARQ = df$YEARQ, Value = df[[location]])
  
  
  df_location <- df_location[!is.na(df_location$Value), ]
  

  p <- ggplot(df_location, aes(x = YEARQ, y = Value)) +
    geom_point() + 
    theme_minimal() +
    labs(title = paste("Value Trend for", location, "Since 2000"),
         x = "Year",
         y = "Value") +
    scale_y_continuous(breaks = seq(100, max(df_location$Value, na.rm = TRUE), by = 10)) 
  
 
  print(p)
  

  ggsave(paste0(location, "_value_trend_scatterplot.png"), plot = p, width = 10, height = 6, dpi = 300)
}
