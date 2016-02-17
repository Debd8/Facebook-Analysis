# Libraries used for this analysis
library(Rfacebook)
library(Rook)
library(igraph)
library(ggplot2)
library(wesanderson)
library(scales)

# Data is read from Amazon India facebook page and saved for local use here:
if (!file.exists("C:/Users/debdutta/Documents/R/AmazonIN.csv")) 
{
  page <- getPage("AmazonIN", token, n = 5000)
  write.csv(page, "C:/Users/debdutta/Documents/R/AmazonIN.csv")
}
page <- read.csv("C:/Users/debdutta/Documents/R/AmazonIN.csv")

# Plotting page popularity over time
## Convert Facebook date format to R date format
format.facebook.date <- function(datestring) 
{
  date <- as.POSIXct(datestring, format = "%Y-%m-%dT%H:%M:%S+0000", tz = "GMT")
}

## Aggregate metric counts over month
aggregate.metric <- function(metric) 
{
  m <- aggregate(page[[paste0(metric, "_count")]], list(month = page$month), mean)
  m$month <- as.Date(paste0(m$month, "-15"))
  m$metric <- metric
  return(m)
}

## Create data frame with average metric counts per month
page$datetime <- format.facebook.date(page$created_time)
page$month <- format(page$datetime, "%Y-%m")
df.list <- lapply(c("likes", "comments", "shares"), aggregate.metric)
df <- do.call(rbind, df.list)

## Plotting average actions per post
library(ggplot2)
ggplot(df, aes(x = month, y = x, group = metric)) + geom_line(aes(color = metric),
size = 2) + scale_color_manual(values = wes.palette(3, "Zissou")) + 
scale_x_date(breaks = "months", labels = date_format("%Y-%m")) + theme_bw() +
theme(axis.title.x = element_blank()) +
labs(y = "average number of actions per post",
title = "Average number of actions per post for \n Amazon India Facebook page")





