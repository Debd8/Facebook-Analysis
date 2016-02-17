# Libraries used for this analysis
library(Rfacebook)
library(Rook)
library(igraph)
library(ggplot2)
library(wesanderson)
library(scales)

# Data is read from the websites' facebook page and saved for local use here
page_amzn <- getPage("AmazonIN", token, n = 1000)
page_fk <- getPage("flipkart", token, n = 1000)
page_myn <- getPage("myntra", token, n = 1000)
page_jab <- getPage("myjabong", token, n = 1000)
page_sd <- getPage("Snapdeal", token, n = 1000)
page_ebay <- getPage("ebaydotin", token, n = 1000)
Compete <- rbind(page_amzn, page_fk, page_myn, page_jab, page_sd, page_ebay)
if (!file.exists("C:/Users/debdutta/Documents/R/Compete.csv")) 
{
  write.csv(Compete, "C:/Users/debdutta/Documents/R/Compete.csv")
}
Compete <- read.csv("C:/Users/debdutta/Documents/R/Compete.csv", header = T, stringsAsFactors = F, strip.white = T)

#Finding the most popular posts in terms of likes, comments & shares
maxlikes <- Compete[which.max(Compete$likes_count),]
maxcomments <- Compete[which.max(Compete$comments_count),]
maxshares <- Compete[which.max(Compete$shares_count),]

# The most-liked post is from Flipkart and it was about exclusive offers for college students. 
# It was a photo post and had received more than 187K likes.

# The most-commented post is from Amazon and it was about a lucky draw contest on Christmas wishes and 25 lucky winners will have their wishes fulfilled.
# It was a photo post and had received more than 4.3K comments.

# The most-shared post is again from Flipkart and it was about the much-hyped Big Billion Day sale.
# It was a video post and had received more than 6.9K shares.

#Creating a data frame with average metric counts per month
Compete$datetime <- format.facebook.date(Compete$created_time)
Compete$month <- format(Compete$datetime, "%Y-%m")
Compete$post_type <- as.factor(Compete$type)

# Renaming the columns
colnames(Compete) <- c("post_id", "website", "post_text", "created_time", "post_type", "url", "post_id2", "likes", "comments", "shares", "datetime", "month")

# Removing NA column
Compete <- Compete[,-13]                                                               

# Comparative plot of the average number of likes per website
avlikes <- ggplot(Compete, aes(website,likes, colour = website, group = website, fill = website))+
scale_color_brewer(type = qual, palette = "Dark2") + stat_summary(fun.y = "mean",
geom = "point", size = 6) + stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2, size = 1) +
labs(title = "Average number of likes per website \n mean and 95% confidence interval", x = "", y = "Average number of likes")
print(avlikes)

# Comparative plot of the average number of comments per website
avcomments <- ggplot(Compete, aes(website,comments, colour = website, group = website, fill = website))+
scale_color_brewer(type = qual, palette = "Dark2") + stat_summary(fun.y = "mean",
geom = "point", size = 6) + stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2, size = 1) +
labs(title = "Average number of comments per website \n mean and 95% confidence interval", x = "", y = "Average number of comments")
print(avcomments)

# Comparative plot of the average number of shares per website
avshares <- ggplot(Compete, aes(website,shares, colour = website, group = website, fill = website))+
scale_color_brewer(type = qual, palette = "Dark2") + stat_summary(fun.y = "mean",
geom = "point", size = 6) + stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2, size = 1) +
labs(title = "Average number of shares per website \n mean and 95% confidence interval", x = "", y = "Average number of shares")
print(avshares)

# From the comparative plots of average likes, shares & comments, it is evident that Flipkart is the most popular e-commerce website in India.

# Creating a new variable "actions" and adding it to dataset
library(dplyr)
combine <- select(Compete, likes:shares)
actions <- apply(combine, 1, sum)
Compete <- cbind(Compete, actions)

# Plotting the average number of actions for Flipkart by post type
fk_data <- subset(Compete, Compete$website == "Flipkart")
avactions <- ggplot(fk_data, aes(post_type,actions, colour = website, group = website, fill = website))+
scale_color_brewer(type = qual, palette = "Dark2") + stat_summary(fun.y = "mean",
geom = "point", size = 6) + stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0.2, size = 1) +
labs(title = "Average number of actions for Flipkart by post type \n mean and 95% confidence interval", x = "", y = "Average number of actions")
print(avactions)

# Flipkart receives more number of likes, comments and shares for video posts followed by links.
