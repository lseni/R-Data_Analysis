# Text Crawling Code

# Install these libraries before using
library(rvest)
library(stringr)

setwd("C:/R_wd")

# Url of the site you want to crawling
main_url = "https://---------------"

# ex) Movie review site
review_list = character()
star_list = numeric()
date_list = character()

# Set the last page you want to crawling
for(page_url in 1:30){              
  url = paste(main_url, page_url, sep="")
  content = read_html(url)
  node_1 = html_nodes(content, ".score_reple p")
  node_2 = html_nodes(content, ".score_result .star_score em")
  node_3 = html_nodes(content, ".score_reple em:nth-child(2)")
  review = html_text(node_1)
  star = html_text(node_2)
  date = html_text(node_3)
  date = as.Date(gsub("\\.","-", date))
  review_list = append(review_list, review)
  star_list = append(star_list, star)
  date_list = append(date_list, date)
}

df = data.frame(review_list, star_list, date_list)
colnames(df) = c("review","rank","date")

# Pretreatment
df$review <- str_replace_all(string = df$review, pattern = "[^가-힣]", replacement = " ")
df$review <- str_squish(df$review)

# Remove Stop words (불용어) : --------------------------
df$review <- str_replace_all(string = df$review, pattern = "----", replacement = " ")
df$review <- str_squish(df$review)

write.csv(df, "file_name.csv", row.names = F)

# Write the text file
write.table(df$review, file="file_name.txt", quote=FALSE, fileEncoding = "UTF-8", col.names = FALSE, row.names = FALSE)
