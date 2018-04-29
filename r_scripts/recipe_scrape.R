#recepeie scrape
library(tidyverse)
library(rvest)
library(stringr)
library(parallel)
library(beepr)



#determine nubmer urls by term
term <- "clone"


url <- str_c("https://www.brewtoad.com/recipes?page=1&sort=rank&name=", term)
pages_n <- read_html("https://www.brewtoad.com/recipes?page=1&sort=rank&name=clone") %>%
  html_node(".pagination") %>% 
  html_text() %>% 
  str_extract_all("[:digit:]+") %>%
  unlist() %>%
  max

#urls to be scraped
urls <- str_c("https://www.brewtoad.com/recipes?page=", 1:pages_n, "&sort=rank&name=", term)


#scrpae recepies urls and and show progress
pb <- progress_estimated(length(urls))
recipes_brewtoad  <- map(urls, read_recipes, .pb = pb)
recipes_brewtoad <- recipes_brewtoad %>% bind_rows()
recipe_list <- vector("list", nrow(recipes_brewtoad))


#scrape 
time_total <- Sys.time()
for (i  in 1:length(recipe_list)){
  
  time <- Sys.time()  
  url <- recipes_brewtoad$link[i]
  name <- recipes_brewtoad$name[i]
  
  
  html <-  read_html(url)
  
  
  tables <- html %>% html_table() 
  
  stats_2 <- html %>% 
    html_nodes(".recipe-show--stats  .value")  %>% 
    html_text()
  
  stats_1 <- html %>%  html_nodes(".stat-group-thirds li") %>% html_text(trim = TRUE) 
  
  recipe_list[[i]] <- list(tables, stats_1,stats_2)
  
  names(recipe_list[[i]]) <- name
  
  print(paste(name, i, "of", length(recipe_list), "in", Sys.time() - time, "runing for:", Sys.time() - time_total))
}



save(recipe_list, file = "recipes_clones_brewtoad")


names(recipe_list[[1]])[1]

map(recipe_list, function(x){x[1] %>% names }) %>% unlist

a <- recipe_list[[1]][1] %>% names


readRDS("recipes_clones_brewtoad.rds")


recipe_list[[1]][[1]][[1]]$Name  <- a





readRDS(recipes_list_brewtoad)






malts <- map(recipe_list, function(x){x[[1]][[1]]})
hops <- map(recipe_list, function(x){x[[1]][[2]]})

map(hops, function(x){x = })



bind_rows(hops) 
bind_rows(malts)

b[1]

str(recipe_list)
