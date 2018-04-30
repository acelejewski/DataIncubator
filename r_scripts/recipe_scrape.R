#recpieie scrape
library(tidyverse)
library(rvest)
library(stringr)
library(parallel)


#Scrape =============================================
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


#scrape recepies into list
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


#save list
#save(recipe_list, file = "data/recipes_list_brewtoad.rds")


names(recipe_list[[1]])[1]

map(recipe_list, function(x){x[1] %>% names }) %>% unlist





#Tidy    ===================================================
recipe_list <- readRDS("data/recipes_list_brewtoad.rds")



#other recipe paramters
reciepe_name <- recipe_list %>% map(names) %>% map(1)
batch_size <- recipe_list %>% map(2) %>% map(1) %>% unlist
OG <- recipe_list %>% map(3) %>% map(1) %>% unlist
FG <- recipe_list %>% map(3) %>% map(2) %>% unlist
IBU <- recipe_list %>% map(3) %>% map(3) %>% unlist
SRM <- recipe_list %>% map(3) %>% map(4) %>% unlist



#wrangle data for malt hops, yeast


# * wrangle hops ============
hops <- recipe_list %>% map(1) %>% map(2)




#HOP df descriptors
hops <- map(hops, function(x){mutate(x, Hop = as.character(Hop))}  )
hops  <- map2(hops, reciepe_name, function(x, y){mutate(x, Clone_of  =  y)  }  )
hops <- map2(hops, batch_size, function(x, y){mutate(x, Batch_Size = y) }  )
hops <- map2(hops, IBU, function(x, y){mutate(x, IBU = y) }  )
hops <- map2(hops, SRM, function(x, y){mutate(x, SRM = y) }  )

hops <- bind_rows(hops) %>% as_tibble



#are ounces and grams the only units?
(hops$Amount[str_detect(hops$Amount, "oz")] %>% length + hops$Amount[str_detect(hops$Amount, "g")] %>% length()) == nrow(hops)

#are gals and Ls the only batch size units?
(hops$Batch_Size  [str_detect(hops$Batch_Size  , "gal")] %>% length + hops$Batch_Size  [str_detect(hops$Batch_Size  , "L")] %>% length()) == nrow(hops)


#partial tidy of hop additon data
hops <- hops %>% 
  mutate(is_ounces = if_else(str_detect(.$Amount, "oz"), TRUE, FALSE)) %>%
  mutate(Amount = str_extract(.$Amount, "[:digit:]+.[:digit:]+") %>% as.numeric()) %>%
  mutate(Amount = if_else(.$is_ounces ==TRUE, .$Amount, .$Amount* 0.035274 )) %>% #convet weight to ounces
  mutate(is_gal = if_else(str_detect(.$Batch_Size, "gal"), TRUE, FALSE)) %>%
  mutate(Batch_Size = str_extract(.$Batch_Size   , "[:digit:]+.[:digit:]+") %>% as.numeric()) %>%
  mutate(Batch_Size = if_else(.$is_gal ==TRUE, .$Batch_Size, .$Batch_Size* 0.264172 )) %>% #convet voliume to ounces
  separate(Hop, into = c("Hop", "Hop_Country"), sep = " \\(" ) %>% 
  mutate(Hop_Country = str_remove(.$Hop_Country, "\\)")) %>%
  select(-is_ounces, -is_gal)

write_csv(hops, "data/hops.csv")  

# * wrangel malts ==========         
         
malt <- recipe_list %>% map(1) %>% map(1)

malt  <- map2(malt, reciepe_name, function(x, y){mutate(x, Clone_of  =  y)  }  )
malt  <- map2(malt, OG, function(x, y){mutate(x, OG  =  y)  }  )
malt  <- map2(malt, FG, function(x, y){mutate(x, FG  =  y)  }  )
malt <- map2(malt, batch_size, function(x, y){mutate(x, Batch_Size = y) }  )
malt <- map2(malt, SRM, function(x, y){mutate(x, SRM = y) }  )
malt <- map2(malt, IBU, function(x, y){mutate(x, IBU = y) }  )

malt <- bind_rows(malt) %>% as_tibble()



malt <- malt %>% 
  separate(Amount, into = c("Amount", "Amount_Units"), "\\s") %>%
  mutate(Amount = as.numeric(Amount)) %>%
  mutate(Amount = if_else((Amount_Units == "oz"), Amount* 0.0625, 
                          if_else((Amount_Units == "g"), Amount * 0.00220462,
                                  if_else((Amount_Units == "kg"), Amount* 2.20462, Amount)))) %>%
  mutate(is_gal = if_else(str_detect(.$Batch_Size, "gal"), TRUE, FALSE)) %>%
  mutate(Batch_Size = str_extract(.$Batch_Size   , "[:digit:]+.[:digit:]+") %>% as.numeric()) %>%
  mutate(Batch_Size = if_else(.$is_gal ==TRUE, .$Batch_Size, .$Batch_Size* 0.264172 )) %>% #convet voliume to ounces
  separate(Fermentable, into = c("Fermentable", "Fermentable_Country"), sep = " \\(" ) %>% 
  mutate(Fermentable_Country = str_remove(.$Fermentable_Country, "\\)")) %>%
  select(-Amount_Units, -is_gal) 


write_csv(malt, "data/malt.csv")  


#* warangel yeast  

yeast <-  recipe_list %>% map(1) %>% map(3) %>% map(2) %>% map(as_tibble) 
yeast <- map(yeast, function(x){mutate(x, value = as.character(value))}  )

yeast <-  map(yeast, function(x) {x$value}) 


map(yeast, function(x){is.na(x)}) %>% unlist



a <ind_rows(yeast) %>% unlist


yeast  <- map2(yeast, reciepe_name, function(x, y){mutate(x, Clone_of  =  y)  }  )

is_null<- map(yeast, is.na) %>% unlist

is_null[is_null == TRUE]

 <- yeast %>% map(nrow) %>% unlist 

reciepe_name[which(a > 1)]
yeast[which(a > 1)]





yeast %>% bind_rows()
