##THE BEER DEBATE PROJECT ----
# Objective- to get and clean data on beer ratings ----
# Load required packages ----
library(tidyverse)
library(rvest)
library(ggthemes)
library(plotly)
library(GGally)

###############################################################
## Scrapping function -----
###############################################################

my_comprehensive_scrapper <- function(url, n_rows, category){
  
## get the Beers, number of votes and average rating ----
  
  beer_data_main <- read_html(url) %>% 
    
    html_nodes(".hr_bottom_light b") %>% 
    
    html_text() %>% 
    
    tibble() %>% 
    
    mutate(key = rep(1:3, n_rows))
  
  beer <- data.frame(beer = filter(beer_data_main, key == 1))
  
  votes <- data.frame(votes = filter(beer_data_main, key == 2))
  
  avg <- data.frame(avg = filter(beer_data_main, key == 3))
  
  long_beers <- bind_cols(beer, votes, avg) %>% 
    
    select(-ends_with("key")) %>% 
    
    set_names(c("beer", "votes", "rating_average"))
  
  
## Get the beer category- like stout ----
  
  beer_category <- read_html(url) %>% 
    
    html_nodes("#ba-content a~ a") %>% 
    
    html_text() %>% 
    
    tibble() %>% 
    
    set_names("type")
  
## Get the full table  ----
  
  full_table <- read_html(url) %>% 
    
    html_nodes("table") %>% 
    
    html_table() %>% 
    
    .[[1]] %>% 
    
    filter(!is.na(X1)) %>% 
    
    select(-X1, -X3, -X4, -X5)
  
## Combine the three tables to form one table ----
  
long_beers %>% bind_cols(full_table) %>% 
    
    bind_cols(beer_category) %>% 
    
    mutate(X2 = str_remove_all(X2, beer)) %>% 
    
    mutate(X2 = str_remove_all(X2, type)) %>% 
    
    mutate(alcohol_perc = str_extract(X2, "\\d{1,2}\\.\\d{2}%$"), 
           
           X2 = str_remove_all(X2, "\\|\\s?\\d{1,2}\\.\\d{2}%$"),
           
           alcohol_perc = parse_number(alcohol_perc),
           
           category = category) %>% 
    
    rename(brewer = X2)
  
}


###################################################################################

top_250_beers <- my_comprehensive_scrapper(url = "https://www.beeradvocate.com/beer/top-rated/", 
                                           
                                           n_rows = 250, category = "top_250")


trending_beers <- my_comprehensive_scrapper(url = "https://www.beeradvocate.com/beer/trending/", 
                                            
                                            n_rows = 100, category = "trending")

top_new <- my_comprehensive_scrapper(url = "https://www.beeradvocate.com/beer/top-new/", 
                                            
                                            n_rows = 250, category = "new")

fame_beer <- my_comprehensive_scrapper(url = "https://www.beeradvocate.com/beer/fame/", 
                                     
                                     n_rows = 250, category = "fame")

popular_beer <- my_comprehensive_scrapper(url = "https://www.beeradvocate.com/beer/popular/", 
                                       
                                       n_rows = 250, category = "popular")

## Combine the three datasets ----

full_beer_data <- top_250_beers %>% 
  
  bind_rows(trending_beers) %>% 
  
  bind_rows(top_new) %>% 
  
  bind_rows(fame_beer) %>% 
  
  bind_rows(popular_beer) %>% 
  
## Add alcohol percentage for beers with the missing data

  mutate(
    
    alcohol_perc = case_when(
      
      beer == "Rare Scooop" ~ 6.16,
      
      beer == "Madness & Civilization #14" ~ 3.81,
      
      beer == "Thumbprint Lots O' Peach 21" ~ 7.55,
      
      beer == "Persevere" ~ 6.12,
      
      beer == "Sankt" ~ 7.21,
      
      beer == "Nonconformist 02" ~ 7.94,
      
      TRUE ~ alcohol_perc
      
    )) %>% 

###################################################################################
## Add beer category light, medium, strong
  mutate(category = case_when(
    
    alcohol_perc <= 5 ~ "lite",
    
    alcohol_perc > 5 & alcohol_perc <= 10 ~ "medium",
    
    alcohol_perc > 10 & alcohol_perc <= 15 ~ "strong",
    
    alcohol_perc > 15 & alcohol_perc <= 20 ~ "super",
    
    TRUE ~ "ultra"
    
  )) %>% 
  
## Convert rating average and votes to numeric
  
mutate(votes = parse_number(votes),
       
       rating_average = parse_number(rating_average))


##########################################################################
## Check for duplicates ----
full_beer_data %>% 
  
  filter(duplicated(.)) %>% 
  
  nrow()

##########################################################################
  

full_beer_data %>% 
  
  filter(!duplicated(.)) %>% 
  
  ggplot(aes(x = category, y = rating_average, fill = category)) + 
  
  geom_boxplot(show.legend = FALSE, outlier.color = "red", outlier.shape = 1,
               
               outlier.size = 4) + 
  
  geom_jitter(alpha = 0.15, show.legend = FALSE) + 
  
  scale_fill_brewer(palette = 8) + 
  
  ggthemes::theme_fivethirtyeight()

??position_jitter
########################################################################################
## Feature engineer moren----
# Types ----
full_beer_data$type <- factor(full_beer_data$type) 
                              #levels = names(sort(table(full_beer_data$type))))

class(full_beer_data$type)



##########################################################################################
##Visualize the data ----

full_beer_data %>% 
           
           group_by(type) %>% 
  
           filter(n() > 10) %>% 
           
           ungroup %>% 
           
  ggplot(aes(x = reorder(type, alcohol_perc, median), 
             
  y = alcohol_perc, fill = type)) + 
    
    geom_boxplot() + 
    
  theme_hc() + theme(legend.position = "none") + 
    
  theme(axis.text.x = element_text(angle = 90)) + 
    
  labs(y = "Alcohol Percentage", x = "Type of Beer", 
       title = "BEER DEBATE",
       subtitle = "A Visual Guide to Choosing Your Poison", 
       caption = "John Karuitha (2020),
       Data Source: Beer Advocate- Your Go-To Resource for Beer
       Website: https://www.beeradvocate.com, 
       **Respect Beer") + 
  
  theme(title = element_text(size = 20))


##Alcohol content vs ratings ----
ggplotly(full_table_top_250 %>% group_by(type) %>% 
           
           ggplot(aes(x = alcohol_perc, 
                      
                              y = average, 
                      
                              color = type)) + 
           
                              geom_point(alpha = 0.5))


full_table_top_250 %>% 
  
  group_by(type) %>% 
  
  summarise(n(), mean(alcohol_perc, na.rm = TRUE), 
            
            mean(average, na.rm = TRUE)) %>% 
  
  arrange(`n()`)



full_table_top_250 %>% 
  
  ggplot(aes(x = fct_reorder(type, alcohol_perc, max), y = alcohol_perc, 
             
             fill = type)) + 
  
  geom_boxplot(show.legend = FALSE) + 
  
  ggthemes::theme_clean() +
  
  theme(axis.text.x = element_text(angle = 90)) +
  
  labs(y = "Alcohol Percentage", x = "Type of Beer", 
       title = "THE BEER DEBATE",
       subtitle = "A Visual Guide to Choosing Your Poison", 
       caption = "John Karuitha (2020),
       Data Source: Beer Advocate- Your Go-To Resource for Beer
       Website: https://www.beeradvocate.com, 
       **Respect Beer")



                    