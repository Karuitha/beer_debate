##THE BEER DEBATE PROJECT ----
# Objective- to get and clean data on beer ratings ----
# Set working directory ----
setwd("C:\\Users\\John Karuitha\\OneDrive - University of Witwatersrand\\Documents\\My Thesis\\Karuitha and Ojah Data\\r_training\\beer_debate")
save.image("C:/Users/John Karuitha/OneDrive - University of Witwatersrand/Documents/My Thesis/Karuitha and Ojah Data/r_training/beer_debate/data_beer.R.RData")
# Load required packages ----
library(tidyverse)
library(rvest)
library(ggthemes)
library(plotly)
library(GGally)

# Write a scrapping function ----
scraper <- function(x){
  read_html(x) %>% 
    html_nodes("table") %>% 
    html_table() %>% 
    .[[1]]
}

# scrape the data ----
# Top 250
top_250 <- scraper("https://www.beeradvocate.com/beer/top-rated/")

# Trending 100
trend_100 <- scraper("https://www.beeradvocate.com/beer/trending/")

# New beers 
new_beers <- scraper("https://www.beeradvocate.com/beer/top-new/")

# Fame beer
fame_beers <- scraper("https://www.beeradvocate.com/beer/fame/")

# Popular beer 
popular_beers <- scraper("https://www.beeradvocate.com/beer/popular/")

# Rename columns and drop first row ----
# Top 250
names(top_250) <- c("weighted_rank", "beer", "no_of_ratings", 
                    "average_ratings", "my_rating")

top_250 <- top_250[-1,]

# Trending 100
names(trend_100) <- c("weighted_rank", "beer", "no_of_ratings", 
                    "average_ratings", "my_rating")

trend_100 <- trend_100[-1,]

# New beers
names(new_beers) <- c("weighted_rank", "beer", "no_of_ratings", 
                      "average_ratings", "my_rating")

new_beers <- new_beers[-1,]

# Fame beers 
names(fame_beers) <- c("weighted_rank", "beer", "no_of_ratings", 
                      "average_ratings", "my_rating")

fame_beers <- fame_beers[-1,]

# Popular beers 
names(popular_beers) <- c("weighted_rank", "beer", "no_of_ratings", 
                      "average_ratings", "my_rating")

popular_beers <- popular_beers[-1,]


## Merge the datasets and start cleaning ----
full_beer_data <- rbind(top_250, trend_100, new_beers, 
                        fame_beers, popular_beers)

full_beer_data <- full_beer_data[,-c(1,5)]

## Feature engineering ----
full_beer_data$alcohol_percent <- 
  str_extract(full_beer_data$beer, "\\d*\\.\\d\\d%")

## Remove % from the alcohol % column and make numeric 
full_beer_data$alcohol_percent <- 
  str_remove_all(full_beer_data$alcohol_percent, "%") %>% 
  as.numeric()

## Remove , from rating columns and make numeric 
full_beer_data$no_of_ratings <- 
  str_remove_all(full_beer_data$no_of_ratings, ",") %>% 
  as.numeric()
## Convert average ratings to numeric 
full_beer_data$average_ratings <- 
  as.numeric(full_beer_data$average_ratings)

##Remove the % and | sign from the beer column ----
str_detect(full_beer_data$beer, "\\s*\\|\\s*\\d*\\.\\d*%")

full_beer_data$beer <- str_remove_all(full_beer_data$beer, 
                                "\\s*\\|\\s*\\d*\\.\\d*%")

## Add beer type column ----
full_beer_data$type <- NULL
## Adding stouts  ----
full_beer_data$type <- ifelse(str_detect(full_beer_data$beer, 
                        "[Ss]tout"), "Stout", NA)

## Adding IPA - Indian Pale Ale ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "IPA"), "IPA", full_beer_data$type)

## Adding IPA - Wild Ale ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                      str_detect(full_beer_data$beer, 
                      "[Ww]ild\\s*[Aa]le"), "Wild Ale", 
                      full_beer_data$type)
## Adding pale ale----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                        str_detect(full_beer_data$beer, 
                          "[Pp]ale\\s*[Aa]le"), "Pale Ale", 
                              full_beer_data$type)
# Adding Strong ale -----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                                           "[Ss]trong Ale"), "Strong Ale", 
                              full_beer_data$type)

# Adding Farmhouse ale -----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "PLCOld Ale"), "PLCOld Ale", 
                              full_beer_data$type)

# Adding Strong ale -----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                                "[Ss]trong Ale"), "Strong Ale", 
                              full_beer_data$type)

## Adding IPA - Ale ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                        str_detect(full_beer_data$beer, 
                        "Ale"), "Ale", full_beer_data$type)

## Adding lambic ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                        str_detect(full_beer_data$beer, 
                          "[Ll]ambic"), "Lambic", 
                              full_beer_data$type)

## Adding lager----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                        str_detect(full_beer_data$beer, 
                       "[Ll]ager"), "Lager", 
                        full_beer_data$type)

## Adding sour ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                                "[Ss]our"), "Sour", 
                                 full_beer_data$type)

## Adding Barleywine ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                                "[Bb]arleywine"), "Barleywine", 
                              full_beer_data$type)

## Adding Porter ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                                "[Pp]orter"), "Porter", 
                              full_beer_data$type)

## Adding wheat beer ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "[Ww]heat Beer"), "Wheet Beer", 
                              full_beer_data$type)

## adding Rye beer ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "[Rr]ye Beer"), "Rye Beer", 
                              full_beer_data$type)

## adding Fruit and Field Beer---- 
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                                "[Ff]ruit and Field Beer"), 
                              "Fruit and Field Beer", 
                              full_beer_data$type)

## Adding Pilsner ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                               "[Pp]ilsner"), "Pilsner", 
                              full_beer_data$type)

## Adding Bock ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                                "[Bb]ock"), "Bock", 
                              full_beer_data$type)

## Adding Tripelr ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                                "[Tt]ripel"), "Tripel", 
                              full_beer_data$type)

## ## Adding Quadrupel (Quad) ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                              str_detect(full_beer_data$beer, 
                              "[Qq]uadrupel\\s*\\(Quad\\)"), "Quadrupel (Quad)", 
                              full_beer_data$type)

## Adding Dubbel ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "[Dd]ubbel"), "Dubbel", 
                              full_beer_data$type)

## Adding Herb and Spice Beer ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                                "[Hh]erb and Spice Beer"), 
                              "Herb and Spice Beer", 
                              full_beer_data$type)

## Adding Champagne  ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "[Bb]ière de Champagne \\/ Bière Brut"), 
                              "Bière de Champagne / Bière Brut", 
                              full_beer_data$type)

## Adding HausbrauereiAltbier ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "HausbrauereiAltbier"), 
                              "HausbrauereiAltbier", 
                              full_beer_data$type)

## Adding Kölsch ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "Kölsch"), 
                              "Kölsch", 
                              full_beer_data$type)

## Adding Brett Beer  ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "Brett Beer"), 
                              "Brett Beer", 
                              full_beer_data$type)

## Adding Pumpkin Beer  ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "Pumpkin Beer"), 
                              "Pumpkin Beer", 
                              full_beer_data$type)

## Adding California Common / Steam Beer  ----
full_beer_data$type <- ifelse(is.na(full_beer_data$type) & 
                                str_detect(full_beer_data$beer, 
                              "California Common \\/ Steam Beer"), 
                              "California Common / Steam Beer", 
                              full_beer_data$type)

###################################################################################
## Add beer category light, medium, strong----
full_beer_data$category <- case_when(
  full_beer_data$alcohol_percent <= 8 ~ "lite",
  full_beer_data$alcohol_percent > 8 & 
    full_beer_data$alcohol_percent <= 16 ~ "medium",
  full_beer_data$alcohol_percent > 16 ~ "strong")

ggplotly(full_beer_data %>% ggplot(aes(y = average_ratings, 
                              x = category, fill = category)) + geom_boxplot())

######################################################################################
## Add column for subtype and clean dataset ----
full_beer_data$subtype <- 
  ifelse(str_detect(full_beer_data$beer, 
  "\\s*\\-\\s*\\W*"), str_extract_all(full_beer_data$beer, 
  "\\s*\\-\\s*\\w*\\s*\\w*\\s*\\w*"), NA)


########################################################################################
## Feature engineer moren----
# Types ----
full_beer_data$type <- factor(full_beer_data$type) 
                              #levels = names(sort(table(full_beer_data$type))))

class(full_beer_data$type)



##########################################################################################
##Visualize the data ----
median_na <- function(x){median(x, na.rm = TRUE)}

ggplotly(full_beer_data %>% group_by(type) %>% filter(n() >= 7) %>% ungroup %>% 
  ggplot(aes(x = reorder(type, alcohol_percent, median_na), 
  y = alcohol_percent, fill = type)) + geom_boxplot() + 
  theme_hc() + theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 90)) + 
  labs(y = "Alcohol Percentage", x = "Type of Beer", 
       title = "A Visual Guide to Choosing Your Poison", 
       caption = "John Karuitha (2020),
       Data Source: Beer Advocate- Your Go-To Resource for Beer
       Website: https://www.beeradvocate.com, 
       **Respect Beer"))


##Alcohol content vs ratings ----
ggplotly(full_beer_data %>% group_by(type) %>% ggplot(aes(x = alcohol_percent, 
                              y = average_ratings, 
                              color = type)) + 
                              geom_point(alpha = 0.5))

#################################################################################################
full_beer_data <- full_beer_data %>% relocate(brewer, .after = beer)
################################################################################################

## Extract the brewers name----
full_beer_data$brewer <- NA
##Toppling Goliath Brewing company ----
full_beer_data$brewer <- NA
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                        str_detect(full_beer_data$beer, "Toppling Goliath Brewing Company"), 
                        "Toppling Goliath Brewing Company", full_beer_data$brewer)

## 3 Floyds Brewing Co.
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                          str_detect(full_beer_data$beer, "3 Floyds Brewing Co."), 
                         "3 Floyds Brewing Co.", full_beer_data$brewer)

## Perennial Artisan Ales
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, "Perennial Artisan Ales"), 
                                "Perennial Artisan Ales", full_beer_data$brewer)

## Cigar City Brewing
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, "Cigar City Brewing"), 
                                "Cigar City Brewing", full_beer_data$brewer)


## The Alchemist
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, "The Alchemist"), 
                                "The Alchemist", full_beer_data$brewer)

## Russian River Brewing Company
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Russian River Brewing Company"), 
                                "Russian River Brewing Company", 
                                full_beer_data$brewer)
## Tree House Brewing Company
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                 "Tree House Brewing Company"), 
                                "Tree House Brewing Company", 
                                full_beer_data$brewer)

## Goose Island Beer Co.
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Goose Island Beer Co."), 
                                "Goose Island Beer Co.", 
                                full_beer_data$brewer)
## Bottle Logic Brewing
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Bottle Logic Brewing"), 
                                "Bottle Logic Brewing", 
                                full_beer_data$brewer)
# Founders Brewing Company
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Founders Brewing Company"), 
                                "Founders Brewing Company", 
                                full_beer_data$brewer)

## Brasserie Cantillon
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                str_detect(full_beer_data$beer, 
                                "Brasserie Cantillon"), 
                                "Brasserie Cantillon", 
                                full_beer_data$brewer)
## Lawson's Finest Liquids
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Lawson's Finest Liquids"), 
                                "Lawson's Finest Liquids", 
                                full_beer_data$brewer)
## Brouwerij 3 Fonteinen
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Brouwerij 3 Fonteinen"), 
                                "Brouwerij 3 Fonteinen", 
                                full_beer_data$brewer)

## Hill Farmstead Brewery
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Hill Farmstead Brewery"), 
                                "Hill Farmstead Brewery", 
                                full_beer_data$brewer)

## Brouwerij Westvleteren (Sint-Sixtusabdij van Westvleteren)
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Brouwerij Westvleteren \\(Sint-Sixtusabdij van Westvleteren\\)"), 
                                "Brouwerij Westvleteren (Sint-Sixtusabdij van Westvleteren)", 
                                full_beer_data$brewer)

## Angry Chair Brewing
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Angry Chair Brewing"), 
                                "Angry Chair Brewing", 
                                full_beer_data$brewer)

## Maine Beer Company
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Maine Beer Company"), 
                                "Maine Beer Company", 
                                full_beer_data$brewer)

## The Lost Abbey
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "The Lost Abbey"), 
                                "The Lost Abbey", 
                                full_beer_data$brewer)

## Firestone Walker Brewing Co.
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Firestone Walker Brewing Co."), 
                                "Firestone Walker Brewing Co.", 
                                full_beer_data$brewer)

## Trillium Brewing Company
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Trillium Brewing Company"), 
                                "Trillium Brewing Company", 
                                full_beer_data$brewer)

## Smith Brewing Company
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Smith Brewing Company"), 
                                "Smith Brewing Company", 
                                full_beer_data$brewer)

## Funky Buddha Brewery
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Funky Buddha Brewery"), 
                                "Funky Buddha Brewery", 
                                full_beer_data$brewer)

## Sante Adairius Rustic Ales
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                str_detect(full_beer_data$beer, 
                                "Sante Adairius Rustic Ales"), 
                                "Sante Adairius Rustic Ales", 
                                full_beer_data$brewer)

## Side Project Brewing
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Side Project Brewing"), 
                                "Side Project Brewing", 
                                full_beer_data$brewer)

## Jester King Brewery
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Jester King Brewery"), 
                                "Jester King Brewery", 
                                full_beer_data$brewer)

## Kern River Brewing Company
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Kern River Brewing Company"), 
                                "Kern River Brewing Company", 
                                full_beer_data$brewer)

## Bissell Brothers Brewing Co.
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Bissell Brothers Brewing Co."), 
                                "Bissell Brothers Brewing Co.", 
                                full_beer_data$brewer)

## Premierede Garde Brewing
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "Premierede Garde Brewing"), 
                                "Premierede Garde Brewing", 
                                full_beer_data$brewer)

## The Bruery
full_beer_data$brewer <- ifelse(is.na(full_beer_data$brewer) & 
                                  str_detect(full_beer_data$beer, 
                                "The Bruery"), 
                                "The Bruery", 
                                full_beer_data$brewer)
