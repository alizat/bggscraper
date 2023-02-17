
# The `bggscraper` library

<!-- badges: start -->
<!-- badges: end -->

`bggscraper` is an R package that scrapes all sorts of (publicly
accessible) data from the [Board Game Geek](boardgamegeek.com) website.
It depends heavily on the [BGG XML
API2](https://boardgamegeek.com/wiki/page/BGG_XML_API2) library.

***Note that this is a work in progress. However, I intend to make it
useful and usable soon (by end of March, 2023).***

Using this library, you may scrape the following info (among others):

-   list of [top 5000 board
    games](https://boardgamegeek.com/search/boardgame?advsearch=1&q=&sort=rank)
    as per the rankings on BGG.
-   details on board games and board game expansions (i.e., categories,
    mechanics, designers, etc.)
-   BGG users’ games collections, including what they own as well as
    what they hope to play or buy in the future.
-   Info on users themselves.
-   list of games are hot nowadays.
-   Information on plays of a certain game for one or more users.
-   geeklists, forum lists, forums, threads, guilds pertaining to a
    certain board game id.

----

## Installation

You can install the development version of bggscraper from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("alizat/bggscraper")
```

----

## Examples

This is an example that shows how to retrieve information on a
particular user’s (board game) collection:

``` r
library(bggscraper)

# get a user's game collection
user <- 'alizat'
collection_items <- user_collection(user)

# get details for each item in user's collection
games_details <- list()
for (i in 1:nrow(collection_items)) {
    # get details for current item
    games_details[[as.character(collection_items$object_id[[i]])]] <- thing(collection_items$object_id[[i]])
    
    # 5-second sleep so BGG website would not block us
    Sys.sleep(5)
}

# print info for the first game
print(games_details[[1]], n = 100)
```
``` r
#> # A tibble: 32 x 3
#>    game_id feature            value                                             
#>    <chr>   <chr>              <chr>                                             
#>  1 130899  name               12 Days                                           
#>  2 130899  year_published     2011                                              
#>  3 130899  description        The holiday-themed 12 Days takes the familiar &qu~
#>  4 130899  min_players        3                                                 
#>  5 130899  max_players        5                                                 
#>  6 130899  min_playtime       15                                                
#>  7 130899  max_playtime       15                                                
#>  8 130899  playing_time       15                                                
#>  9 130899  min_age            8                                                 
#> 10 130899  category           Card Game                                         
#> 11 130899  category           Print & Play                                      
#> 12 130899  mechanic           Auction/Bidding                                   
#> 13 130899  mechanic           Set Collection                                    
#> 14 130899  mechanic           Trick-taking                                      
#> 15 130899  family             Crowdfunding: Kickstarter                         
#> 16 130899  family             Holidays: Christmas                               
#> 17 130899  designer           James Ernest                                      
#> 18 130899  designer           Mike Selinker                                     
#> 19 130899  artist             Echo Chernik                                      
#> 20 130899  publisher          Calliope Games                                    
#> 21 130899  publisher          Gamesmith, LLC                                    
#> 22 130899  rating_users_rated 1448                                              
#> 23 130899  rating_avg         6.52585                                           
#> 24 130899  rating_bayes_avg   5.93899                                           
#> 25 130899  rank               3033                                              
#> 26 130899  owned              3232                                              
#> 27 130899  trading            67                                                
#> 28 130899  wanting            41                                                
#> 29 130899  wishing            281                                               
#> 30 130899  num_comments       428                                               
#> 31 130899  num_weights        63                                                
#> 32 130899  avg_weight         1.2381
```

<BR>

This is an example that shows to retrieve the current top 10 board games
ids as per [BGG’s rankings](a)

``` r
library(bggscraper)

# top 10 ranked games on BGG
top_K_games_ids <- top_k_games_ids(k = 10)

# get details for each item in user's collection
games_details <- list()
for (id in top_K_games_ids) {
    # get details for current game
    games_details[[id]] <- thing(id)
    current_games_details <- games_details[[id]]
    
    # print current game id and name
    print(glue::glue("{id}: {current_games_details$value[current_games_details$feature == 'name']}"))
    
    # 5-second sleep so BGG website would not block us
    Sys.sleep(5)
}
```
``` r
#> 174430: Gloomhaven
#> 224517: Brass: Birmingham
#> 161936: Pandemic Legacy: Season 1
#> 342942: Ark Nova
#> 233078: Twilight Imperium: Fourth Edition
#> 167791: Terraforming Mars
#> 291457: Gloomhaven: Jaws of the Lion
#> 187645: Star Wars: Rebellion
#> 115746: War of the Ring: Second Edition
#> 162886: Spirit Island
```
