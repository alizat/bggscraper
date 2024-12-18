
<!-- README.md is generated from README.Rmd. Please edit that file -->

# The `bggscraper` project

<!-- badges: start -->
<!-- badges: end -->

`bggscraper` is a bundle of scripts that provide the functionality to
scrape all sorts of (publicly accessible) data from the [Board Game
Geek](boardgamegeek.com) website. It depends heavily on the [BGG XML
API2](https://boardgamegeek.com/wiki/page/BGG_XML_API2) library.

`bggscraper` was previously an R package, that could be installeda and
used. However, I decided against making it a package because I found
myself wasting quite a lot of time doing package development as opposed
to writing the scraping code itself. Perhaps, when I am done writing all
the code for scraping the BGG website, I will convert it back to an R
package again; however, this remains to be seen.

***Note: This is a work in progress!***

Using this library, you may scrape the following info (among others):

- list of [top 5000 board
  games](https://boardgamegeek.com/search/boardgame?advsearch=1&q=&sort=rank)
  as per the rankings on BGG.
- details on board games and board game expansions (i.e., categories,
  mechanics, designers, etc.)
- BGG users’ games collections, including what they own as well as what
  they hope to play or buy in the future.
- info on users themselves.
- list of games that are hot nowadays.
- information on plays of a certain game for one or more users.
- geeklists, forum lists, forums, threads or guilds pertaining to a
  certain board game id.

## Examples

This is an example that shows how to retrieve information on a
particular user’s (board game) collection:

``` r
source('common_funcs.R')

# get a user's game collection
user <- 'alizat'
collection_items <- collection(user)
collection_items_owned <- collection_items %>% filter(owned == "1")

# display names of that user's games
print(collection_items_owned$item_name)
#>   [1] "12 Days"                                           
#>   [2] "3 Wishes"                                          
#>   [3] "Age of Galaxy"                                     
#>   [4] "Agropolis"                                         
#>   [5] "Air, Land & Sea"                                   
#>   [6] "Architects of the West Kingdom"                    
#>   [7] "Attack on Titan: Deck-Building Game"               
#>   [8] "Azul"                                              
#>   [9] "Bandida"                                           
#>  [10] "Bärenpark"                                         
#>  [11] "Beasty Bar 3: Born to Be Wild"                     
#>  [12] "The Blood of an Englishman"                        
#>  [13] "Blood Rage"                                        
#>  [14] "Boss Monster: The Dungeon Building Card Game"      
#>  [15] "Capital Lux 2: Generations"                        
#>  [16] "Captain Sonar"                                     
#>  [17] "Cat in the Box: Deluxe Edition"                    
#>  [18] "Celestia"                                          
#>  [19] "Champions of Midgard"                              
#>  [20] "Champions of Midgard: The Dark Mountains"          
#>  [21] "Champions of Midgard: Valhalla"                    
#>  [22] "Citadels"                                          
#>  [23] "Clue: Card Game"                                   
#>  [24] "Codenames"                                         
#>  [25] "Colt Super Express"                                
#>  [26] "Couture"                                           
#>  [27] "The Crew: Mission Deep Sea"                        
#>  [28] "Crossfire"                                         
#>  [29] "Custom Heroes"                                     
#>  [30] "Dale of Merchants"                                 
#>  [31] "Dead of Winter: A Crossroads Game"                 
#>  [32] "Deception: Murder in Hong Kong"                    
#>  [33] "Decrypto"                                          
#>  [34] "Deduckto"                                          
#>  [35] "Deep Sea Adventure"                                
#>  [36] "Déjà Vu"                                           
#>  [37] "Dinosaur Tea Party"                                
#>  [38] "Dixit"                                             
#>  [39] "Dixit: Origins"                                    
#>  [40] "Dominion (Second Edition)"                         
#>  [41] "Dominion: Allies"                                  
#>  [42] "Dominion: Menagerie"                               
#>  [43] "DOS"                                               
#>  [44] "Downforce"                                         
#>  [45] "Elysium"                                           
#>  [46] "Empire's End"                                      
#>  [47] "Ethnos"                                            
#>  [48] "A Fake Artist Goes to New York"                    
#>  [49] "Fantasy Realms"                                    
#>  [50] "Filler"                                            
#>  [51] "Floriferous"                                       
#>  [52] "Fort"                                              
#>  [53] "The Fox in the Forest"                             
#>  [54] "Furnace"                                           
#>  [55] "Gloomhaven: Jaws of the Lion"                      
#>  [56] "Gravwell: 2nd Edition"                             
#>  [57] "Hanabi"                                            
#>  [58] "Hanamikoji"                                        
#>  [59] "Happy Salmon"                                      
#>  [60] "Herd Mentality"                                    
#>  [61] "High Society"                                      
#>  [62] "Hippo"                                             
#>  [63] "ICECOOL"                                           
#>  [64] "Illusion"                                          
#>  [65] "Infinity Gauntlet: A Love Letter Game"             
#>  [66] "Inheritors"                                        
#>  [67] "Inis"                                              
#>  [68] "Insider"                                           
#>  [69] "Jaws"                                              
#>  [70] "Jórvík"                                            
#>  [71] "Junk Art"                                          
#>  [72] "Kemet"                                             
#>  [73] "The King Is Dead: Second Edition"                  
#>  [74] "Kobayakawa"                                        
#>  [75] "The Lady and the Tiger"                            
#>  [76] "Long Shot: The Dice Game"                          
#>  [77] "Love Letter"                                       
#>  [78] "Magic Maze"                                        
#>  [79] "Memoarrr!"                                         
#>  [80] "The Mind"                                          
#>  [81] "Mystic Vale"                                       
#>  [82] "Neuroshima Hex! 3.0"                               
#>  [83] "Neuroshima Hex! 3.0: Iron Gang"                    
#>  [84] "Neuroshima Hex! 3.0: New York"                     
#>  [85] "Neuroshima Hex! 3.0: Pirates"                      
#>  [86] "Neuroshima Hex! 3.0: Steel Police"                 
#>  [87] "Neuroshima Hex! 3.0: Uranopolis"                   
#>  [88] "No Thanks!"                                        
#>  [89] "Not Alone"                                         
#>  [90] "Oh My Goods!"                                      
#>  [91] "Oh My Goods!: Longsdale in Revolt"                 
#>  [92] "Ohanami"                                           
#>  [93] "Order Overload: Cafe"                              
#>  [94] "Pandemic"                                          
#>  [95] "Pandemic: On the Brink"                            
#>  [96] "Pictionary Card Game"                              
#>  [97] "Pictomania (Second Edition)"                       
#>  [98] "Pixel Tactics 5"                                   
#>  [99] "Point Salad"                                       
#> [100] "Port Royal"                                        
#> [101] "Pyramid Arcade"                                    
#> [102] "The Quacks of Quedlinburg"                         
#> [103] "Quadropolis"                                       
#> [104] "Quantum"                                           
#> [105] "Quirky Circuits"                                   
#> [106] "Ra"                                                
#> [107] "Raccoon Tycoon"                                    
#> [108] "Railroad Ink: Deep Blue Edition"                   
#> [109] "Raptor"                                            
#> [110] "Rune Stones"                                       
#> [111] "Schotten Totten"                                   
#> [112] "SCOUT"                                             
#> [113] "Scrabble"                                          
#> [114] "Scram!"                                            
#> [115] "The Shipwreck Arcana"                              
#> [116] "Similo"                                            
#> [117] "Sluff Off!"                                        
#> [118] "Smartphone Inc."                                   
#> [119] "Songbirds"                                         
#> [120] "Space Base"                                        
#> [121] "Spirit Island"                                     
#> [122] "Spot it!"                                          
#> [123] "Star Fluxx"                                        
#> [124] "Star Realms"                                       
#> [125] "Stockpile: Epic Edition"                           
#> [126] "Sundae Split"                                      
#> [127] "Super Motherload"                                  
#> [128] "Survive: Escape from Atlantis!"                    
#> [129] "Sushi Go!"                                         
#> [130] "Taco Cat Goat Cheese Pizza"                        
#> [131] "Take 5!"                                           
#> [132] "Tem-Purr-A"                                        
#> [133] "That's Not a Hat"                                  
#> [134] "Ticket to Ride Map Collection 6: France & Old West"
#> [135] "Ticket to Ride: Europa 1912"                       
#> [136] "Ticket to Ride: Europe"                            
#> [137] "Tides of Time"                                     
#> [138] "Troika"                                            
#> [139] "Tsuro of the Seas"                                 
#> [140] "Uncharted: The Board Game"                         
#> [141] "UNO"                                               
#> [142] "The Vale of Eternity"                              
#> [143] "Valley of the Kings"                               
#> [144] "Vegetable Stock"                                   
#> [145] "Villagers"                                         
#> [146] "Welcome Back to the Dungeon"                       
#> [147] "Werewords Deluxe Edition"                          
#> [148] "Why First?"                                        
#> [149] "Wildlands"                                         
#> [150] "Zany Penguins"

# get supplementary details of the first 10 games
games_details <- thing(collection_items_owned$item_id[1:10])

# observe details of the first games
glimpse(games_details[1,])
#> Rows: 1
#> Columns: 26
#> $ name               <chr> "12 Days"
#> $ year_published     <chr> "2011"
#> $ description        <chr> "The holiday-themed 12 Days takes the familiar &quo…
#> $ min_players        <chr> "3"
#> $ max_players        <chr> "5"
#> $ min_playtime       <chr> "15"
#> $ max_playtime       <chr> "15"
#> $ playing_time       <chr> "15"
#> $ min_age            <chr> "8"
#> $ category           <list> <"Card Game", "Print & Play">
#> $ mechanic           <list> <"Auction / Bidding", "Set Collection">
#> $ family             <list> <"Crowdfunding: Kickstarter", "Holidays: Christmas"…
#> $ designer           <list> <"James Ernest", "Mike Selinker">
#> $ artist             <list> "Echo Chernik"
#> $ publisher          <list> <"Calliope Games", "Cheapass Games", "Gamesmith, LL…
#> $ rating_users_rated <chr> "1561"
#> $ rating_avg         <chr> "6.52864"
#> $ rating_bayes_avg   <chr> "5.92165"
#> $ rank               <chr> "3468"
#> $ owned              <chr> "3527"
#> $ trading            <chr> "72"
#> $ wanting            <chr> "36"
#> $ wishing            <chr> "283"
#> $ num_comments       <chr> "458"
#> $ num_weights        <chr> "67"
#> $ avg_weight         <chr> "1.209"
```

This is an example that shows to retrieve the current top 10 board games
ids as per [BGG’s rankings](a)

``` r
source('common_funcs.R')

# top 10 ranked games on BGG
top_K_games_ids <- top_k_games_ids(k = 10)

# get details for each item in user's collection
games_details <- thing(top_K_games_ids)
games_details %>% 
    select(name, year_published, designer, rating_avg) %>% 
    mutate(designer = map_chr(designer, paste, collapse = ', ')) %>% 
    print()
#> # A tibble: 10 × 4
#>    name                              year_published designer          rating_avg
#>    <chr>                             <chr>          <chr>             <chr>     
#>  1 Brass: Birmingham                 2018           Gavan Brown, Mat… 8.58637   
#>  2 Pandemic Legacy: Season 1         2015           Rob Daviau, Matt… 8.52337   
#>  3 Ark Nova                          2021           Mathias Wigge     8.53371   
#>  4 Gloomhaven                        2017           Isaac Childres    8.57667   
#>  5 Twilight Imperium: Fourth Edition 2017           Dane Beltrami, C… 8.58874   
#>  6 Dune: Imperium                    2020           Paul Dennen       8.42987   
#>  7 Terraforming Mars                 2016           Jacob Fryxelius   8.3533    
#>  8 War of the Ring: Second Edition   2011           Roberto Di Megli… 8.54874   
#>  9 Star Wars: Rebellion              2016           Corey Konieczka   8.41853   
#> 10 Gloomhaven: Jaws of the Lion      2020           Isaac Childres    8.41513
```

<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
