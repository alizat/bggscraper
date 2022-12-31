# The `bggscraper` library
`bggscraper` is an R package that scrapes all sorts of (publicly accessible) data from the [Board Game Geek](boardgamegeek.com) website. It depends heavily on the [BGG XML API2](https://boardgamegeek.com/wiki/page/BGG_XML_API2) library.

***Note that this is a work in progress. However, I intend to make it useful and usable soon (by end of March, 2023).***

Using this library, you may scrape the following info (among others):

* list of [top 5000 board games](https://boardgamegeek.com/search/boardgame?advsearch=1&q=&sort=rank) as per rankings on BGG.
* details on board games and board game expansions (i.e. categories, mechanics, designers, etc.)
* BGG users' games collections, including what they own as well as what they hope to play or buy in the future.
* Info on users themselves.
* list of games are hot nowadays.
* Information on plays of a certain game for one or more users.
* Geeklists, forum lists, forums, threads, guilds pertaining to a certain board game id.
