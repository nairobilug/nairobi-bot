# Nairobilug irc bot

A rewrite of the nairobilug irc bot in haskell.

#### Main deps:
* [simpleIrc](https://hackage.haskell.org/package/simpleirc)
* [auto](https://hackage.haskell.org/package/auto)


#### Usage:
* seen `@seen <nick>`
* reputation
  - Check reputation `@rep <nick>`
  - Add reputation `@addRep <nick>`
  - Subtract reputation `@subRep <nick>`
* last fm `@np <(optional): lastfm username>` *You should pass your last fm username if your last fm username is different from your irc nick.*
* define `@define <word>` 
* echo `@echo <sentence you want to echo>`
* wolfram alpha `@wa <search query>`

###### Maybe later if need be:
* google/duckduckgo `@search <search query>`
* wikipedia `@wiki <search query>`


#### Reasons:
* Main reason is because I can ;D.
* I want to play with the auto library and discrete events in haskell.
* I want to write a bot without going through a bot building framework.


#### Deploying
* stack build
