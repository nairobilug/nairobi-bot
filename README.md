# Nairobilug IRC bot
[Nairobilug](https://github.com/nairobilug/) IRC bot in haskell taking advantage of discrete events (some will call it FRP) in haskell.

[![Build Status](https://travis-ci.org/nairobilug/nairobi-bot.svg?branch=master)](https://travis-ci.org/nairobilug/nairobi-bot)
[![Hackage](https://img.shields.io/hackage/v/nairobi-bot.svg?maxAge=2592000)](https://hackage.haskell.org/package/nairobi-bot)


#### Usage
* seen `@seen <nick>`
* reputation
  - Check reputation `@rep <nick>`
  - Add reputation `+1 <nick>`
  - Subtract reputation `-1 <nick>`
* last fm
  *Setting a new username for your nick replaces the older one.*
  - `@np set <username>` to associate a last.fm username with your nick.
  - `@np <last.fm username>` If you don't want to set your last fm username.
  - `@np` If your IRC nick is your last.fm username or if you have set your last.fm username.
* define `@define <word/phrase>`
* echo `@echo <sentence you want to echo>`
* wolfram alpha `@wa <search query>`
* URLs *Automatically fetches page titles from URLs.*
* Help `@help` — there are too many commands to paste the help in a channel so we give the user a URL to the wiki: https://github.com/nairobilug/nairobi-bot/wiki#usage

###### Maybe later if need be
* google/duckduckgo `@search <search query>`
* wikipedia `@wiki <search query>`


#### Reasons
* Main reason is because I can ;D.
* I want to play with the auto library and discrete events in haskell.
* I want to write a bot without going through a bot building framework.
* Much thanks to [Justin Lee](https://github.com/mstksg) for creating the [auto](https://github.com/mstksg/auto) library.

#### Tests
I haven't yet written tests for it but they are coming.


#### Main deps
* [simpleIrc](https://hackage.haskell.org/package/simpleirc)
* [auto](https://hackage.haskell.org/package/auto)


#### Deploying
Build and run the nairobi-bot executable like any other binary.  
Preferably using stack:
* `stack build`   *to compile but not add it to path*
* `stack install` *to add nairbibot-exe to path*
* `./.stack-work/install/x86_64-linux/lts-3.15/7.10.2/bin/nairobi-bot-exe`

In production use the following [answer from stackoverflow](http://stackoverflow.com/questions/4797050/how-to-run-process-as-background-and-never-die) to run your executable.
* `nohup .stack-work/install/x86_64-linux/lts-3.15/7.10.2/bin/nairobi-bot-exe > /dev/null 2>&1 &`

1. nohup means: Do not terminate this process even when the stty is cut off.
2. > /dev/null means: stdout goes to /dev/null (which is a dummy device that does not record any output).
3. 2>&1 means: stderr also goes to the stdout (which is already redirected to /dev/null). You may replace &1 with a file path to keep a log of errors, e.g.: 2>/tmp/myLog
4. & at the end means: run this command as a background task.

#### License
BSD3.  
See [LICENSE](https://github.com/nairobilug/nairobi-bot/blob/master/LICENSE) file for complete license.

#### Other/RTFM
See the [Wiki](https://github.com/nairobilug/nairobi-bot/wiki)
