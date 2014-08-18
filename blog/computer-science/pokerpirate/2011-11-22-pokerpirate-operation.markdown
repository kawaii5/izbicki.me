---
title: PokerPirate's operation
---

Before talking about how the AI actually works, I think it would be useful to talk about the program's architecture. This section is mostly relevant from a software engineering perspective and can easily be skipped.

I run Linux on my desktop. In order to run the poker software, I had to load a copy of Windows XP inside a VMWare virtual machine. I did the development and testing in this virtual machine.

PokerPirate can be run in any of 5 modes:



	
  * Play

	
  * Watch

	
  * Debug

	
  * Proc

	
  * ResetDB


This section describes each mode, and the download section describes how to get the program running in each.


### "Play" mode


This is the default mode, and the way PokerPirate was meant to be operated. This is what my screen looks like when running:

<center>
![](/img/uploads/2011/11/pp-play-all.jpg)
</center>

Around the edges is my linux desktop. We're interested, however, in the VMWare window that occupies most of the space. Inside that are 4 RVP tables, PokerPirate, and Microsoft Visual C++. This screenshot is typical for what the program looks like in play mode. Unfortunately, at the time of the screen shot, no one was seated at any of the tables. Apparently, Royal Vegas Poker is not as popular as it used to be.

This is what the PokerPirate console looks like:

<center>
![](/img/uploads/2011/11/pp-play-console.jpg)
</center>

PokerPirate is just a console program, and an ugly one at that. The only purpose of the window is to provide some debugging and status information. All the displayed information is simply the default for an empty screen.

When PokerPirate is running, in any mode, it is important to note that the computer is unusable. PokerPirate is constantly changing which window is in the foreground so it can read new information, and constantly moving the mouse to make the plays. This makes it difficult to follow each game, so the PokerPirate console displays the status of each game in red. At the bottom is space for the AI to output its debug information.

Upon starting, the program checks the file pp.conf. This file controls everything about the program's operation. Most notably, it controls which player the AI will act for ("Moniker"), and what times of day the AI is allowed to play for ("TimeStart" and "TimeEnd"). I ran three accounts on Royal Vegas Poker, each with their own set of times they were allowed to play. This made it appear that someone was just logging in and playing as though it were their "job."

If there are already RVP tables open when PokerPirate starts, it will take control of them. If there are none open, it will automatically start RVP, join 5 tables, and begin play. 5 tables is the maximum number of tables that RVP will allow a single account to sit down at. This is another reason I operated 3 RVP accounts. Each account had its own virtual machine. Inside the virtual machine would be PokerPirate playing a different account on 5 separate tables. Each account was given specific tables it was allowed to play at, so no two accounts would play at the same table.


### "Watch" mode


Watch mode functions exactly like play mode, except the AI won't automatically seat you at a table. If you're already seated, however, it will play for you so you don't just fold every hand. Because it still records the games, it is useful for building a large dataset of games for future analysis in proc mode.

The screenshots above are actually from watch mode. Because I'm making this page long after I stopped using the program actively, I no longer have any money in my accounts and can't play. Notice how it says watch in the PokerPirate title bar, and Allow new games (56): 0. The 0 means do not sit down at new games. The 56 is the total number of games it has observed since being reset. This is tracked in gameid.txt and incremented after each completed game.


### "Debug" mode


As the name suggests, I used debug mode to develop PokerPirate and not for actually playing to make money.

With the game like:

<center>
![](/img/uploads/2011/11/table.jpg)
</center>

PokerPirate will display:

<center>
![](/img/uploads/2011/11/pp-default.jpg)
</center>

This screen displays a lot more information about the table, because PokerPirate is only playing on one table. It's basically just spitting out all the information it knows about the game's status. I used this screen to, you guessed it, debug the program. It helped debug interfacing with the RVP software in two ways, and it helped in the early stages of AI development.

First, reading the cards. The only way to read the cards is to take a capture of the screen. Fortunately, the cards appear in the same locations every time, so you simply take that region and compare it to all 52 cards and see which one matches. The row called "cards" displays the community cards, and whatever players' cards are revealed will show up next to that player. Unfortunately, the new version has gotten fancy. They now move the card positions around when people win, to highlight what the winning hand was. They also dim the non-winning cards. This all interferes with my screen reading routines. This should be fairly straight forward to fix if anyone has the motivation.

Second, reading the chip counts. Reading the chip counts is much easier than reading the cards. As I mentioned in the note above, all you have to do is use the GetWindowText() function built into Windows to get the text on the side of the screen. Then, parse that text to find out how much everyone has bet/won/lost. Notice how the stack sizes are all integers. Originally, I designed the game to be compatible with only integer stacks, because that is how RVP was originally built. They have since changed their code. Now, when a pot with an odd dollar amount is split the "change" is split between the winners. My code can't handle this. These decimals are so small compared to the stack sizes, however, that it is probably not a big deal strategy-wise. The error message at the top has occurred because if you add up all the player's stacks and the pot, you no longer get the 10000 like you're supposed to.

Finally, displaying the AI. Notice all the stuff on the right. This was an early means for me to output the AI's decision process. I have no idea what any of it means anymore. (Hint for developers: comment your code!) At the very bottom, I output what the AI's recommended move is. In the latter stages of AI development, this screen was no longer useful, because "interesting" hands would only come along about once a game. I was faster to have many games running in play mode to get the interesting hands to appear more frequently.


### "Proc" mode


In proc mode, the program uses the game recordings from play mode to compile information about the players. Originally, my idea was to compile a database of every player, to determine their unique playing styles and exploit them. This was found not to be necessary. Additionally, it was excessively difficult. After playing thousands of games, I realized that I played with the same people only occasionally. It was, however, useful in characterizing the relative difficulty between blinds. Based on this data, I determined that the level of play was roughly the same from .75+.25 level until the 5.00+.50 level. The next blinds level, 10.00+1.00 provided significant increase in player skill. I stopped work on the program before I was able to beat the 10+1 game, however, I believe it would be possible with only a little work.

Proc mode requires access to a MySQL database where all this information is stored. I also had a series of php scripts to go through and analyze the database. My current desktop does not have these available, so I've disabled proc mode in the source code. Analyzed results can be viewed in the results section.


### "ResetDB" mode


This clears the database. It is required to be run before running proc mode again to update the database with new information.
