---
title: Download and run
---

Download the source code [here](http://izbicki.me/public/pokerpirate/poker.zip). It is released under the BSD license. If you find it useful or do anything cool with it, I'd appreciate a heads up.

Other stuff you'll need:
	
  * Microsoft Visual C++ Express Edition, available [here](http://www.microsoft.com/express/Windows/)

	
  * Royal Vegas Poker software and account, available [here](http://www.royalvegaspoker.com/) (You must have an account to log on to the software and observe a game. You do not need to have money in the account, unless you want to actually have PokerPirate play.)

	
  * (optional) VMWare player, available [here](http://www.vmware.com/)

	
  * You will need a copy of Windows to run inside VMWare.


Optional stuff for the database analysis:

	
  * Download my sample database [here](http://www.robotpoker.org/PokerPirate/download.html) (currently unavailable)

	
  * Download my analysis tool [here](http://www.robotpoker.org/PokerPirate/download.html) (currently unavailable)

	
  * MySQL

	
  * Any web server with php support, for example Apache


The original setup was on an AMD Athlon 1800+ computer running linux. I used VMWare player to run a Windows XP virtual machine. On the virtual machine I ran only Visual C++, RVP, and my own code. It worked out pretty well and I recommend that setup to anyone who is considering a serious pokerbot.


### Reading the code


I apologize for the poor quality of coding in places. I never thought I would release it to the public. Anyways, my recommendation is to completely understand what I wrote in the AI section. Once you've done that, start reading at main(). Most of the functions and variables have descriptive names. If you don't understand what a function does or how it does it, then right click -> view implementation. Good luck! [Send me an email](mailto:robotpoker@robotpoker.org) if you want some help.


### Launching the program in "debug" mode


When you start PokerPirate, it automatically scans your open windows to detect which ones belong to Royal Vegas Poker. In debug mode, it will automatically connect to the first table it finds. Therefore, first start RVP and open one game table.

Starting RVP...

<centeR>
![](/img/uploads/2011/11/loadstep1.jpg)
</centeR>

Select and open a sit-and-go table...

NOTE: _Make sure the table is for 10 players, with the appropriate blind structures. This is not required, but it is what the AI is designed for._

<center>
![](/img/uploads/2011/11/loadstep2.jpg)
</center>

An empty table is opened...

<center>
![](/img/uploads/2011/11/loadstep3.jpg)
</center>

NOTE: _PokerPirate determines the game's state from the information in the chat panel. It assumes that, initially, all players started with 1000 chips. Every time a bet, raise, call, or all-in is recorded in the chat box it deducts the appropriate number of chips from that player. Every time a player wins a hand, it adds those chips. The advantage of this is that no OCR techniques are required to determine a player's chip counts. A simple call to GetWindowText() returns the information in text format, which is much easier to parse. The disadvantage is that the entire game's status must be displayed in the window. Therefore, it is not possible to begin analyzing a game half way through. You must have the game open when it starts._

In order to launch PokerPirate, you will have to pass the command line option "debug" to the program. The easiest way to do this is to create a shortcut on your desktop (or 5 like I did)...

<center>
![](/img/uploads/2011/11/loadstep4.jpg)
</centeR>

NOTE: _PokerPirate will not be able to connect to a window unless it knows what your alias is! (Notice how your name appears in the window's title.) Edit the pp.conf file, and change the "Moniker" value to be what yours actually is._

The program is now running...

<centeR>
![](/img/uploads/2011/11/table.jpg)

![](/img/uploads/2011/11/pp-default.jpg)
</center>

By default, the program will not play for you. This will let you experiment with the code without the risk of losing money. If you want it to play, simply update pp.conf and set "Observer" to "false". Be forewarned that PokerPirate will take over your mouse, so you won't be able to surf the web or anything while this is happening. Good luck!
