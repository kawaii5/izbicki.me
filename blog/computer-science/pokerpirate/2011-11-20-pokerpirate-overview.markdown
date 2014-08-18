---
title: PokerPirate - Overview
---

<img class=right src=/img/uploads/2011/11/robot_pirate_cards.png>

In 2005-2006 I developed PokerPirate, a pokerbot. Pokerbots are software that play online poker without human interaction. PokerPirate successfully beat single table, no limit, Texas Hold'em tournaments. These are better known as sit-and-go's. PokerPirate played the $5 tournaments on Royal Vegas Poker. After recouping my losses from the development process, I decided to turn the bot off. I am now releasing the source code and using it as a case study in artificial intelligence and software engineering.

Poker presents a particular challenge to the AI developer. Much like the real world, poker hands can have millions of variations, there is a lot of unknown information, and a lot of human interaction. AI techniques capable of winning at poker would be a significant advance because they could be applied to many problems that are currently unsolved.

A successful pokerbot, however, is intrinsically interesting apart from any potential advancement in AI. For example, it could provide the owner with an effort-free source of income. The AI in PokerPirate does not use any advanced techniques. Instead, I have carefully selected a game that simple techniques would be effective at beating. Most pokerbots specialize in limit table games, but PokerPirate specializes in no-limit sit-and-go tournaments. I don't think there are many bots competing in these games yet.

### Results

PokerPirate was play tested on the .75+.25 sit-and-go's on RVP.  That means that everyone would play one dollar to play, the house would take 25 cents, and 75 cents would go into the pot. PokerPirate played over 5000 tournaments during this time. The AI would be revised, then a small test would be run, and the cycle would repeat. The AI never had a positive ROI at this level because the house's take was so high. A final test of 1000 games was run in which the AI would have had about a 10% ROI if the house take was reduced to 10% vice 33%.

I felt the AI was ready after this test to be moved to the 5+.50 games. These were the lowest stakes games offering only a 10% house take. Over the course of 2000 games, PokerPirate covered the development costs of about $1000 and had an ROI of about 10%.

_I decided stop running PokerPirate after I had recouped my losses.  I now believe that online gambling is a net loss to society, and I do not want to be a part of it in any fashion._  I'm continuing to make these articles available because people enjoy them.  

### Outline

The links below describe the development of the bot. A working knowledge of C++ will be required to understand the AI engine. Each page builds on the discussion of those previous, so I recommend reading them in order.

[Exploiting the sit-and-go game](http://izbicki.me/blog/exploiting-the-sit-and-go-game)

  * Pokerbot styles
	
  * The Sit-and-go tournament
	
  * Exploiting the game
	
  * How to measure success

[PokerPirate operation](http://izbicki.me/blog/pokerpirate-operation)
	
  * "play" mode
	
  * "watch" mode
	
  * "debug" mode
	
  * "proc" mode
	
  * "resetDB" mode

[PokerPirate's architecture and code](http://izbicki.me/blog/pokerpirates-architecture-and-code)

	
  * How to interface with the poker client

	
  * Explaining the makePlay() function

	
  * Challenges associated with Royal Vegas Poker

	
  * Staying below the radar


[The artificial intelligence](http://izbicki.me/blog/the-artificial-intelligence)



	
  * Explaining the getPlay() function

	
  * Why heads up play is easy

	
  * Pre-flop play

	
  * Post-flop play


[Download and run](http://izbicki.me/blog/download-and-run)



	
  * Reading the code

	
  * Launching the program in "debug" mode



