---
title: PokerPirate's architecture
---

PokerPirate has two main components: an interface with the Royal Vegas Poker software and the AI. This section describes how these components relate.<!-- more -->


### How the AI Interfaced with RVP


This flowchart (explained below) shows the overall program execution flow.

<center>
![PokerPirate architecture flowchart](/img/uploads/2011/11/flow-play.gif)
</center>

The red boxes represent windows opened by the Royal Vegas Poker software. The black and green boxes represent code inside the PokerPirate program. PokerPirate has only one console window.

The black boxes represent the "heart" of the program and are used in all of the program's modes. The MsgQueue class is what controls all the interaction between PokerPirate and RVP. It reads the cards off the screen and calculates players' stack sizes. When playing, it controls the mouse and keyboard inputs to perform bets, raise, etc. The TexasHoldemAIManual class contains the actual AI code. Looking over the header for the MsgQueue class (MsgQueue.h) will give you a good understanding of how the interface actually works.

The green boxes represent the program's flow path when in the "play" mode. Notice how it is able to have an arbitrary number of open tables. The RVP software, however, was limited to five tables at a time.

The actual update loop for "play" mode looks like this:

``` C
/*
bring the table to the foreground;
start a timer to see how long AI engine takes
*/
tables[i].loadTable();
/*
read the table to see if anything has changed
*/
tables[i].update();
/*
print the table's status;
display type ("play"/"debug"/etc) controlled by
tables[i].setPrintType() above
*/
cur (0, 4+i);
tables[i].print();
/*
ask the AI for its move;
will actually make the move if tables[i].allowPlay==true
*/
tables[i].getAI()->makePlay();
/*
stop AI engine timer
*/
tables[i].closeTable();
```

### Explaining makePlay()


makePlay() is the interface between the AI and the MsgQueue. It calls the function getPlay(), which is where the AI is located. getPlay() will return an ActionRet struct.

``` C
struct ActionRet
{
    string name,sub;
    int action,amt;
};
```

The strings are only for output purposes. Amt is the amount to bet or raise if applicaple. Action can be any of the following:
 
``` C
#define FOLD                                'f'
#define CHECK                               'p'
#define CALL                                'c'
#define ALLIN                               'a'
#define BET                                 'b'
#define RAISE                               'r'
```

Once it knows what action to make, it calls the appropriate MsgQueue function (DoBet which also handles raises and all-ins, DoCall, DoCheck, and DoFold). These in turn use MsgQueue's Keyboard and MouseClick functions to send the play to the RVP window.


### Challenges associated with interfacing with RVP


The interface was surprisingly simple to build. The only challenge I came accross was when RVP updated their software. This would occur approximately monthly. These updates would sometimes move card or button positions very slightly. This would cause PokerPirate to read cards incorrectly, or not to be able to make plays.

A lot of my losses during development of the program occurred when I just let it play unobserved over a weekend as a test of its robustness. Luckily it was only playing one table. Basically, RVP moved their button positions for betting/calling/etc down about 20 pixels, and PokerPirate was no longer able to make any plays. It went the whole weekend just timing-out and folding every hand.

If you read through some of the table connecting code, it will look very messy. This is a result of constant changes trying to keep up with the pace of updates. Now, RVP has changed its software over to PokerTime. If anyone wants to start using PokerPirate again, they will have to completely update PokerPirate's ability to find cards and buttons. In practice, this means finding the x and y coordinates and updating the variables, which sounds easy but takes several hours of tedious work.


### Comments on PokerPirate's stealth


I thought the casinos would not want to have bots playing on their networks. My biggest fear was that they would seize my assets and I would be out several thousand dollars. Therefore I took some measures to cover my trail.

I didn't think I had to fear that casinos would be using software to monitor my computer for bots. Because my bot was custom, it would be relatively difficult to detect. There are two ways I could see me being detected. The first is that an admin might try to talk to me over the chat system. I could not think of a way to counter this, but considered it to be unlikely. In retrospect I am lucky this didn't happen with the weekend-timeout story above. The second would be monitoring for artificial keyboard/mouse inputs. This would be possible if the software somehow monitored calls to the SendInput() function that I was using to do this. I do not know much about Windows, but believed this would require administrator-like privileges which I did not give my user. Ultimately, however, I believed this to be an insignificant threat.

My greatest counterdetection threat was due to being incorrectly identified as using collusion (a form of cheating where two people at the same table share information). Poker rooms seem to take this much more seriously than pokerbots. This was a concern because I was using multiple accounts to get the table coverage I wanted. If two of these accounts from the same IP address logged on to the same table, the poker room would flag them even though they weren't colluding. Even if this weren't the case, I wouldn't want my bots playing against each other. Therefore, I took three counter-collusion-detector measures. First, I limited each account to be able to play at only certain tables so they would never overlap. Second, each bot's virtual machine had a unique IP address. Third, money was deposited and withdrawn from each poker account using different banking information (all of which was legal).

Based on the current proliferation of pokerbots on the net, it doesn't seem like anyone is too concerned about the matter. I am guessing this is because most of these bots are not successful. If it became widely known that there was a successful bot operating on a casino, I guess that would hurt the casino's business and they would take measures to shut it down.
