---
title: it's easy to cheat at settlers of cataan by loading the dice
---

[Settlers of Cataan](http://www.amazon.com/dp/B005BM07P0) is a great board game... but I hate losing at it... so I figured out a way to cheat.  In the game, dice are used to determine which resource cards you collect.  For example, if the number nine is rolled, and you have a settlement next to a nine, you'll get some resources.  Obviously, more resources helps you win.  So if we load the dice, we can control which numbers come up more frequently, helping us win more often.  This post describes how to physically load the dice and how to do statistical tests to prove we'll win more often.

![]()

In a previous post, we saw that it's [pretty hard to make a loaded coin](/blog/how-to-create-an-unfair-coin-and-prove-it-with-math.html).  But making loaded dice is actually pretty easy.  I decided to make make the number six come up more often than every other number.  To do this, we'll add some weight to the opposite side as the six.  Since the numbers on opposite sides of dice always add to seven, we'll have to add weight to the one side.  When we roll the dice, the weight will pull the one down towards the table, pushing the six to the top.

Amazon sells [preweighted dice](http://www.amazon.com/dp/B008Z23BOM) that you can buy.  But it's really easy to weight the dice that come with Settlers of Cataan yourself.  The dice are made out of wood; and if we set the dice in water, the wood will absorb the water.  The water weighs down that side of the dice.  

The picture below shows the two settlers dice in a small dish.  If you look closely, you can see there is water covering just the bottom of the dice:

![settlers dice sitting in water to get biased and weighted](/img/dice/in-water.jpg)

My beautiful wife Kristen rolled the dice for me and I recorded the results.  We used a very scientific method to determine how many rolls to do: we stopped when Kristen got too bored!

It certainly looks like we're getting a lot more sixes.  But how can we be sure this isn't just random variations?  And if the dice really are loaded, how much will this affect our game of Settlers?  To answer these questions, we'll have to do some math.

### now for the math

Dice rolling is a [multinomial process](http://en.wikipedia.org/wiki/Multinomial_distribution).  This just means that every trial can have a limited number of outcomes and is independent of every other trial.  For us, each dice roll is a trial; there's only six outcomes; and each roll is unaffected by the rolls that happened just before and after it.  We want to know what's the probability that we'll roll a given number on the dice.  The obvious way to calculate this probability that number $i$ is rolled is to divide the number of times we rolled $i$ by the number of times we rolled the dice.  Unfortunately, this doesn't give us a good idea bout how accurate our estimate is.

Enter the [dirichlet distribution](http://en.wikipedia.org/wiki/Dirichlet_distribution).  This is a distribution over the bias of a multinomial process.  In other words, it gives us the probability that a probability is what we think it should be.  That's a convoluted definition!  Some examples should make it clearer.

The dirichlet distribution takes a single parameter $\alpha$, but $\alpha$ is a vector with one entry for every outcome of our trials.  The value 

One of the most challenging tasks in statistics is visualizing multivariate distributions.  We'll use a technique called [dimensionality reduction]().  The basic idea is that we'll use some prior information that we have to reduce our high-dimensional distribution down to just two or three dimensions.  Then plot the result.  In our case, we suspect that the bias on sides 2, 3, 4, and 5 of the dice should be the same.  Therefore, we're going to lump all of these results into a single dimension.  In effect, there's only three dice rolls that we care about: 1, 6, and everything else.

Three dimensional distributions are conveniently plotted using [barycentric charts](http://en.wikipedia.org/wiki/Barycentric_coordinate_system).

