---
title: Introduction to data mining images
---

Image processing is one of those things people are still much better at than computers.  Take this set of cats:

<center>
![](/img/uploads/2011/10/cats.png)
</center>

Just at a glance, you can easily tell the difference between the cartoon animals and the photographs.  You can tell that the hearts in the top left probably don't belong, and that Odie is tackling Garfield in the top right.  The human brain does this really well on small datasets.

But what if we had thousands, millions, or even billions of images?  Could we make an image search engine, where I give it a picture of an animal and it says what type it is?  Could we make it automatically find patterns that people miss?

Yes!  This post is the beginning of a series about how.  Finding patterns in large databases of images is still an active research area, and these posts will hopefully make those results more accessible.  The current research still isn't perfect, but it's probably much better than you'd guess.

###The "black box" framework

There are three basic steps in data mining images:

STEP 1: Create the "black box"

STEP 2: Cluster

STEP 3: Run queries

That's it!

... well ... sort of ...

There are many different algorithms that can be used at each step.  Which ones you decide to use will depend on the type of information you're mining from the images.  The rest of this post gives a high level overview of how each of these steps works, and later posts will focus on specific implementations for each step.

###STEP 1: Creating the black box

The black box defines the "distance" between two images.  The smaller the distance, the more similar the images are.  For example:

![](/img/uploads/2011/10/garfield1.png)Garfield is very similar to himself, that's why Box A gives him a low score--nearly zero.  Odie is not very similar to Garfield, but he's a lot closer than a palm tree.  The specific numbers outputted don't matter.  All that matters is the ordering created by those numbers.  In this case:

![](/img/uploads/2011/10/ordering1.png)Of course, if we compare against a different image, we will probably get a different ordering.

Likewise, we can get different orderings with a different black box.  Let's imagine that Box A was designed to determine if two pictures are of the same type of animal.  If we test it on some new input, we might get:

![](/img/uploads/2011/10/boxA.png)Notice that Box A thinks the real cat is more similar to Garfield than Odie is.  Now let's consider another black box.  Imagine Box B is designed to see if two images were drawn in a similar style.  Box B might give the following:

![](/img/uploads/2011/10/boxB.png)

Box B gives the opposite results of box A.

Creating a good black box is the hardest part of data mining images.  Most research is dedicated to this area, and most of this series will be focused on evaluating the performance of different black boxes.  Which ones are good depends on your dataset and what information you're trying to extract.  Some general categories of black boxes we'll look at are:
	
  1. Histogram analysis (a simple technique that can be surprisingly effective on colored input)

	
  2. Converting images into a time series (for analyzing the shapes of rigid objects, e.g. fruit)

	
  3. Creating shock graphs (for analyzing the shapes of non-rigid objects, e.g. animals)

	
  4. Komolgorov comlexity of the images (for comparing an image's textures)


But first, let's take a closer look at what makes a black box good.

###Properties of a good black box


There are two more aspects of black boxes we must look at.  First, every black box will be _sensitive_ to certain features of an image and _invariant_ to others.  In the examples below, Box C is sensitive to shape, but invariant to color.  Box D is sensitive to color, but invariant to shape.

![](/img/uploads/2011/10/boxC.png)

![](/img/uploads/2011/10/boxD1.png)

Most black box algorithms contain both sensitivities and invariances.  These are the properties you will use to decide which black box is best for your application.

Second, a black box is a [metric](http://en.wikipedia.org/wiki/Metric_%28mathematics%29) and as such must satisfy four criteria:



	
  1. _distance_(_x_, _y_) ≥ 0     (_[non-negativity](http://en.wikipedia.org/wiki/Non-negative)_)

	
  2. __distance__(_x_, _y_) = 0   if and only if   _x_ = _y_     (_[identity of indiscernibles](http://en.wikipedia.org/wiki/Identity_of_indiscernibles)_)

	
  3. __distance__(_x_, _y_) = __distance__(_y_, _x_)     (_[symmetry](http://en.wikipedia.org/wiki/Symmetric_relation)_)

	
  4. __distance__(_x_, _z_) ≤ __distance__(_x_, _y_) + __distance__(_y_, _z_)     (_[subadditivity](http://en.wikipedia.org/wiki/Subadditivity)_ / _[triangle inequality](http://en.wikipedia.org/wiki/Triangle_inequality)_).


If you don't understand these criteria, don't worry too much.  All the black boxes we'll look at in the rest of this series will satisfy these criteria automatically.

###STEP 2: Cluster the images

[Clustering](http://en.wikipedia.org/wiki/Cluster_analysis) is much easier than designing the black box.  Clustering algorithms are used in many fields, so they have received much more attention.  Some clustering algorithms commonly used are: [K-means](http://en.wikipedia.org/wiki/K-means_clustering) and [Hierarchical clustering](http://en.wikipedia.org/wiki/Hierarchical_clustering) (i.e. [Dendrograms](http://en.wikipedia.org/wiki/Dendrogram)).

There are many more as well.  In general, you can use whatever clustering algorithm you want.  When developing an application, most people will try several and pick whichever one happens to work the best for their data.

Here's an example clustering of our cat data using Black Box A (i.e. by what the picture is of):

<center>
![](/img/uploads/2011/10/cats_cluster1.png)
</center>

We've created three clusters.  The red cluster contains hearts, the white cluster contains cats, and the blue cluster is an anomaly.  It contains both a cat and a dog, and there is no easy way to separate them.  If we had used a hierarchical classifier, the "contains cats and dogs cluster" might be a sub-cluster of the "contains cats cluster."


Here's the same data clustered using Black Box B (i.e. by how the picture is drawn):

<center>
![](/img/uploads/2011/10/cats_cluster2.png)
</center>

Now we have only two clusters: the white cluster contains cartoons, and the red cluster contains photographs.

One last note.  Most of the CPU work gets done during this step.  On large datasets, clustering can take hours to months depending on the algorithm and the speed of the black box.  There are many tricks for speeding up clustering, which will take a look at in later posts.

###STEP 3: Run your query

Queries are fairly easy once the ground work is set up with the black box and clustering. Sometimes, all you want to know is how STEP 2 clustered your input.  For example, you could query "how many types of animals are in this dataset?"  The answer would just be the number of clusters using Box A.  Typically, however, your query we will supply the database with an image and find similar images.

<center>
![](/img/uploads/2011/10/query1.png)
</center>

If we've done steps 1 and 2 well, this should take only seconds even when the database contains millions of images.  Of course, it's not always possible to do steps 1 and 2 well enough to make this happen.  Later posts may cover some new techniques for speeding up the querying process.

###The Rest of the Series

So far, we've seen that the black box framework for image datamining is very simple.  The tricky part is putting the right algorithm in each step.  In the rest of the series, we'll look at a few different black boxes, and show how to efficiently combine them with a clustering algorithm.  The different types of black boxes are the most interesting part of image mining, so we will focus on that first.
