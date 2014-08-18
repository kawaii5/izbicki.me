---
title: HLearn cross-validates >400x faster than Weka
---

<img class=right src=/img/uploads/2013/05/weka-lambda-haskell-300x150.png>

[Weka](http://www.cs.waikato.ac.nz/~ml/weka/) is one of the most popular tools for data analysis.  But Weka takes **70 minutes** to perform leave-one-out cross-validate using a simple [naive bayes classifier](https://en.wikipedia.org/wiki/Naive_Bayes_classifier) on the [census income](http://archive.ics.uci.edu/ml/datasets/Census-Income+(KDD)) data set, whereas Haskell's [HLearn](https://github.com/mikeizbicki/HLearn) library only takes **9 seconds**.  Weka is 465x slower!

**Code and instructions for reproducing these experiments are [available on github](https://github.com/mikeizbicki/HLearn/tree/master/HLearn-classification/src/examples/weka-cv#readme).**

Why is HLearn so much faster?

Well, it turns out that the bayesian classifier has the algebraic structure of a [monoid](https://en.wikipedia.org/wiki/Monoid), a [group](https://en.wikipedia.org/wiki/Abelian_group), and a [vector space](https://en.wikipedia.org/wiki/Vector_space).  HLearn uses a new cross-validation algorithm that can exploit these algebraic structures.  The standard algorithm runs in time $\Theta(kn)$, where $k$ is the number of "folds" and $n$ is the number of data points.  The algebraic algorithms, however, run in time $\Theta(n)$.  In other words, it doesn't matter how many folds we do, the run time is constant!  And not only are we faster, but we get the _exact same answer_.  Algebraic cross-validation is not an approximation, it's just fast.

Here's some run times for k-fold cross-validation on the census income data set.  Notice that HLearn's run time is constant as we add more folds.

<center>
![k-fold-cross-validation-weka](/img/uploads/2013/05/k-fold-cross-validation-weka1.png)
</center>

And when we set k=n, we have leave-one-out cross-validation.  Notice that Weka's cross-validation has quadratic run time, whereas HLearn has linear run time.

<center>
![leave-one-out-fast-cross-validation-weka](/img/uploads/2013/05/leave-one-out-fast-cross-validation-weka1.png)
</center>

HLearn certainly isn't going to replace Weka any time soon, but it's got a number of cool tricks like this going on inside.  If you want to read more, you should check out these two recent papers:

  * (ICML13) [Algebraic Classifiers: a generic approach to fast cross-validation, online training, and parallel training](http://izbicki.me/public/papers/icml2013-algebraic-classifiers.pdf)

  * (TFP13) [HLearn: a machine learning library for Haskell](http://izbicki.me/public/papers/tfp2013-hlearn-a-machine-learning-library-for-haskell.pdf)

I'll continue to write more about these tricks in future blog posts.
