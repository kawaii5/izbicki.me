---
title: HLearn's code is shorter and clearer than Weka's
---

<img class=right src=/img/uploads/2013/05/weka-lambda-haskell-300x150.png>

Haskell code is expressive.  The [HLearn library](https://github.com/mikeizbicki/HLearn) uses 6 lines of Haskell to define a function for training a Bayesian classifier; the equivalent code in the [Weka library](http://www.cs.waikato.ac.nz/ml/weka/) uses over 100 lines of Java.  That's a big difference!  In this post, we'll look at the actual code and see why the Haskell is so much more concise.

**But first, a disclaimer:**  It is really hard to fairly compare two code bases this way.  In both libraries, there is a lot of supporting code that goes into defining each classifier, and it's not obvious what code to include and not include.  For example, both libraries implement interfaces to a number of probability distributions, and this code is not contained in the source count.  The Haskell code takes more advantage of this abstraction, so this is one language-agnostic reason why the Haskell code is shorter.  If you think I'm not doing a fair comparison, here's some links to the full repositories so you can do it yourself:



	
  * [HLearn's bayesian classifier source code](https://github.com/mikeizbicki/HLearn/blob/master/HLearn-classification/src/HLearn/Models/Classifiers/Bayes.hs) (74 lines of code)

	
  * [Weka's naive bayes source code](https://svn.cms.waikato.ac.nz/svn/weka/trunk/weka/src/main/java/weka/classifiers/bayes/NaiveBayes.java) (946 lines of code)


<!-- more -->


### The HLearn code


HLearn implements training for a [bayesian classifier](https://en.wikipedia.org/wiki/Naive_Bayes_classifier) with these six lines of Haskell:

    
    newtype Bayes labelIndex dist = Bayes dist
        deriving (Read,Show,Eq,Ord,Monoid,Abelian,Group)
    
    instance (Monoid dist, HomTrainer dist) => HomTrainer (Bayes labelIndex dist) where
        type Datapoint (Bayes labelIndex dist) = Datapoint dist
        train1dp dp = Bayes $ train1dp dp


This code elegantly captures how to train a Bayesian classifier---just train a probability distribution.  Here's an explanation:



	
  * The first two lines define the Bayes data type as a wrapper around a distribution.

	
  * The fourth line says that we're implementing the Bayesian classifier using the HomTrainer type class.  We do this because **the Haskell compiler automatically generates a parallel batch training function, an online training function, and a fast cross-validation function for all HomTrainer instances.**

	
  * The fifth line says that our data points have the same type as the underlying distribution.

	
  * The sixth line says that in order to train, just train the corresponding distribution.


We only get the benefits of the HomTrainer type class because the bayesian classifier is a monoid.  But we didn't even have to specify what the monoid instance for bayesian classifiers looks like!  In this case, it's automatically derived from the monoid instances for the base distributions using a language extension called [GeneralizedNewtypeDeriving](http://www.haskell.org/ghc/docs/7.6.1/html/users_guide/deriving.html).  For examples of these monoid structures, check out the algebraic structure of the [normal](http://izbicki.me/blog/gausian-distributions-are-monoids) and [categorical](http://izbicki.me/blog/the-categorical-distributions-algebraic-structure) distributions, or more complex distributions using [Markov networks](http://izbicki.me/blog/markov-networks-monoids-and-futurama).


### The Weka code


Look for these differences between the HLearn and Weka source:



	
  * In Weka we must separately define the online and batch trainers, whereas Haskell derived these for us automatically.

	
  * Weka must perform a variety of error handling that Haskell's type system takes care of in HLearn.

	
  * The Weka code is tightly coupled to the underlying probability distribution, whereas the Haskell code was generic enough to handle any distribution. This means that while Weka must make the "naive bayes assumption" that all attributes are independent of each other, HLearn can support any dependence structure.

	
  * Weka's code is made more verbose by for loops and if statements that aren't necessary for HLearn.

	
  * The Java code requires extensive comments to maintain readability, but the Haskell code is simple enough to be self-documenting (at least once you know how to read Haskell).

	
  * Weka does not have parallel training, fast cross-validation, data point subtraction, or weighted data points, but HLearn does.



``` Java    
 /**
   * Generates the classifier.
   *
   * @param instances set of instances serving as training data 
   * @exception Exception if the classifier has not been generated 
   * successfully
   */
  public void buildClassifier(Instances instances) throws Exception {

    // can classifier handle the data?
    getCapabilities().testWithFail(instances);

    // remove instances with missing class
    instances = new Instances(instances);
    instances.deleteWithMissingClass();

    m_NumClasses = instances.numClasses();

    // Copy the instances
    m_Instances = new Instances(instances);

    // Discretize instances if required
    if (m_UseDiscretization) {
      m_Disc = new weka.filters.supervised.attribute.Discretize();
      m_Disc.setInputFormat(m_Instances);
      m_Instances = weka.filters.Filter.useFilter(m_Instances, m_Disc);
    } else {
      m_Disc = null;
    }

    // Reserve space for the distributions
    m_Distributions = new Estimator[m_Instances.numAttributes() - 1]
      [m_Instances.numClasses()];
    m_ClassDistribution = new DiscreteEstimator(m_Instances.numClasses(), 
                                                true);
    int attIndex = 0;
    Enumeration enu = m_Instances.enumerateAttributes();
    while (enu.hasMoreElements()) {
      Attribute attribute = (Attribute) enu.nextElement();

      // If the attribute is numeric, determine the estimator 
      // numeric precision from differences between adjacent values
      double numPrecision = DEFAULT_NUM_PRECISION;
      if (attribute.type() == Attribute.NUMERIC) {
        m_Instances.sort(attribute);
        if ( (m_Instances.numInstances() > 0)
          && !m_Instances.instance(0).isMissing(attribute)) {
          double lastVal = m_Instances.instance(0).value(attribute);
          double currentVal, deltaSum = 0;
          int distinct = 0;
          for (int i = 1; i < m_Instances.numInstances(); i++) { 	    
            Instance currentInst = m_Instances.instance(i); 	    
            if (currentInst.isMissing(attribute)) {
              break; 	    
            }
            currentVal = currentInst.value(attribute);
            if (currentVal != lastVal) {
              deltaSum += currentVal - lastVal;
              lastVal = currentVal;
              distinct++;
            }
          }
          if (distinct > 0) {
            numPrecision = deltaSum / distinct;
          }
        }
      }

      for (int j = 0; j < m_Instances.numClasses(); j++) {
        switch (attribute.type()) {
        case Attribute.NUMERIC: 
        if (m_UseKernelEstimator) {
          m_Distributions[attIndex][j] = 
            new KernelEstimator(numPrecision);
        } else {
          m_Distributions[attIndex][j] = 
            new NormalEstimator(numPrecision);
        }
        break;
        case Attribute.NOMINAL:
          m_Distributions[attIndex][j] = 
            new DiscreteEstimator(attribute.numValues(), true);
          break;
        default:
          throw new Exception("Attribute type unknown to NaiveBayes");
        }
      }
      attIndex++;
    }

    // Compute counts
    Enumeration enumInsts = m_Instances.enumerateInstances();
    while (enumInsts.hasMoreElements()) {
      Instance instance = 
        (Instance) enumInsts.nextElement();
      updateClassifier(instance);
    }

    // Save space
    m_Instances = new Instances(m_Instances, 0);
  }
```

And the code for online learning is:


``` Java 
 /**
   * Updates the classifier with the given instance.
   *
   * @param instance the new training instance to include in the model 
   * @exception Exception if the instance could not be incorporated in
   * the model.
   */
  public void updateClassifier(Instance instance) throws Exception {

    if (!instance.classIsMissing()) {
      Enumeration enumAtts = m_Instances.enumerateAttributes();
      int attIndex = 0;
      while (enumAtts.hasMoreElements()) {
        Attribute attribute = (Attribute) enumAtts.nextElement();
        if (!instance.isMissing(attribute)) {
          m_Distributions[attIndex][(int)instance.classValue()].
                addValue(instance.value(attribute), instance.weight());
        }
        attIndex++;
      }
      m_ClassDistribution.addValue(instance.classValue(),
                                   instance.weight());
    }
  }
```



### Conclusion


Every algorithm implemented in HLearn uses similarly concise code.  I invite you to [browse the repository](https://github.com/mikeizbicki/HLearn/) and see for yourself.  The most complicated algorithm is for Markov chains which use only [6 lines for training, and about 20 for defining the Monoid](https://github.com/mikeizbicki/HLearn/blob/master/HLearn-markov/src/HLearn/Models/Markov/MarkovChain.hs).

You can expect lots of tutorials on how to incorporate the HLearn library into Haskell programs over the next few months.

