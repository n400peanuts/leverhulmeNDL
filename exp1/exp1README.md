# Replication study

#### 1)	Main question addressed in this study: does the order by which cues are presented affect learning? ####

Learning is all about discrimination via cues competition, and the order by which the information is available to a learner affects learning. Discrimination learning is facilitated when objects predicts labels, but not when labels predict objects. When objects predict labels all possible features are evaluated and compete for relevance, this results in a process of “narrowing down” the most predictive feature that unambiguously predicts a label. On the other hand, when labels predict objects, because of the sparse nature of the features of the labels, the competition process is inhibited.

#### 2)	Description of the key independent and dependent variable(s), specifying how they will be measured. ####
This is a 2x2 between design where learning (LF,FL) and frequency (highFreq, lowFreq) are manipulated in the following way.

Independent variables: 
Learning:
-	Label-to-Feature (LF) learning, i.e., learning to predict the objects from the labels
-	Feature-to-Label (FL) learning, i.e., learning to predict the labels from the objects

Frequency:
-	High frequency exemplars (highFreq): 75% of exemplars are made of a high salient feature specific of that subcategory
-	Low frequency exemplars (lowFreq): 25% of exemplars are drawn from another subcategory, therefore will have a different high salient feature

Dependent variable: 
Accuracy on a speeded two-alternative forced choice test (2AFC) on:
-	Previous learned stimuli/labels (explicit memory) and 
-	Novel unseen stimuli (generalization).

#### 3)	Hypothesis: ####
Predictions are guided by modelling and previous experiments described in Ramscar 2010 study and Masa’s 4a experiments. We predict a main effect of training and exemplars’ frequency respectively, and a training by frequency interaction.
-	H1: learners assigned to the LF training will display higher accuracy for highFreq compared to lowFreq items, while learners of the FL training will display no differences in accuracy between highFreq and lowFreq items.
-	H0: no difference between trainings, and no difference between high and low frequency exemplars

#### 4)	Positive controls or quality checks  that will confirm that the obtained results are able to provide a fair test of the stated hypothesis. ####

We are going to introduce a task independent to our main hypothesis test. This task is going to be a nonverbal counting task where participants will be presented with a random number of white dots on a black background for around 150ms, and asked to estimate and report the number of dots in a given time (around 3s). This task is going to be presented to participants on a pseudo-random basis for a maximum of 4 times while performing the core experiment. We will record reaction times and their magnitude estimation from this task however we will exclude participants on the basis of how many timeouts they had during the whole experiment, fixing a max to 2. This will guarantee us that participants payed attention to the experiment at least half of the time.

#### 5)	Method and materials #### 

Stimuli

Three experimental ‘‘fribble’’ categories are constructed in a way that match the
categories used in the simulations (see simulation on github https://github.com/n400peanuts/leverhulmeNDL). Each category is comprised of two subcategories (highFreq and lowFreq) both clustered around a high-saliency nondiscriminating feature, that is the central body shape, and a set of low-saliency discriminating features attached to it. The main idea is that learners in order to discriminate unambiguously across categories have to rely on the discriminating features more than the central body shape. 

For example, in figure 1, category 1 has the subcategory 1.1 clustered around the red barrel-like shape, while subcategory 1.2 is clustered around the blue jar-like shape. Each discriminating feature circled in red attached to the central body is uniquely assigned to the specific subcategory, so that no discriminating feature was shared within and across categories and subcategories. On the other hand, the non-discriminating feature, i.e., the central body, is shared across categories in the following way: body shapes belonging to the lowFreq subcategory exemplars in one category are used to build highFreq subcategory exemplars in another category. 

![Stimuli expe1](stimuli/stimuliReplication.png)
