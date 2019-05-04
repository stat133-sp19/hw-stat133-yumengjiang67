##Introduction
This is a R package *binomial* that implements functions for calculating probabilities of a Binomial random variable, and related calculations such as the probability distribution, the expected value, variance, etc.

##Overview
* "binomial" package contains the following basic functions
  - bin_choose()
  – bin_probability()
  – bin_distribution(); and plot.bindis()
  – bin_cumulative(); and plot.bincum()
  – bin_var(); and print.binvar(), summary.binvar(), print.summary.binvar()
  – bin_mean(), bin_variance(), bin_mode(), bin_skewness(), bin_kurtosis()


##Usage

```{r}
library(binomial)
#Create the distribution
trials1 <- 10
prob1<- 0.3
success1 <- 5
```

```{r}
#bin_choose
bin_choose(n = 5, k = 2)
bin_choose(trials1, prob1)
bin_choose(5, 1:3)
```
  
```{r}
#culculate the probability of binomial distribution
bin_probability(success1,trials1,prob1)
```

```{r}
#See the distribution of probability  
dis1 <- bin_distribution(trials1,prob1)
# to get a barplot
plot.bindis(dis1)
```
```{r}
#The cumulativeprobability
dis2 <- bin_cumulative(trials1,prob1)
#get a plot
plot.bincum(dis2)
```

```{r}
#Check the features
binvar1 <- bin_variable(trials1,prob1)
binvar1
#get a summary
summary.binvar(binvar1)
```

