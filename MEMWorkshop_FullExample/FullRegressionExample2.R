install.packages("lme4.0", repos = c("http://lme4.r-forge.r-project.org/repos", getOption("repos")))
library(lme4.0)
library(plyr) # contains ddply(), a function we'll use for down-sampling our data
library(ggplot2) # plotting functions
# set the working directory to wherever you keep the data file
setwd("/Users/alexfine/Dropbox/Projects/RapidAdaptation/Experiments/AllRC_80Fillers_MT/results/")

# read in the table, save it as a dataframe called "d"
read.table("mturkallrc80fill.rtm", header=F)->d

# name the columns of d
colnames(d) <- c("Expt", "Cond", "Item", "Subj", "ListPos", "WordPos", "Word", "Region", "RawRT", "RawZ", "ResidRT", "zResidRT", "Correct")

# get rid of practice trials (normally you may want to inspect these, but we're going to just get rid of them)
d <- subset(d, Expt != "practice")

# this function eliminates levels of factors for which there are 0 observations (you can do summary(d) before running this command and look at the "Expt" column to see what I mean)

myFactorCleanup <- function(x) {
  if (is.factor(x)) {
    return(as.factor(as.character(x)))
  }
  if (is.data.frame(x) || is.matrix(x)) {
    for (i in 1:ncol(x)) {
      if(is.factor(x[,i])) { x[,i] <- as.factor(as.character(x[,i])) }
    }
    return(as.data.frame(x))
  }
}

myFactorCleanup(d)->d


# Here I'm going to get a sense of overall RT distributions and comprehension question accuracy and see if any subjects or items need to be removed from the analysis

# compute overall percentage of RTs that fall between 100ms and 2000ms (exclusion cutoff points)
nrow(subset(d, RawRT > 100 & RawRT < 2001))/nrow(d) # .99

# compute overall percentage of correctly answer comprehension Q's
nrow(subset(d, Correct == 100))/nrow(d) # .93

# inspect percentage of correctly answered Qs by subject.  I typically remove subjects with <80% correct
q <- ddply(d, .(Subj), function(x) nrow(subset(x, Correct == 100))/nrow(x))
colnames(q) <- c("Subj", "correct")
sort(q$correct) # don't need to remove anyone by the 80% criterion

# we can also break this down by item, to see if there were any items that have very low comprehension question accuracy.  I leave this as an exercise to the reader.

# same for RTs, etc. etc.
ddply(d, .(Subj), function(x) nrow(subset(x, RawRT > 100 & RawRT < 2001))/nrow(x))

# I'm going to replace the value of RawRT with an "NA" for trials where comprehension Qs are incorrectly answered and for words that were read faster than 100ms or slower than 2000 ms.
d[d$Correct != 100 | (d$RawRT < 100 | d$RawRT > 2000),]$RawRT <- NA

# the next few lines of code perform a residualization step.  Residualization comes up in a lot of contexts.  Here, what I'm doing is regressing RawRTs against word length (in characters) and saving the residuals of this model (i.e., roughly all the variance in RTs that is NOT explained by word length) and assigning it to a variable called "resids" that will serve as the dependent measure in our analysis.

d$Length <- nchar(as.character(d$Word))
d$resids <- resid(lmer(RawRT ~ Length + (1 + Length | Subj), na.action = na.exclude, data = d))

# we performed the residualization step with critical items and fillers.  from hear on out, though, we're just going to be analyzing critical items, so we can get rid of the fillers.

d <- subset(d, Expt == "adapt")
myFactorCleanup(d) -> d

# This for-loop takes about 3 minutes to run.  For each subject, gives the "list position" of critical items relative ONLY to other critical items.  This variable is interesting for me because the syntactic adaptation hypothesis predicts that the relevant variable is--in this experiment--how many RCs a subject has seen, not how many trials they have completed overall.  Of course the two variables are correlated, but ideally you'd like to find an effect of number of RCs while controlling for overall list position.  In the actual analyses reported in my papers, this is what we do.  Below we'll be doing a simplified version of the analysis.

is.factor(d$Subj)


myFactorCleanup(d)->d
d$ListPos.N = 0

for (level in levels(d$Subj)) {
  uniquePos = unique(d[d$Subj==level,]$ListPos)
  totalLP = length(uniquePos)
  
  index = 1
  currentLP = uniquePos[index]
  for (i in c(1:totalLP)) {
    d[d$Subj==level & d$ListPos==currentLP,]$ListPos.N = i
    index = index + 1
    currentLP = uniquePos[index]
  }
}


# We need to analyze RTs just at the disambiguating region (though in a paper of course you'd want to analyze RTs at each sentence region to make sure that the predicted effect shows up at the predicted region and not, say, at the subject region.  You can do that on your own if you want!).  So you might imagine we could do something like...

disambiguating <- subset(d, Region == 4)

# ...and be done with it.  

# However, if you perform the analysis on this, you're going to have an artificially inflated n, since each sentence will correspond to three observations.  So we need to down-sample the data, and compute the mean RT at the disambiguating region for all the unique combinations of Subject, Item, List position, and condition.  ddply() was made for this task.

disamb <- ddply(subset(d, Region==4), .(Subj, Item, ListPos, ListPos.N, Cond), function(x) mean(x$resids, na.rm = T))
summary(disamb)
colnames(disamb) <- c("Subj", "Item", "ListPos", "ListPos.N", "Cond", "resids")

# regression analyses

# RT ~ Cond + ListPos.N + Cond:ListPos.N + logListPos

# first, we need to perform any necessary transformations of our independent variables.  

# I'm going to sum-code the two-level categorical predictor.
contrasts(disamb$Cond) <- c(1, -1)

# And I'm going to log-transform and then center list position
disamb$lListPos <- log(disamb$ListPos)
disamb$clListPos <- disamb$lListPos - mean(disamb$lListPos, na.rm=T)

# and I'm going to center item order

disamb$cListPos.N <- disamb$ListPos.N - mean(disamb$ListPos.N, na.rm = T)

# sum-coding a categorical predictor is equivalent to mean-centering that predictor, so we're effectively centering all of our predictors.  Why do this?  Mean-centering predictors reduces collinearity (see slides for an explanation) and, in a model with lots of continuous predictors whose coefficients you want to COMPARE, (1) mean-centering and (2) setting the SD of each predictor to 1 (1 and 2 together are known as "standardizing" an independent variable) put continuous predictors in the same "space", so that the magnitudes of their coefficients are comparable.  Otherwise the magnitude of the coefficient will reflect the scale in which the independent variable is measured, and will be meaningless when compared to coefficients that are on different scales (e.g., it'd be meaningless to compare coefficient sizes for an IV encoding inches and an IV encoding pounds unless you standardize them).

# now we can specify the model.  We're going to try a model with the maximal random effects structure justified by the design, as recommended by Barr et al. (2013)

# run this model and go get some coffee or something.  it will take a few minutes.

# We set REML = F, by the way, because we can only do log-likelihood ratio tests for models estimated using maximum likelihood, not restricted maximum likelihood (REML), as a criterion for model parameters

m.max <- lmer(resids ~ Cond * cListPos.N + clListPos  + (1+Cond * cListPos.N + clListPos|Subj) + (1+Cond * cListPos.N + clListPos|Item), data = disamb, REML = F)
summary(m.max)

# this model does not converge.  that is, within a reasonable number of model evaluations, the ML algorithm was unable to find stable parameter values that maximize p(Data|Model)

# failure to converge will often happen with big models like this with complex RE structures. We inspect the partially converged model and remove random slopes one at  time according to how much of hte variance they account for, starting with those random slopes that account for the least variance.

summary(m.max)

# for instance, here the by-subj random slope for the interaction seems to be contributing least, so we could remove that and try this:

m.max.2 <- lmer(resids ~ Cond * cListPos.N + clListPos  + (1+Cond + cListPos.N + clListPos|Subj) + (1+Cond * cListPos.N + clListPos|Item), data = disamb, REML = F)

# this model also fails to converge.  

summary(m.max.2)

# let's try removing the by-item random slope for the two-way interaction

m.max.3 <- lmer(resids ~ Cond * cListPos.N + clListPos  + (1+Cond + cListPos.N + clListPos|Subj) + (1+Cond + cListPos.N + clListPos|Item), data = disamb, REML = F)

# still no luck.

summary(m.max.3)

# remove the by-item random slope for item order
contrasts(disamb$Cond) <- c(0,1)
m.max.4 <- lmer(resids ~ Cond * cListPos.N + clListPos  + (1+Cond + cListPos.N + clListPos|Subj) + (1+Cond + clListPos|Item), data = disamb, REML = F)

# this model converges.

summary(m.max.4)

m.max.4.NoCond <- lmer(resids ~ Cond * cListPos.N + clListPos - Cond  + (1+Cond + cListPos.N + clListPos|Subj) + (1+Cond + clListPos|Item), data = disamb, REML = F)

anova(m.max.4, m.max.4.NoCond)

# how do I report this model?  See slides.

# How do I visualize these effects?  Joe will go into data visualization more, but here's a teaser.


down <- ddply(disamb, .(ListPos.N, Cond), function(x) mean(x$resids, na.rm = T))
colnames(down) <- c("ListPos.N", "Cond", "resids")
ggplot(aes(x = ListPos.N, y = resids, colour = Cond),
       data = down)+
       geom_point()+
       scale_x_continuous("Item Order")+
       scale_y_continuous("Length-corrected reading times")+
       scale_colour_manual("Ambiguity", labels = c("Ambiguous", "Unambiguous"), values = c("blue", "red"))+
       stat_smooth(method = lm, fullrange = T, se = T)
