## a quick overview on contrasts, centering, and weighted empirical logit
## written by laurel for 3/19/14 R/MEM workshop
library(lme4.0) #modeling functions
library(plyr) #reshaping
library(ggplot2) # plotting functions
library(lattice) #more plotting
# set the working directory to wherever you keep your data files
setwd("~/Desktop/2014_MEM_Workshop")

source('centerfactor.R') #load in the function saved in this script. it's in the working directory

# read in the main data table, save it as a dataframe called "bbdata". this is the data from my 2013 paper.
read.table("bbdata.txt", header=T)->bbdata

# for kicks, we're going to merge in the day and time the subject was run. I've always wanted to see if that matters.
# loading & merging date info in to data file
read.table("bbdates.txt",header=T)->bbdates
# this merge command combines data frames if they have common columns: very useful to have in your skill toolbox.
merge(bbdata,bbdates)-> bbdata

#look at data
summary(bbdata)
#hey, RT isn't treated in the right way... change it.
#and the int rating scale is averaged from a Likert scale, aka ordinal, not interval. Because the sizes of differences are not as important as their order, bin based upon score, and then treat that as ordinal.

#solve problem 1: make sure RT is reading as numeric
#if you have a numerical variable that has been treated automatically as a factor, you need to do this "as.numeric(as.character())" function because otherwise it will treat the factor *level* as numeric (aka, make an ordinal variable out of the factor)
bbdata$RT <- as.numeric(as.character(bbdata$RT))

#solve problem 2: bin likert variable
#bin based upon rating scores
#start by rounding to nearest integer
bbdata$IntOrd <-round(bbdata$IntRating)
#there are not very many extreme scores, so we are going to lump all the 1s with the 2s and all the 7s with the 6s.
bbdata$IntOrd[bbdata$IntOrd==1] <- 2
bbdata$IntOrd[bbdata$IntOrd==7] <- 6
#and then treat this measure as a factor
bbdata$IntOrd <- as.factor(bbdata$IntOrd)

#We're also going to crate a numeric variable for errors/accuracy for ease of analysis later on. 
bbdata$Accuracy <- as.numeric(bbdata$Response)-1   #factor is ordered alphabetically such that "pluralv"=1 and "singv"=2. this command takes the factor level as a number and then subtracts 1. turns it in to pluralv=0 and singv=1. since the correct response is singular, this is accuracy.
bbdata$PError <- 1- bbdata$Accuracy  #error is the inverse of accuracy in this data set (since there are just 2 outcomes)

#look at data again-- ok, all the things that are numbers are treated as numbers, and all the other things are not.
summary(bbdata)

#now subset data: we're going to want to examine RTs for singular and plural verb responses separately. we'll do analysis on the whole data set for the error data.
bbdataS <- bbdata[bbdata$Response == "singv",]
bbdataP <- bbdata[bbdata$Response == "pluralv",]

summary(bbdataS)
summary(bbdataP)


#Contrasts
#to check what contrasts are set to currently...
# contrasts(x)
#to set new contrasts
# contrasts(x)=c(a,b)

#If you get stuck, try these resources: 
# http://www.ats.ucla.edu/stat/r/library/contrast_coding.htm
# scottfraundorf.com/statistics.html

#Let's start by thinking about types of contrasts for unordered factors.

#Dummy(treatment) coding: c(0,1)
#Compare to baseline level
#Hold variable A at 0, compare levels of B.
#This is R's default. It's probably not what you want.
#This will give you SIMPLE main effects, rather than (regular) main effects.
#contrasts(x)=c(0,1)
#e.g.
contrasts(bbdataS$LocNum)=c(0,1)
contrasts(bbdataS$IntCat)=c(0,1)

#run a model with these contrasts:
model1 <- lmer(RT~LocNum*IntCat+(1+LocNum*IntCat|Subject)+(1+LocNum*IntCat|Item),data=bbdataS)
summary(model1)

#now let's try a different (and BETTER) set of contrasts.
#Effects coding
#Compare to average
#Compare levels of B at average level of A (Average of -.5 and +.5 is 0)
contrasts(bbdataS$LocNum)=c(.5,-.5)
contrasts(bbdataS$IntCat)=c(.5,-.5)

#set contrasts, and run the same model as above with the new contrasts
model2 <- lmer(RT~LocNum*IntCat+(1+LocNum*IntCat|Subject)+(1+LocNum*IntCat|Item),data=bbdataS)
summary(model2)

#what's the same? the interaction term. what's different? the main effects.


#Effects coding for variables with more levels
#For our preposition variable: "with" feels more plural than "for" and "of" (something like a comitative semantic role). test how this affects agreement, compare "with" to the other 2, and then compare "for" and "of" to each other.
#we're creating a little matrix to set up that contrast. 
#cbind = combine columns. c( )= list of items to go in rows.
#default ordering of factor levels is alphabetical, so level 1 is for, level 2 is of, and level 3 is with.
contrasts(bbdataS$Preposition)=cbind(c(.25,.25,-.5),c(.5,-.5,0))

#let's look at the way this Preposition variable interacts with Local noun number. 
#Use effects coding contrasts for both.
model3 <- lmer(RT~LocNum*Preposition+(1+LocNum*Preposition|Subject)+(1+LocNum*Preposition|Item),data=bbdataS)
summary(model3)

#note that unlike ANOVA, there are multiple interaction terms if you have factors with more than 2 levels. 
#we can put it into an anova table though and see the overall main effect:
anova(model3)

#problem: if you look at the correlations of factors, the fixed effects are super correlated...we need to worry about centering variables. 
#there are not very many "for"s, and a ton of "with"s, so this makes the data unbalanced. 
#we'll return to this in a moment.

#Now, here's another type of contrast, for ordered factors:
#note that you can use everything from above for ordered factors if you think about how you're setting up the comparisons

#Helmert coding
#Test whether level is different from average of previous ones
#nice, that's what we want to look at for that ordinal variable that comes from a likert scale, IntOrd-- we binned the ratings because the magnitude of differences between items is not necessarily consistent, aka, not linear, but there is an order to it.
#There's a built-in for this but it doesn't standardize to sum(|contrasts|)=0
#contrasts(x)=contr.helmert(#levels)
contrasts(bbdataS$IntOrd)=contr.helmert(5)
#you can write it yourself! this code makes a little matrix, binding together several rows of contrasts 
contrasts(bbdataS$IntOrd)=cbind(c(-1/2,1/2,0,0,0),c(-1/4,-1/4,1/2,0,0),c(-1/6,-1/6,-1/6,1/2,0),c(-1/8,-1/8,-1/8,-1/8,1/2))
#or do the comparison from R to L: that's actually what I want to look at. 
contrasts(bbdataS$IntOrd)=cbind(c(0,0,0,1/2,-1/2),c(0,0,1/2,-1/4,-1/4),c(0,1/2,-1/6,-1/6,-1/6),c(1/2,-1/8,-1/8,-1/8,-1/8))

#and here's a model that looks at this in comparison to local number. the maximal structure won't converge, because each item only has 4 forms. therefore the item random effects have missing cells & need to be simplified.
model4 <- lmer(RT~LocNum*IntOrd+(1+LocNum*IntOrd|Subject)+(1|Item),data=bbdataS)
summary(model4)

#One other type of contrast for ordered factors that is less common
#Polynomial coding
#Test shape of curve: how much is it linear/quadratic/cubic...?
#not really what we would want here, but one could do it:
#in general:
#contrasts(x)=contr.poly(#levels)
#e.g.
#contrasts(bbdataS$IntOrd)=contr.poly(5)


#Centering
#Sometimes you don't have the same number of items per bin
#This can matter for interpretation of effects.
#I would like to account for different numbers of obervations per cell in our contrasts, otherwise the regression model will be more influenced by things with more observations.
#we do this by centering the fixed effects so that the mean of the means is 0
#give more weight to cells with fewer observations
#note: this is only valuable when data is missing AT RANDOM.
# If data is NOT MISSING AT RANDOM, this will distort you output in a bad way.

#Logically, this is similar to mean centering for continuous variables:
#Mean center, or else you're testing compared to a value of 0
# that day variable is continuous, and day = 0 (Dec 31) is not where we want to do our comparisons
# what would make more sense is the mean day data was collected during
bbdataS$JulianCent <- bbdataS$Julian - mean(bbdataS$Julian)

#here's a little model with that as sole predictor:
model5 <- lmer(RT~JulianCent+ (1|Subject)+ (1+JulianCent|Item),data=bbdataS)
summary(model5)
#WHEW. day of year doesn't predict simple RT effects.

# It is very simple to make your contrasts centered for 2 level factors!!
# we had set this: contrasts(bbdataS$LocNum)=c(.5,-.5)
#but there aren't the same number of observations in each case:
summary(bbdataS$LocNum)->locnsum
#change weights to reflect that there are more locs than locp cases
#assuming that the data are not missing at random, we want to give more weight to the smaller set
w1 <- 1-locnsum[1]/(locnsum[1]+locnsum[2])
w2 <- -(1-(locnsum[2]/(locnsum[1]+locnsum[2])))
contrasts(bbdataS$LocNum)=c(w1,w2)
#adjust IntCat
summary(bbdataS$IntCat)->insum
#change weights to reflect that there are more locs than locp cases
#assuming that the data are not missing at random, we want to give more weight to the smaller set
w1 <- 1-insum[1]/(insum[1]+insum[2])
w2 <- -(1-(insum[2]/(insum[1]+insum[2])))
contrasts(bbdataS$IntCat)=c(w1,w2)

#rerun the model from before:
model6 <- lmer(RT~LocNum*IntCat+(1+LocNum*IntCat|Subject)+(1+LocNum*IntCat|Item),data=bbdataS)
summary(model6)
#for kicks, I'm curious to throw in the date predictor too. we have to simplify b/c the random effects structure won't converge.
model7<- lmer(RT~LocNum*IntCat*Julian+(1+LocNum*IntCat|Subject)+(1+LocNum*IntCat|Item),data=bbdataS)
summary(model7)
#yep, day of year isn't mattering here too!

# It is much less simple to center contrasts for >2 level factors... Think about what you want to compare and how that translates to weight in cells, and then set a comparison to test that.
# scott fraundorf has a script to center factors with a helmert contrast. we'll use that. we loaded it in in the very beginning.
# the basic idea is to weight the reference level as .5, and then to center the mean of the factors.
# I've sent this script around to you all.
# syntax: centerfactor(factor,c(reference level for contrast 1, reference level for contrast 2 ...))
contrasts(bbdataS$Preposition)=centerfactor(bbdataS$Preposition,c(3,2))

#cool, let's re-run that model from before, with local number & preposition
model8 <- lmer(RT~LocNum*Preposition+(1+LocNum*Preposition|Subject)+(1+LocNum*Preposition|Item),data=bbdataS)
summary(model8)

#weighted empirical logit
# thus far, we have looked at RTs for correct responses.
# we also have data on errors: In which conditions do people mess up the most?
# this is a binomial DV, so we want a logit as our link function.
# however, being ERROR data, there are a lot of cases in which people were 100% correct.
# this is a problem: the logit (log odds) is not defined close to 0 and 1. it approaches negative and positive infinity.
# so we need to correct for it by doing an aggregation and adjusting both the numerator and denominator.
# we also want to add weights to the analysis to make the variability accurate, because of this aggregation.  one way of thinking about it: a 0 coming from 10000 observations is a lot more reliable than a 0 coming from 5 observations. even though they're both 0.
# we'll do a by-subjects (collapsing across items) and by-items (collapsing across subjects) version of this analysis
# here's a tutorial: http://talklab.psy.gla.ac.uk/tvw/elogit-wt.html

#to start, let's add a counter variable:
bbdata$Counter = 1

#now, begin the aggreagation for the by-subjects analysis:
#calculate the proportion of plurals, using the numeric version of errors.
bbdataSubs <- aggregate(bbdata$PError, by=list(bbdata$Subject,bbdata$LocNum,bbdata$IntCat), sum)
bbdataSubs <- data.frame(bbdataSubs)
# add the frequency count
bbdataSubs$Frequency <- aggregate(bbdata$Counter, by=list(bbdata$Subject,bbdata$LocNum,bbdata$IntCat), sum)[,4]
# [,4] because we just want the LAST (4th) column from this -- the one with the frequency data
#cleaning up: add names back in
colnames(bbdataSubs) = c('Subject','LocNum','IntCat','PropPlural','TotalTrials')
summary(bbdataSubs)

# calculate the empirical logit for each bin: add .5 to numerator and denominator to make points near 0 or 1 defined.
bbdataSubs$EmpLogit = log((bbdataSubs$PropPlural+.5)/(bbdataSubs$TotalTrials-bbdataSubs$PropPlural+.5))

# calculate the WEIGHT for each bin, based on the # of observations
v = (1/(bbdataSubs$PropPlural+.5)) + (1/(bbdataSubs$TotalTrials-bbdataSubs$PropPlural+.5))
myweights = 1/v

#add contrasts, just like before! note that since we aggregated across subs, there are now an equal number of observations per cell, we don't need to adjust the weights of these contrasts. that's why we DID have to weight before.
contrasts(bbdataSubs$LocNum)=c(.5,-.5)
contrasts(bbdataSubs$IntCat)=c(.5,-.5)

#run the model!
elogitsubs1 <- lmer(EmpLogit~LocNum*IntCat + (1+LocNum*IntCat|Subject), data=bbdataSubs,weights=myweights)
summary(elogitsubs1)

#we would do the same for a by-items analysis! I leave that as an exercise for the reader.
#note that as in SBS's presentation on eye data, if you have multiple observations per trial (such as in eyetracking), you can aggregate within trial, preserving subject and item level information. then you can have crossed subject and item effects, and you only need to run one analysis.




### assorted graphs, from presentation or otherwise:
#tabulate, then plot
avgRT <- ddply(bbdata, .(IntCat,LocNum,Response),summarize,RT=mean(RT))
proppl <- ddply(bbdata, .(IntCat,LocNum),summarize,PError=mean(PError))

ggplot(aes(x = IntCat, y = RT, fill=IntCat,alpha=LocNum),data = avgRT)+
	geom_bar(stat="identity",color="black")+
	scale_y_continuous("RT")+
	facet_grid(Response~LocNum)+
	theme_bw()
	
ggplot(aes(x = IntCat, y = PError, fill=IntCat),data = proppl)+
	geom_bar(stat="identity",color="black")+
	scale_y_continuous("Proportion plural responses")+
	facet_grid(.~LocNum)+
	theme_bw()	

#again for the ordinal integration measure, but this time, I want to show the subj variability since it makes sense to plot as a line graph
#get average RT and proportion error for graphs:
avgRT2 <- ddply(bbdata, .(Subject,IntOrd,LocNum,Response),summarize,RT=mean(RT))
proppl2 <- ddply(bbdata, .(Subject,IntOrd,LocNum),summarize,PError=mean(PError))

#plot average RTs for singular and for plural data in the ordinal integration measure
xyplot(RT~IntOrd|Response,group=LocNum,type=c("p","a"),data=avgRT2,auto.key=TRUE)
#error rates across the expt
xyplot(PError~IntOrd,group=LocNum,type=c("p","a"),data=proppl2,auto.key=TRUE)

#error rates are also extremely variable by subject. but the local plural number factor increases error rates. referential integration doesn't do too much-- maybe in the middle of the scale it's bigger?  that seems odd. let's look at the way error rates change by item type (preposition factor) and see if it's caused by that.

#look at preposition effect on RT & error rates
#reaggregate data so we're averaging across subjects & items
avgRT3 <- ddply(bbdata, .(Preposition,LocNum,Response),summarize,RT=mean(RT))
proppl3 <- ddply(bbdata, .(Preposition,LocNum),summarize,PError=mean(PError))

ggplot(aes(x = Preposition, y = RT,fill=Preposition,alpha=LocNum),data = avgRT3[1:12,])+
	geom_bar(stat="identity",color="black")+
	scale_y_continuous("RT")+
	facet_grid(Response~LocNum)+
	theme_bw()
	
ggplot(aes(x = Preposition, y = PError,fill=Preposition,alpha=LocNum),data = proppl3[1:6,])+
	geom_bar(stat="identity",color="black")+
	scale_y_continuous("Proportion plural responses")+
	facet_grid(.~LocNum)+
	theme_bw()	


#ok, looks like the manipulation in the "for" items was maybe not very strong. is this what's causing the bump in the error x referential integration plot? see how preposition items line up across referential integration & the error rates in each bin.
#reaggregate
proppl4 <- ddply(bbdata, .(Preposition,LocNum,IntOrd),summarize,PError=mean(PError))
barchart(PError~IntOrd|LocNum,group=Preposition,data=proppl4,auto.key=TRUE)

#yes, the loc-p "for" items are giving us the finger. that's what seems to be causing the bump. if you look at the other 2 prepositions, the error data looks a bit more similar to the RT data. 

#we know that local number matters for error rates. let's see how that effect changes across the year. I have a hypothesis that end-of-semester subject pool students are not much like the other students. most of this data came from the subject pool, so we can look at that.
#reaggregating to prepare for plot
proppl4 <- ddply(bbdata, .(Subject,LocNum,Julian),summarize,PError=mean(PError))

ggplot(aes(x = Julian, y = PError, color=LocNum),
	data = proppl4)+
	geom_point()+
	geom_line()+
	scale_x_continuous("Day since Jan 1")+
	scale_y_continuous("Proportion of errors")
	#overall it looks like there are more errors for the locp condition (hard) than the locs condition (easy)
	#except for this one day...
	#day 120 is the big spike in error rates in the "easy" condition (and also the "hard" condition).  that's April 30. last day to run subj pool studies in 2010 was May 6.
	#but it IS just one person.

