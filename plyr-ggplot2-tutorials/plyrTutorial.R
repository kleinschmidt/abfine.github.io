
# plyr tutorial for R workshop
# 17-Apr 2014
# Joe Toscano (jtoscano@illinois.edu)
#
# More info about plyr: http://plyr.had.co.nz/
#
# Article on logic of plyr data analysis approach:
#	Wickham, H. (2011). The split-apply-combine strategy for data analysis.
#	Journal of Statistical Software, 40, 1-29.
#
# main plyr functions:
#	transform: make changes to current variable
#	summarize: output new variable <- recommended
#	a*ply: used for arrays
#	l*ply: used for lists
#	d*ply: used for dataframes <- most likely what you will be working with
#
# other plyr functions:
#	r*ply: used for iterative processing
#	mutate: create new columns in dataframe
#	arrange: re-arrange columns in dataframe
#	and several others...
#
# Note: code is for plyr version 1.8
#
# Data samples from:
#
# 	Toscano, J.C., & McMurray, B. (2012, October). Voicing in English revisited:
#	Measurement of acoustic features signaling word-medial voicing in trochees.
#	Poster presented at the 164th Meeting of the Acoustical Society of America,
#	Kansas City, MO.


##############################################################################

# Clear workspace
rm(list = ls(all = TRUE))

# Load libraries
library(plyr)

# Set working directory
setwd('~/desktop/r-workshop')

# Load data
data <- as.data.frame(read.delim(file='sampleData1.txt', sep='\t'))
head(data)


##############################################################################

# Get mean burst intensity as a function of consonant and output as array
avgData <- daply(data, .(consonant), summarize, mean(burstInt))
avgData

# Get mean burst intensity as a function of consonant and output as list
avgData <- dlply(data, .(consonant), summarize, mean(burstInt))
avgData

# Get mean burst intensity as a function of consonant and output as (smaller) dataframe
avgData <- ddply(data, .(consonant), summarize, burstInt=length(burstInt))
avgData
plot(avgData)


# Get mean burst intensity as a function of consonant and vowel
avgData <- ddply(data, .(consonant, vowel), summarize, burstInt=mean(burstInt))
head(avgData)

# Get mean burst intensity as a function of voicing for each subject
avgData <- ddply(data, .(talker, voicing), summarize, burstInt=mean(burstInt))
head(avgData)

# Get mean and standard error for each subject
nsub <- length(unique(data$talker))
subMeans <- ddply(data, .(talker, voicing), summarize, burstInt=mean(burstInt))
avgData <- ddply(subMeans, .(voicing), summarize, 
	meanBurstInt=mean(burstInt),
	seBurstInt=sd(burstInt)/sqrt(nsub))
avgData

# Find (and remove) outliers based on 2.5 SDs for each subject
avgData.outliers <- ddply(data, .(talker), mutate,
              burstIntUpper = mean(burstInt) + (2.5*sd(burstInt)),
              burstIntLower = mean(burstInt) - (2.5*sd(burstInt)),
              burstIntOutlier = 
				ifelse(burstInt > burstIntUpper, 'outlier', 
				ifelse(burstInt < burstIntLower, 'outlier', 'keep')))
nOutliers <- nrow(subset(avgData.outliers, burstIntOutlier=='outlier'))
percentOutliers <- (nOutliers/nrow(data))*100
avgData.trimmed <- subset(avgData.outliers, burstIntOutlier=='keep')

