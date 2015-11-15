
# ggplot2 tutorial for R workshop
# 17-Apr 2014
# Joe Toscano (jtoscano@illinois.edu)
#
# More info about ggplot2: http://ggplot2.org/
# ggplot2 book: http://ggplot2.org/book/
#
# Note: code is for ggplot2 version 0.9.3.1
#
# Data samples from:
#
# 	Toscano, J.C., & McMurray, B. (2012, October). Voicing in English revisited:
#	Measurement of acoustic features signaling word-medial voicing in trochees.
#	Poster presented at the 164th Meeting of the Acoustical Society of America,
#	Kansas City, MO.
#
# 	Toscano, J.C., McMurray, B., Dennhardt, J., & Luck, S.J. (2010). Continuous
# 	perception and graded categorization: Electrophysiological evidence for a linear
# 	relationship between the acoustic signal and perceptual encoding of speech.
# 	Psychological Science, 21, 1532-1540.


##############################################################################

# Clear workspace
rm(list = ls(all = TRUE))

# Load libraries
library(ggplot2)
library(plyr)
library(lme4)

# Set working directory
setwd('~/desktop/r-workshop')

# Load data
data <- as.data.frame(read.delim(file='sampleData1.txt', sep='\t'))
head(data)


##############################################################################

# Basics

# Get mean burst intensity as a function of consonant and output as (smaller) dataframe
avgData <- ddply(data, .(consonant), summarize, meanBurstInt=mean(burstInt))
avgData

# Default plot
plot(avgData)

# Equivalent plot using ggplot
ggplot(avgData, aes(x=consonant, y=meanBurstInt)) +
	geom_point()


##############################################################################

# One categorical independent variable, one dependent variable
# (a.k.a. '1D' scatter plots)

# Get mean and standard error as a function of consonant
nsub <- length(unique(data$talker))
subMeans <- ddply(data, .(talker, consonant), summarize, burstInt=mean(burstInt))
avgData <- ddply(subMeans, .(consonant), summarize, 
	meanBurstInt=mean(burstInt),
	seBurstInt=sd(burstInt)/sqrt(nsub))
avgData

# ggplot figures are based on the 'Grammar of Graphics' (Wilkinson, 2005; Wickham, 2009)
#	Aesthetics
#	Geoms
#	Scales
#	Facets
#	Statistics <- often used in combination with geoms


# Aesthetics (aes): map variables in dataframe onto elements in plot
#	x
#	y
#	colour
#	linetype
#	shape
ggplot(avgData, aes(x=consonant, y=meanBurstInt)) +
	geom_point(size=3)


# Geoms: draw a particular type of object; can combine multiple geoms
#	geom_point()
#	geom_line()
#	geom_errorbar()
#	geom_boxplot()

# points and errorbars
ggplot(avgData, aes(x=consonant, y=meanBurstInt)) +
	geom_point(size=3) +
	geom_errorbar(width=0, aes(ymin= meanBurstInt-seBurstInt, ymax= meanBurstInt+seBurstInt))

# boxplot
# Note: this creates summary statistics from your data; make sure you know what it is doing
ggplot(data, aes(x=consonant, y=burstInt)) +
	geom_boxplot(notch=T)


# Scales: elements for adjusting aesthetics (axes, colors, linetypes, shapes)
# 	scale_x_continuous()
# 	scale_y_continuous()
# 	scale_y_log10()
#	scale_y_reverse() <- if you want your ERP data wit negative up
#	scale_colour_discrete()
# e.g., use limits on x-scale to adjust order of consonants
ggplot(avgData, aes(x=consonant, y=meanBurstInt)) +
	geom_point(size=3) +
	geom_errorbar(width=0, aes(ymin= meanBurstInt-seBurstInt, ymax= meanBurstInt+seBurstInt)) +
	scale_x_discrete('Consonant', limits=c('b','d','g','p','t','k'),
		labels=c('/b/','/d/','/g/','/p/','/t/','/k/')) +
	scale_y_continuous('Mean burst intensity (dB HL)')


# Coords: for adjusting the coordinate system, including the x and y limits
#	coord_cartesian()
#	coord_polar()
# e.g., use coord_cartesian to adjust y-axis range in plot
ggplot(avgData, aes(x=consonant, y=meanBurstInt)) +
	geom_point(size=3) +
	geom_errorbar(width=0, aes(ymin= meanBurstInt-seBurstInt, ymax= meanBurstInt+seBurstInt)) +
	scale_x_discrete('Consonant', limits=c('b','d','g','p','t','k'),
		labels=c('/b/','/d/','/g/','/p/','/t/','/k/')) +
	scale_y_continuous('Mean burst intensity (dB HL)') +
	coord_cartesian(ylim=c(50,80))


##############################################################################

# Multiple categorical independent variables

# Set up data frames
nsub <- length(unique(data$talker))
subMeans <- ddply(data, .(talker, voicing, place, consonant), summarize, burstInt=mean(burstInt))
avgData <- ddply(subMeans, .(voicing, place, consonant), summarize, 
	meanBurstInt=mean(burstInt),
	seBurstInt=sd(burstInt)/sqrt(nsub))
avgData


# Colors: specificed by aesthetics (mapping) and scales
#	scale_colour_hue()
#	scale_colour_discrete()
#	scale_colour_continuous()
#	scale_colour_brewer() <- see http://colorbrewer2.org/
#	scale_colour_manual()
ggplot(avgData, aes(x=place, y=meanBurstInt, colour=factor(voicing))) +
	geom_point(size=3) +
	geom_errorbar(width=0, aes(ymin= meanBurstInt-seBurstInt, ymax= meanBurstInt+seBurstInt)) +
	scale_x_discrete('Place of articulation', limits=c('bilabial', 'alveolar', 'velar')) +
	scale_y_continuous('Burst intensity (dB HL)') +
	scale_colour_brewer('Consonant', type='qual', palette=6)
	
# Linetypes: specificed by aesthetics (mapping) and scales
# Shapes: specificed by aesthetics (mapping) and scales
ggplot(avgData, aes(x=place, y=meanBurstInt, linetype=factor(voicing), shape=factor(voicing))) +
	geom_point(size=3) +
	geom_errorbar(width=0, aes(ymin= meanBurstInt-seBurstInt, ymax= meanBurstInt+seBurstInt)) +
	scale_x_discrete('Place of articulation', limits=c('bilabial', 'alveolar', 'velar')) +
	scale_y_continuous('Burst intensity (dB HL)') +
	scale_linetype('Voicing') +
	scale_shape('Voicing')


# Facets: sub-plots or panels based on one or more variables
#	facet_grid()
#	facet_wrap()
subMeans <- ddply(data, .(talker, place, voicing, vowel, consonant), 
	summarize, burstInt=mean(burstInt))
avgData <- ddply(subMeans, .(place, voicing, vowel, consonant), summarize, 
	meanBurstInt=mean(burstInt),
	seBurstInt=sd(burstInt)/sqrt(nsub))
avgData

# Plot with facets
ggplot(avgData, aes(x=place, y=meanBurstInt)) +
	geom_point(size=3) +
	geom_errorbar(width=0, aes(ymin= meanBurstInt-seBurstInt, ymax= meanBurstInt+seBurstInt)) +
	scale_x_discrete('Place of articulation') +
	scale_y_continuous('Burst intensity (dB HL)') +
	facet_grid(voicing~vowel)

# ...fix scales and add phoneme labels
ggplot(avgData, aes(x=place, y=meanBurstInt)) +
	geom_point(size=3) +
	geom_errorbar(width=0, aes(ymin=meanBurstInt-seBurstInt, ymax= meanBurstInt+seBurstInt)) +
	geom_text(aes(label=as.character(consonant), hjust=3, size=9), show_guide=F) +
	scale_x_discrete('Place of articulation') +
	scale_y_continuous('Burst intensity (dB HL)') +
	facet_grid(voicing~vowel, scales='free')


##############################################################################

# Histograms and 2D scatter plots

# Histogram
ggplot(data, aes(x=closureDur)) +
	geom_histogram()
	
# Adjust bin width add second factor
ggplot(data, aes(x=closureDur, fill=factor(voicing))) +
	geom_histogram(binwidth=0.005, position='identity', alpha=0.67)

# Plot a different acoustic cue from the dataset...
ggplot(data, aes(x=closureInt, fill=factor(voicing))) +
	geom_histogram(binwidth=1, position='identity', alpha=0.67)

# and another...
ggplot(data, aes(x=burstInt, fill=factor(voicing))) +
	geom_histogram(binwidth=1, position='identity', alpha=0.67)

# closureIntensity and burstIntensity are likely to be correlated;
# create a scatter plot to examine correlation
ggplot(data, aes(x=burstInt, y=closureInt, colour=factor(voicing))) +
	geom_point(size=3, alpha=0.75)

# hexagonal 2D histograms
ggplot(data, aes(x=burstInt, y=closureInt, fill=factor(voicing))) +
	geom_hex(aes(alpha=..count..), bins=35) +
	scale_alpha_continuous(range=c(0.3,1))

# ...with scales
ggplot(data, aes(x=burstInt, y=closureInt, fill=factor(voicing))) +
	geom_hex(aes(alpha=..count..), bins=35) +
	scale_x_continuous('Burst intensity (dB HL)') +
	scale_y_continuous('Closure Intensity (dB HL)') +
	scale_fill_discrete('Voicing') +
	scale_alpha_continuous('Count', range=c(0.3,1))


##############################################################################

# Continuous variables, model fits, and smoothing

# Load data
data <- as.data.frame(read.delim(file='sampleData2.txt', sep='\t'))
head(data)

# Get mean and standard error as a function of VOT
nsub <- length(unique(data$subject))
subMeans <- ddply(data, .(subject, vot), summarize, n1Amplitude=mean(n1Amplitude))
avgData <- ddply(subMeans, .(vot), summarize,
	meanN1Amp=mean(n1Amplitude),
	seN1Amp=sd(n1Amplitude)/sqrt(nsub))
avgData

# Basic plot
ggplot(avgData, aes(x=vot, y=meanN1Amp)) +
	geom_point(size=3) +
	geom_errorbar(width=0, size=0.5, aes(ymin=meanN1Amp-seN1Amp, ymax=meanN1Amp+seN1Amp)) +
	scale_x_continuous('VOT (ms)', breaks=seq(0,40,5)) +
	scale_y_continuous('N1 amplitude (uV)')

# Smoothing spline
library(splines)
library(MASS)
ggplot(avgData, aes(x=vot, y=meanN1Amp)) +
	geom_point(size=3) +
	geom_smooth(method='lm', size=0.5, formula=y~ns(x,2))


# Adding model fits to figures

# Set up data frames
nsub <- length(unique(data$subject))
subMeans <- ddply(data, .(subject, vot), summarize, n1Amplitude=mean(n1Amplitude))
subMeans$subject <- as.factor(subMeans$subject)
subMeans$vot <- as.numeric(scale(subMeans$vot, scale=F))
avgData <- ddply(subMeans, .(vot), summarize,
	meanN1Amp=mean(n1Amplitude),
	seN1Amp=sd(n1Amplitude)/sqrt(nsub))

# Fit model and add predicted values to avgData data frame
model <- lm(n1Amplitude ~ 1 + vot, data=subMeans)
fit <- predict(model, newdata=avgData, se=F)
avgData$fit <- fit

# Plot data + model
ggplot(avgData, aes(x=vot)) +
	geom_smooth(aes(y=fit), stat='identity') +
	geom_point(aes(y=meanN1Amp), size=3) +
	geom_errorbar(width=0, size=0.5, aes(ymin=meanN1Amp-seN1Amp, ymax=meanN1Amp+seN1Amp)) +
	scale_x_continuous('VOT (ms)', breaks=c(-20,-10,0,10,20), labels=c(0,10,20,30,40)) +
	scale_y_continuous('N1 amplitude (uV)')

# Alternatively, you can fit the model from within ggplot using geom_smooth
# Note: make sure you know what ggplot is computing for you!
ggplot(subMeans, aes(x=vot, y=n1Amplitude)) +
	geom_smooth(method='lm', se=F) +
	geom_point(stat='summary', fun.y='mean', size=3) +
	geom_errorbar(width=0, size=0.5, stat='identity', data=avgData, 
		aes(y=meanN1Amp, ymin=meanN1Amp-seN1Amp, ymax=meanN1Amp+seN1Amp)) +
	scale_x_continuous('VOT (ms)', breaks=c(-20,-10,0,10,20), labels=c(0,10,20,30,40)) +
	scale_y_continuous('N1 amplitude (uV)')


##############################################################################

# Formatting and saving figures (example with time-course data)

# Load data
data <- as.data.frame(read.delim(file='sampleData3.txt', sep='\t'))
head(data)

# Basic plot
ggplot(data, aes(x=time, y=voltage, colour=factor(distance))) +
	geom_line()

# 'ERPified' plot
# Note: the font sizes below may be too large or too small depending on the size of your
# figure window; you can adjust these to different values if needed
ggplot(data, aes(x=time, y=voltage, colour=factor(distance))) +
	geom_hline(yintercept=0, colour='#333333', size=0.25) +
	geom_vline(xintercept=0, colour='#333333', size=0.25) +
	geom_line(size=1) +
	scale_x_continuous('Time (ms)', breaks=c(0,250,500,750)) +
	scale_y_reverse('Voltage (uV)', breaks=c(-4,0,4,8)) +
	scale_colour_hue('Distance\nfrom boundary\n(VOT steps)', direction=-1, h=c(60,220)) +
	coord_cartesian(xlim=c(-190,1000)) +
	theme_bw() +
	theme(
		panel.border = element_blank(),
		legend.key = element_blank(),
		panel.grid.major = element_blank(),
		panel.grid.minor = element_blank(),
		axis.ticks = element_line(colour='#333333', size=0.25),
		axis.text = element_text(size=8),
		axis.title = element_text(size=8),
		legend.text = element_text(size=8),
		legend.title = element_text(size=8, face='plain')
	)

# Saving figures: simplest approach is to set up the figure window with the 
# dimensions you want, plot your data, and run 'ggsave' with just the filename.
# It will output a file with the contents of the active figure window.
ggsave('figure.pdf')

# You can also specify the figure size, but this may produce unexpected results
# depending on font sizes, line widths, etc.
ggsave('figure.pdf', width=4, height=4)

