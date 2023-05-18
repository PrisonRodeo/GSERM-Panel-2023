# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# INTRODUCTION                                 ####
#
# This is some code from "Analyzing Panel 
# Data," designed to provide a basic 
# introduction to the R language.
#
# NOTE: Anything you see to the right of a "#"
# (hashtag) is a comment; it's here to explain
# the code. R won't read it. If you're viewing
# this code in RStudio, the comments usually
# appear as green text.
#
# File initially created June 11, 2021
#
# File last updated May 18, 2023
#
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# OBJECTS                                 #########
#
# R is an "object oriented" language, and the
# most basic thing one does in R is an "assignment."
# An assignment is essentially telling R that 
# a bit (or lot) of information is attached to
# a particular thing in the "environment." 
# So, to assign the number "6" to an object
# named "A" we do:

A <- 6

# Now, whenever you refer to "A",
# you're referring to the number 6.

A  # returns "6" in the window below.
# The square braces and [1] indicate
# that this is a single value / scalar
# (one row, one column)
#
# Note that everything to the right of a #
# is a comment; it appears here, in the
# code, but R ignores it. Putting lots
# of comments in your code is like leaving
# little gifts for your future self, who
# will go back to old code and have a 
# much better idea of what you were thinking
# when you wrote it.

A + 5  # returns "11" in the window below.

A^4    # returns "1296" (6x6x6x6).

"A" # prints "A". Using quotes means
# "take me literally; actually return
# whatever is in the quotes."

# If we want to get rid of A, we can remove 
# it:

rm(A)

# Objects can have multiple elements.
# Here is the Penn State women's volleyball
# team's overall winning percentages, 
# from 2005-2021:

PSUWVB<-c(31/34,32/35,34/36,38/38,38/38,
          32/37,25/32,33/36,34/36,36/39,
          28/34,24/34,33/35,25/32,27/33,
          10/16,21/32)

# The "c()" says "combine these values into
# a vector or list." Note the new object 
# called "PSUWVB" in the "Environment" window.
#
# We can list this object:

PSUWVB

# We can do transformations on it:

PSUWVB * 100

# Note that we now have numbers in the 
# square braces that indicate the position
# of each element (number) in the object;
# the first one is [1], the second is [2],
# etc. We can assign those transformations 
# to other objects:

WinPct <- PSUWVB * 100

# We can also combine objects; usually, we
# do this by creating a "data frame."
# Think of a data frame as like an Excel
# spreadsheet. 
#
# So, we might create another object that lists
# the years from 2005 to 2021, in order:

Year <- c(2005,2006,2007,2008,2009,
          2010,2011,2012,2013,2014,
          2015,2016,2017,2018,2019,
          2020,2021)

Year

# Note that a faster way to do this is to 
# use the "seq()" (for "sequence") command:

Year <- seq(2005,2021)

# We just "overwrote" the old "Year" object
# with the new one. This happens without
# warning, so be careful.
#
# Now we can combine these two objects into
# a single data frame, using the
# "data.frame" command:

VB <- data.frame(Year = Year,
                 WinProp = PSUWVB,
                 WinPct = WinPct)

# Note that there's now a new data frame
# called "DF" in the Environment window;
# it has 17 observations (rows) and
# three variables (columns).
#
# NOTE: If you want to "see" a data frame,
# you can simply click on it in the
# Environment window; this has the same
# effect as running "View()""

View(VB)

# Also important is the fact that there are
# different **types** of objects. Some of the
# most commonly-used ones are:
#
# - Numeric objects (numeric scalars and vectors):

foo<-c(1,2,3,4,5)
is.numeric(foo)

# - Character objects (letters and things, including
#                      most text):

bar<-"We apply law to facts. We don't apply feelings to facts."
is.character(bar)

# - Matrices (blocks of numbers):

baz<-matrix(c(1,2,3,4,5,6),nrow=3)
is.matrix(baz)

# - Data frames (we talked about these back on
#                line 112 or so.)
#
# - Lists (collections of other objects; lists
#          can be complicated, so don't sweat them
#          for now.)
#
# There's lots more to say about objects, but 
# that's enough for now. 
#
# =-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# READING IN DATA            ####################
#
# Most of the time, you won't want to enter
# data by hand. Instead, you will read data
# from something like a spreadsheet into
# Rstudio from some other source file. There
# are many ways to do this.
#
# RStudio can read data in many different
# formats, but the simplest is ".csv" 
# ("comma-separated values"), which is
# simply an Excel-type spreadsheet that 
# has been converted into plain text, with
# "cells" of the spreadsheet separated 
# from each other by a comma.
#
# One way we can read a file is to have it
# "locally" on our own machine. The line at
# the top of the "Console" window below 
# tells you where "local" is; at the moment,
# it's just "~/", the "root" of the drive.
# We can change this by clicking the "Session"
# menu option above, or by using "setwd". So, 
# for example:

setwd("~/Dropbox (Personal)/GSERM/Panel-2023")

# Reading the data is then easy:

SCOTUS <- read.csv("SCOTUS-votes.csv")

# (Note that this command does not work unless
# you have the data stored "locally".) 
#
# These data are on U.S. Supreme Court 
# justices who have served since 1946.
# The first column is the justice's name,
# the second is the percentage of cases
# involving civil rights and liberties in
# which s/he voted in a pro-civil rights
# direction, and the third column records 
# whether (=1) or not (=0) the justice
# was appointed by a Republican president.
#
# We can also read the same data directly
# from the web, using a URL for the file.
# This requires a bit more complexity.
# To do this, we first need to install an
# R "package" called "readr" that can handle
# URLs. 

install.packages("readr")

# Once we've installed the package (that
# is, put it on our machine) we then need
# to load it up for use:

library(readr)

# This is the part that actually gets 
# the data:

SCOTUS <- read_csv("https://raw.githubusercontent.com/PrisonRodeo/GSERM-Panel-APD/master/Data/SCOTUS-votes.csv")

# From there, we can do things like "look
# at" the data:

View(SCOTUS)

# ...and summarize it:

summary(SCOTUS)

# FWIW, those are data on U.S. Supreme Court justices'
# liberal voting percentages in civil rights and liberties
# cases, over their entire careers, along with the
# party identification (Republican -- that's "GOP" -- or
# Democratic) of the president who appointed each
# justice.

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# SINGLING OUT ELEMENTS                ###############
#
# A data frame is rows and columns. We can
# extract rows, columns, or cells by specifying
# which one we need using the square-brace
# ("[]") notation. Watch what happens when we
# print SCOTUS:

SCOTUS

# The notation "FOO[R,C]" denotes the Rth
# row and the Cth column of the object called
# "FOO." So if we wanted to get Samuel Alito's
# row from the SCOTUS data, we could enter:

SCOTUS[24,]

# The fact that we left the second value
# blank means "give me all the columns
# from that row." Similarly, if we just 
# wanted the column with the justices' 
# voting percentages, we would enter:

SCOTUS[,2]

# which means "give me the values of 
# all the rows from column 2 of SCOTUS."
# A single value would then just be:

SCOTUS[24,3]

# which tells us that Alito was appointed 
# by a Republican. This is useful for a
# lot of things; for example, we can use
# it along with conditional statements to
# subset the data, like this:

GOPJustices <- SCOTUS[SCOTUS$GOP==1,]

# That means "Create an object called 
# 'GOPJustices' that consists of all of
# the columns of SCOTUS, but that only
# includes the rows that represent GOP
# appointees." We can see that the command
# worked:

View(GOPJustices)

#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# PLOTTING / GRAPHS               ####################
#
# R / RStudio is fantastic for visualizing and
# plotting / graphing data. This is a fast
# introduction to the most basic graphing
# command, called "plot."
#
# A (very) basic plot of the volleyball team's
# winning percentage by year looks like this:

plot(VB$Year,VB$WinPct,t="l")

# The plot appears at the lower right.
# Note that the dollar-sign "XXX$YYY" 
# notation means "use the element YYY
# from object XXX." We can get around this
# in a few ways, the best probably being
# to use the "with()" command:

with(VB, plot(Year,WinPct,t="l"))

# This says "Use the object called 'VB'
# to draw a plot of the objects 'WinPct'
# and 'Year'." We can add a bunch of things
# to make it nicer looking:

with(VB, plot(Year,WinPct,t="l",lwd=2,col="navy",
      main="PSU Women's Volleyball Winning\nPercentages, 2005-2021",
      xlab="Season",ylab="Winning Percentage"))

# We can do a similar thing with the 
# Supreme Court data:

with(SCOTUS, plot(CRPercent~GOP))

# This is a (bad) "scatterplot"; the values
# of CRPercent are vertical, and the values
# of GOP (either 1 for Republican appointees
# or 0 for Democratic ones) are horizontal.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# LOOPS AND BASIC PROGRAMMING          ###############
# 
# We'll do some work with simulations
# in this course. That means repetition,
# and that means loops. The basic structure
# of a loop in R is:
#
#  for (i in 1:N) {
#     <<activity>>
#  }
#
# This means "Take the activity, and repeat 
# it N times." Note that you don't need to
# use "i" specifically as the index.
#
# A simple example: we'll generate 100 
# "coin flips": draws of either heads (=1)
# or tails (=0) from a binary (bernoulli)
# distribution with P = 0.5:

set.seed(7222009)        # set seed
N <- 100                 # how many flips?
flips <- rbinom(N,1,0.5) # simulate

# (type "?rbinom" for details on this). We
# can summarize this by (e.g.) the number of
# "heads" out of 100:

sum(flips)             # tally up heads

# Now, suppose we wanted to repeat our
# flip-100-coins simulation many (say, 1000)
# times, and each time we want to record 
# the number of heads we get.
#
# First, we need to create a "container" to
# put the head-counts in, one with 1000 bins:

counts <- numeric(1000)   # count storage

# Next, we write a loop that repeats the 
# 100-flip simulation 1000 times, and each 
# time stores the count of "heads" as an
# element of "counts":

NSims <- 1000   # N of simulations
for(i in 1:NSims) {
  flips <- rbinom(N,1,0.5) # simulate
  counts[i] <- sum(flips)  # store count
}

# Now we can take a look at "counts":

summary(counts)
hist(counts)
plot(density(counts))

# Note that one can put loops inside of
# other loops. So, suppose I wanted to
# do the same simulation, but vary the
# number of coins I flipped (say, from
# 10 to 20 to 50 to 100 to 1000). I
# can do that all with a single set
# of nested loops:

NFlips <- c(10,20,50,100,1000) # N of flips
Sums <- data.frame(matrix(ncol=length(NFlips),
                          nrow=NSims))

for(j in 1:length(NFlips)) {
  for(i in 1:NSims) {
    flips <- rbinom(NFlips[j],1,0.5) # simulate
    Sums[i,j] <- sum(flips)  # store count
  }
}

# We can then summarize the results:

summary(Sums)

# ...and we could plot the results, etc.
#
#=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
# GETTING HELP                             ###########
#
# R has extensive documentation, and a help
# function that is available at the command
# line / console. The basic help syntax uses
# a question mark:

?summary

# Note that this will bring up a help file
# on any command that is part of a currently-
# loaded package. If the package is not loaded,
# you'll get an error:

?read.dta

library(foreign)
?read.dta

# One can also do a omnibus search for help on
# a topic; this can be valuable if you know what
# you want to do, but don't know the package
# to do it with. That uses two question marks:

??spss

# The "??" command will work with almost anything:

??sunspots
??tuna
??Belgium

# (This is the end of the code.)