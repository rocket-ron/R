# Correcting common coding issues in data

# We'll look at a dataset of video attributes
# from Youtube and prepare it for further study.
# This data is taken from webcrawls conducted 
# by Cheng, Dale, and Liu at Simon Fraser University. 
# Details of their methodology can be found at 
# http://netsg.cs.sfu.ca/youtubedata/

Videos = read.delim("videos.txt")
summary(Videos)

# Let's examine the categories variable
summary(Videos$category)

# Notice the mysterious first entry in the summary,
# which has a number but no text
# We can confirm this by checking the values of this variable
levels(Videos$category)

# It looks like missing categories are incorrectly 
# coded as empty strings.  We should correct these to NA's
Videos$category[Videos$category == ""] = NA

# the 9 data points are gone, but the "" still 
# appears as a level.
summary(Videos$category)

# Can fix this by recreating the factor in R
Videos$category = factor(Videos$category)

# Next, let's look at the rate variable, which
# represents the average rating given to a video on a
# 5-point scale.  We would typically begin by looking
# at a histogram
hist(Videos$rate)

# Notice the surprising peak around zero - is that meaningful?

# Let's check if these values are exactly zero, or just close to zero.
# Create a vector of videos that have a zero rating exactly
zero_rate = Videos$rate == 0

# And see how many of these videos there are
summary(zero_rate)

# We may check to see how many ratings these videos have.
# According to the documentation for the dataset, the number
# of ratings a video has is coded in the ratings vector
Videos$ratings[zero_rate]

# See all the zeros?  We conclude that videos with no 
# ratings are being coded
# with an average rating of zero.  This would throw off
# our means and other statistics
# get the vector of videos with no ratings
no_rate = Videos$ratings == 0

# and recode the rate variable as missing for these videos
Videos$rate[no_rate] = NA


# Let's similarly inspect the age variable
hist(Videos$age)
# Notice the peak at age 0

# get the vector of videos with age zero to look closer
zero_age = Videos$age == 0

# could it be that these are the unrated videos again?
Videos$ratings[zero_age]

# Let's look at all of the columns to see if anything
# jumps out about these datapoints
# We need to subset the dataset, pulling out just the rows
# which for the zero-age videos
Videos[Videos$age == 0,]

# Notice that they all come from the UNA category
# We can check that all Videos in UNA also have zero age
Videos$age[Videos$category == " UNA "]


# At this point, I went and read the documentation
# on the researchers' website.  The age is supposed to
# show days from the formation of Youtube
# to the time the video was uploaded.

# Because we don't expect many videos the day
# youtube was created, this confirms out suspicion
# that the zeros represent missing values
# This also suggests that the UNA category represents
# missing values, so lets recode all of these as NAs
Videos$age[zero_age] = NA
Videos$category[zero_age] = NA
Videos$category = factor(Videos$category)

# Moreover, for an age variable, we'd rather 
# measure from the day the video was created to
# the day the data was gathered.
# We can do that by subtracting the maximum age from each
# video's age.
Videos$age = max(Videos$age, na.rm = T)- Videos$age

# Let's see if that looks more like what we'd expect
hist(Videos$age)

# Finally, let's save our dataframe so we can study it
# more later
save(Videos, file= "Videos_clean.Rdata")

