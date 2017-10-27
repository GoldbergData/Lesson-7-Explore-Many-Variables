Lesson 5: Multivariate Analysis
================
Josh Goldberg
October 26, 2017

``` r
library(readr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)
library(data.table)
library(GGally)
```

### Multivariate Data

Notes: This lesson goes beyond bivariate analysis.

------------------------------------------------------------------------

### Moira Perceived Audience Size Colored by Age

Notes: Adding color to represent age, but there was no pattern. Too much overplotting, which makes color as an encoder less effective.

------------------------------------------------------------------------

### Third Qualitative Variable

Notes: Conducting EDA can lead to deadends. Don't panic.

``` r
pf <- read_tsv("pseudo_facebook.tsv")

ggplot(aes(x = gender, y = age),
       data = subset(pf, !is.na(gender))) + 
  geom_boxplot() +
  stat_summary(geom = 'point', shape = 4, fun.y = mean)
```

![](Figs/Third%20Qualitative%20Variable-1.png)

``` r
ggplot(aes(x = age, y = friend_count, color = gender), 
       data = subset(pf, !is.na(gender))) +
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)
```

![](Figs/Third%20Qualitative%20Variable-2.png)

``` r
# Write code to create a new data frame,
# called 'pf.fc_by_age_gender', that contains
# information on each age AND gender group.

# The data frame should contain the following variables:

#    mean_friend_count,
#    median_friend_count,
#    n (the number of users in each age and gender grouping)

# Here is an example of the structure of your data frame. Your
# data values will be different. Note that if you are grouping by
# more than one variable, you will probably need to call the
# ungroup() function. 

#   age gender mean_friend_count median_friend_count    n
# 1  13 female          247.2953                 150  207
# 2  13   male          184.2342                  61  265
# 3  14 female          329.1938                 245  834
# 4  14   male          157.1204                  88 1201

# chain functions to create friend count descriptive stats by groups
pf.fc_by_age_gender <- pf %>%
  filter(!is.na(gender)) %>%
  group_by(age, gender) %>%
  summarise(mean_friend_count = mean(friend_count),
            median_friend_count = median(friend_count),
            n = n())
```

------------------------------------------------------------------------

### Plotting Conditional Summaries

Notes:

``` r
# Create a line graph showing the
# median friend count over the ages
# for each gender. Be sure to use
# the data frame you just created,
# pf.fc_by_age_gender.

ggplot(aes(x = age, y = median_friend_count, color = gender), 
       data = pf.fc_by_age_gender) +
  geom_line()
```

![](Figs/Plotting%20Conditional%20Summaries-1.png)

------------------------------------------------------------------------

### Thinking in Ratios

Notes:

------------------------------------------------------------------------

### Wide and Long Format

Notes: Can transform data via dplyr + tidyr or with reshape library.

------------------------------------------------------------------------

### Reshaping Data

Notes:

``` r
pf.fc_by_age_gender.wide <- dcast(pf.fc_by_age_gender,
                                   age ~ gender,
                                   value.var = 'mean_friend_count')

# try out melt
pf.fc_by_age_gender.long <- melt(pf.fc_by_age_gender.wide,
                                 measure.vars = c('male', 'female'),
                                 value.name = 'age')

# same effect with dplyr and tidyr
pf.fc_by_age_gender.wide <-
  subset(pf.fc_by_age_gender[c('age', 'gender', 'median_friend_count')],
         !is.na(gender)) %>%
  spread(gender, median_friend_count) %>%
  mutate(ratio = female / male)
```

------------------------------------------------------------------------

### Ratio Plot

Notes:

``` r
# Plot the ratio of the female to male median
# friend counts using the data frame
# pf.fc_by_age_gender.wide.

# Think about what geom you should use.
# Add a horizontal line to the plot with
# a y intercept of 1, which will be the
# base line. Look up the documentation
# for geom_hline to do that. Use the parameter
# linetype in geom_hline to make the
# line dashed.

# The linetype parameter can take the values 0-6:
# 0 = blank, 1 = solid, 2 = dashed
# 3 = dotted, 4 = dotdash, 5 = longdash
# 6 = twodash

ggplot(aes(x = age, y = ratio), data = pf.fc_by_age_gender.wide) +
  geom_line() +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = 'dashed')
```

![](Figs/Ratio%20Plot-1.png)

------------------------------------------------------------------------

### Third Quantitative Variable

Notes:

``` r
# Create a variable called year_joined
# in the pf data frame using the variable
# tenure and 2014 as the reference year.

# The variable year joined should contain the year
# that a user joined facebook.

pf$year_joined <- sapply(pf$tenure, function(x, y = 2014) { floor(y - (x / 365)) })

# answer
# pf$year_joined <- floor(2014 - (pf$tenure / 365))
```

------------------------------------------------------------------------

### Cut a Variable

Notes:

``` r
# Create a new variable in the data frame
# called year_joined.bucket by using
# the cut function on the variable year_joined.

# You need to create the following buckets for the
# new variable, year_joined.bucket

#        (2004, 2009]
#        (2009, 2011]
#        (2011, 2012]
#        (2012, 2014]

# Note that a parenthesis means exclude the year and a
# bracket means include the year.

pf$year_joined.bucket <- cut(pf$year_joined, breaks = 
                               c(2004, 2009, 2011, 2012, 2014))
```

------------------------------------------------------------------------

### Plotting it All Together

Notes:

``` r
# Create a line graph of friend_count vs. age
# so that each year_joined.bucket is a line
# tracking the median user friend_count across
# age. This means you should have four different
# lines on your plot.

# You should subset the data to exclude the users
# whose year_joined.bucket is NA.

ggplot(aes(x = age, y = friend_count), 
       data = subset(pf, !is.na(pf$year_joined.bucket))) +
  geom_line(aes(color = factor(year_joined.bucket)), stat = 'summary', fun.y = median)
```

![](Figs/Plotting%20it%20All%20Together-1.png)

------------------------------------------------------------------------

### Plot the Grand Mean

Notes: Use the grand mean for high-level observation on data.

``` r
# Write code to do the following:

# (1) Add another geom_line to code below
# to plot the grand mean of the friend count vs age.

# (2) Exclude any users whose year_joined.bucket is NA.

# (3) Use a different line type for the grand mean.

# As a reminder, the parameter linetype can take the values 0-6:

# 0 = blank, 1 = solid, 2 = dashed
# 3 = dotted, 4 = dotdash, 5 = longdash
# 6 = twodash

ggplot(aes(x = age, y = friend_count), 
       data = subset(pf, !is.na(pf$year_joined.bucket))) +
  geom_line(aes(color = factor(year_joined.bucket)), stat = 'summary', fun.y = mean) +
  geom_line(color = 'black', linetype = 'dashed', stat = 'summary', fun.y = mean)
```

![](Figs/Plot%20the%20Grand%20Mean-1.png)

------------------------------------------------------------------------

### Friending Rate

Notes:

``` r
pf.fc_by_rate <- pf %>%
  subset(tenure > 0 )
pf.fc_by_rate$friend_rate <- pf.fc_by_rate$friend_count / pf.fc_by_rate$tenure
summary(pf.fc_by_rate$friend_rate)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##   0.0000   0.0775   0.2205   0.6096   0.5658 417.0000

``` r
# answer
with(subset(pf, tenure >= 1), summary(friend_count / tenure))
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##   0.0000   0.0775   0.2205   0.6096   0.5658 417.0000

------------------------------------------------------------------------

### Friendships Initiated

Notes:

What is the median friend rate? 0.2205

What is the maximum friend rate? 417.0000

``` r
# Create a line graph of mean of friendships_initiated per day (of tenure)
# vs. tenure colored by year_joined.bucket.

# You need to make use of the variables tenure,
# friendships_initiated, and year_joined.bucket.

# You also need to subset the data to only consider user with at least
# one day of tenure.

ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, pf$tenure >= 1)) +
  geom_line(aes(color = year_joined.bucket), 
            stat = 'summary', fun.y = mean)
```

![](Figs/Friendships%20Initiated-1.png)

------------------------------------------------------------------------

### Bias-Variance Tradeoff Revisited

Notes:

``` r
ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure >= 1)) +
  geom_line(aes(color = year_joined.bucket),
            stat = 'summary',
            fun.y = mean)
```

![](Figs/Bias-Variance%20Tradeoff%20Revisited-1.png)

``` r
ggplot(aes(x = 7 * round(tenure / 7), y = friendships_initiated / tenure),
       data = subset(pf, tenure > 0)) +
  geom_line(aes(color = year_joined.bucket),
            stat = "summary",
            fun.y = mean)
```

![](Figs/Bias-Variance%20Tradeoff%20Revisited-2.png)

``` r
ggplot(aes(x = 30 * round(tenure / 30), y = friendships_initiated / tenure),
       data = subset(pf, tenure > 0)) +
  geom_line(aes(color = year_joined.bucket),
            stat = "summary",
            fun.y = mean)
```

![](Figs/Bias-Variance%20Tradeoff%20Revisited-3.png)

``` r
ggplot(aes(x = 90 * round(tenure / 90), y = friendships_initiated / tenure),
       data = subset(pf, tenure > 0)) +
  geom_line(aes(color = year_joined.bucket),
            stat = "summary",
            fun.y = mean)
```

![](Figs/Bias-Variance%20Tradeoff%20Revisited-4.png)

``` r
ggplot(aes(x = tenure, y = friendships_initiated / tenure),
       data = subset(pf, tenure > 0)) +
  geom_smooth(aes(color = year_joined.bucket))
```

![](Figs/Bias-Variance%20Tradeoff%20Revisited-5.png)

------------------------------------------------------------------------

### Sean's NFL Fan Sentiment Study

Notes: It took averaging to get the story to come out for fan sentiment related to NFL team wins and losses. If data is too noisy, average until you see a reasonable story or a story you'd expect. As he added more data, he had lower variance but more biased data.

------------------------------------------------------------------------

### Introducing the Yogurt Data Set

Notes:

------------------------------------------------------------------------

### Histograms Revisited

Notes: If the binwidth is increased, the discreteness of the data disappears, creating a biased model. You can average too much, which would lead to biasness depending on the underlying characteristics of the data set.

``` r
yo <- fread('yogurt.csv')
dim(yo)
```

    ## [1] 2380    9

``` r
str(yo)
```

    ## Classes 'data.table' and 'data.frame':   2380 obs. of  9 variables:
    ##  $ obs        : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ id         : chr  "2100081" "2100081" "2100081" "2100081" ...
    ##  $ time       : int  9678 9697 9825 9999 10015 10029 10036 10042 10083 10091 ...
    ##  $ strawberry : int  0 0 0 0 1 1 0 0 0 0 ...
    ##  $ blueberry  : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ pina.colada: int  0 0 0 0 1 2 0 0 0 0 ...
    ##  $ plain      : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ mixed.berry: int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ price      : num  59 59 65 65 49 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
glimpse(yo)
```

    ## Observations: 2,380
    ## Variables: 9
    ## $ obs         <int> 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,...
    ## $ id          <chr> "2100081", "2100081", "2100081", "2100081", "21000...
    ## $ time        <int> 9678, 9697, 9825, 9999, 10015, 10029, 10036, 10042...
    ## $ strawberry  <int> 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0,...
    ## $ blueberry   <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ pina.colada <int> 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ plain       <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,...
    ## $ mixed.berry <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 2, 2, 0, 3, 3,...
    ## $ price       <dbl> 58.96, 58.96, 65.04, 65.04, 48.96, 65.04, 65.04, 6...

``` r
head(yo)
```

    ##    obs      id  time strawberry blueberry pina.colada plain mixed.berry
    ## 1:   1 2100081  9678          0         0           0     0           1
    ## 2:   2 2100081  9697          0         0           0     0           1
    ## 3:   3 2100081  9825          0         0           0     0           1
    ## 4:   4 2100081  9999          0         0           0     0           1
    ## 5:   5 2100081 10015          1         0           1     0           1
    ## 6:   6 2100081 10029          1         0           2     0           1
    ##    price
    ## 1: 58.96
    ## 2: 58.96
    ## 3: 65.04
    ## 4: 65.04
    ## 5: 48.96
    ## 6: 65.04

``` r
tail(yo)
```

    ##     obs      id  time strawberry blueberry pina.colada plain mixed.berry
    ## 1: 2735 2169896 10115          0         0           0     1           0
    ## 2: 2736 2169896 10207          0         0           0     1           0
    ## 3: 2737 2169896 10315          0         0           0     1           0
    ## 4: 2738 2169896 10318          0         0           0     1           0
    ## 5: 2742 2170639  9809          0         0           0     1           0
    ## 6: 2743 2170639 10010          0         0           0     1           0
    ##    price
    ## 1: 68.96
    ## 2: 39.04
    ## 3: 68.96
    ## 4: 68.96
    ## 5: 65.04
    ## 6: 65.04

``` r
summary(yo)
```

    ##       obs              id                 time         strawberry     
    ##  Min.   :   1.0   Length:2380        Min.   : 9662   Min.   : 0.0000  
    ##  1st Qu.: 696.5   Class :character   1st Qu.: 9843   1st Qu.: 0.0000  
    ##  Median :1369.5   Mode  :character   Median :10045   Median : 0.0000  
    ##  Mean   :1367.8                      Mean   :10050   Mean   : 0.6492  
    ##  3rd Qu.:2044.2                      3rd Qu.:10255   3rd Qu.: 1.0000  
    ##  Max.   :2743.0                      Max.   :10459   Max.   :11.0000  
    ##    blueberry        pina.colada          plain         mixed.berry    
    ##  Min.   : 0.0000   Min.   : 0.0000   Min.   :0.0000   Min.   :0.0000  
    ##  1st Qu.: 0.0000   1st Qu.: 0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
    ##  Median : 0.0000   Median : 0.0000   Median :0.0000   Median :0.0000  
    ##  Mean   : 0.3571   Mean   : 0.3584   Mean   :0.2176   Mean   :0.3887  
    ##  3rd Qu.: 0.0000   3rd Qu.: 0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
    ##  Max.   :12.0000   Max.   :10.0000   Max.   :6.0000   Max.   :8.0000  
    ##      price      
    ##  Min.   :20.00  
    ##  1st Qu.:50.00  
    ##  Median :65.04  
    ##  Mean   :59.25  
    ##  3rd Qu.:68.96  
    ##  Max.   :68.96

``` r
yo$id <- factor(yo$id)
str(yo$id)
```

    ##  Factor w/ 332 levels "2100081","2100370",..: 1 1 1 1 1 1 1 1 1 1 ...

``` r
ggplot(aes(x = price), data = yo) +
  geom_histogram(binwidth = 1, 
                 fill = I('#F79420'), 
                 color = 'black')
```

![](Figs/Histograms%20Revisited-1.png)

------------------------------------------------------------------------

### Number of Purchases

Notes: Signs of discreetness: 75th percentile is near the maximum; check how many unique prices there are per price point with table.

``` r
# Create a new variable called all.purchases,
# which gives the total counts of yogurt for
# each observation or household.

# One way to do this is using the transform
# function. You can look up the function transform
# and run the examples of code at the bottom of the
# documentation to figure out what it does.

# The transform function produces a data frame
# so if you use it then save the result to 'yo'!

# OR you can figure out another way to create the
# variable.

yo <- transform(yo, all.purchases = strawberry + blueberry + pina.colada
                + plain + mixed.berry)

summary(yo$all.purchases)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   2.000   1.971   2.000  21.000

------------------------------------------------------------------------

### Prices over Time

Notes:

``` r
# Create a scatterplot of price vs time.
# This will be an example of a time series plot.

ggplot(aes(x = time, y = price),
       data = yo) +
  geom_jitter(alpha = 1/4, shape = 21, fill = I('#F79420'))
```

![](Figs/Prices%20over%20Time-1.png)

------------------------------------------------------------------------

### Sampling Observations

Notes: Prices were generally higher at the end of the time period. There was some lumpiness in the data, which could be indicative of possible coupon usage, or experimentation in differently priced yogurts. A few outliers had lower prices towards the end of the time period.

------------------------------------------------------------------------

### Looking at Samples of Households

``` r
# set seed for reproducible results
set.seed(400)
sample.ids <- sample(levels(yo$id), 16)

ggplot(aes(x = time, y = price),
       data = subset(yo, id %in% sample.ids)) +
  facet_wrap( ~ id) +
  geom_line() +
  geom_point(aes(size = all.purchases), pch = 1)
```

![](Figs/Looking%20at%20Sample%20of%20Households-1.png)

------------------------------------------------------------------------

### The Limits of Cross Sectional Data

Notes: Need data over time to analyze behavior of variables over time.

------------------------------------------------------------------------

### Many Variables

Notes: Plotting many variables at once can provide insight into potentially impactful variables that may not have been up interest before.

------------------------------------------------------------------------

### Scatterplot Matrix

Notes:

------------------------------------------------------------------------

``` r
theme_set(theme_minimal(20))

# set seed for reproducible results
set.seed(1836)
pf_subset <- pf[, c(2:15)]
names(pf_subset)
```

    ##  [1] "age"                   "dob_day"              
    ##  [3] "dob_year"              "dob_month"            
    ##  [5] "gender"                "tenure"               
    ##  [7] "friend_count"          "friendships_initiated"
    ##  [9] "likes"                 "likes_received"       
    ## [11] "mobile_likes"          "mobile_likes_received"
    ## [13] "www_likes"             "www_likes_received"

``` r
ggpairs(pf_subset[sample.int(nrow(pf_subset), 1000), ], axisLabels = 'internal')
```

![](Figs/Scatterplot%20Matrix-1.png)

### Even More Variables

Notes:

------------------------------------------------------------------------

### Heat Maps

Notes:

``` r
nci <- read.table("nci.tsv")
colnames(nci) <- c(1:64)
```

``` r
nci.long.samp <- melt(as.matrix(nci[1:200,]))
names(nci.long.samp) <- c("gene", "case", "value")
head(nci.long.samp)
```

    ##   gene case  value
    ## 1    1    1  0.300
    ## 2    2    1  1.180
    ## 3    3    1  0.550
    ## 4    4    1  1.140
    ## 5    5    1 -0.265
    ## 6    6    1 -0.070

``` r
ggplot(aes(y = gene, x = case, fill = value),
  data = nci.long.samp) +
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c("blue", "red"))(100))
```

![](Figs/Heat%20Map-1.png)

------------------------------------------------------------------------

### Analyzing Three of More Variables

Reflection: Learning how to demonsionize multiple variables to determine what's important. Sampling datasets, and smoothing datasets to glean insights. Learned drawbacks of overfitting when smoothing too much. Also the importance of time-series data for analyzing trends.

------------------------------------------------------------------------
