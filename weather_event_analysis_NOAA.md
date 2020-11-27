Reproducible Research - Course Project 2
----------------------------------------

NOAA Storm Database - Weather Events in US from 1950 to 2011
============================================================

Author: Linh Dang Date: 18 Jan 2020

This is my submission for the Coure Project 2 in Reproducible Reserach
couse from JHU on Coursera.

Synopsis
--------

In this report, our aim is to investigate the dataset from U.S. National
Oceanic and Atmospheric Administration’s (NOAA) storm database and try
to give anwers to these 2 major questions: 1. Which types of events are
the most harmful with respect to population health? 2. Which types of
events have the greatest economic consequences?

The investigated dataset is a collection of records of weather events
from 1950 to 2011, which include their characteristics along with
estimated fatalities, injuries, property and crop damage that they had
caused. We perform data analysis on this dataset, then extract the top 5
as answers for each question of interest.

The resulting analysis reveals that the weather event types which are
most harmful to population health are
`LIGHTNING`,`FLOOD`,`TSTM WIND`,`EXCESSIVE HEAT`, and `TORNADO`.
Meanwhile, the ones causing greatest economic damage are `HAIL`,
`STORM SURGE`, `TORNADO`, `HURRICANE/TYPHOON`, and `FLOOD`.

Loading and Processing Data
---------------------------

The dataset we would use is obtained from U.S. National Oceanic and
Atmospheric Administration’s (NOAA) storm database. A copy of this
dataset is accessible from
[here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2).

For more information about the dataset and complete database, reference
to this
[documentation](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2Fpd01016005curr.pdf)
and
[FAQ](https://d396qusza40orc.cloudfront.net/repdata%2Fpeer2_doc%2FNCDC%20Storm%20Events-FAQ%20Page.pdf).

This data file is a `.bz2` compressed file. A common practice is
unzipping it (with or without using R function). However, in this case,
I read the data directly to R using `read.csv()`. The unzipped data file
would be a comma-separated value (`.csv`) file.

Assuming that the .bz2 has already in the working directory:

    data <- read.csv('repdata_data_StormData.csv.bz2')

We load some packages to use later:

    library(dplyr) # my favorite package, for data manupulation 
    library(ggplot2) # for plotting

After reading data, we obtain a data frame containing 902,297
observations with 37 variables (features). Take a quick look to the
loaded data:

    str(data)

    ## 'data.frame':    902297 obs. of  37 variables:
    ##  $ STATE__   : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ BGN_DATE  : Factor w/ 16335 levels "1/1/1966 0:00:00",..: 6523 6523 4242 11116 2224 2224 2260 383 3980 3980 ...
    ##  $ BGN_TIME  : Factor w/ 3608 levels "00:00:00 AM",..: 272 287 2705 1683 2584 3186 242 1683 3186 3186 ...
    ##  $ TIME_ZONE : Factor w/ 22 levels "ADT","AKS","AST",..: 7 7 7 7 7 7 7 7 7 7 ...
    ##  $ COUNTY    : num  97 3 57 89 43 77 9 123 125 57 ...
    ##  $ COUNTYNAME: Factor w/ 29601 levels "","5NM E OF MACKINAC BRIDGE TO PRESQUE ISLE LT MI",..: 13513 1873 4598 10592 4372 10094 1973 23873 24418 4598 ...
    ##  $ STATE     : Factor w/ 72 levels "AK","AL","AM",..: 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ EVTYPE    : Factor w/ 985 levels "   HIGH SURF ADVISORY",..: 834 834 834 834 834 834 834 834 834 834 ...
    ##  $ BGN_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ BGN_AZI   : Factor w/ 35 levels "","  N"," NW",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ BGN_LOCATI: Factor w/ 54429 levels "","- 1 N Albion",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ END_DATE  : Factor w/ 6663 levels "","1/1/1993 0:00:00",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ END_TIME  : Factor w/ 3647 levels ""," 0900CST",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ COUNTY_END: num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ COUNTYENDN: logi  NA NA NA NA NA NA ...
    ##  $ END_RANGE : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ END_AZI   : Factor w/ 24 levels "","E","ENE","ESE",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ END_LOCATI: Factor w/ 34506 levels "","- .5 NNW",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ LENGTH    : num  14 2 0.1 0 0 1.5 1.5 0 3.3 2.3 ...
    ##  $ WIDTH     : num  100 150 123 100 150 177 33 33 100 100 ...
    ##  $ F         : int  3 2 2 2 2 2 2 1 3 3 ...
    ##  $ MAG       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ FATALITIES: num  0 0 0 0 0 0 0 0 1 0 ...
    ##  $ INJURIES  : num  15 0 2 2 2 6 1 0 14 0 ...
    ##  $ PROPDMG   : num  25 2.5 25 2.5 2.5 2.5 2.5 2.5 25 25 ...
    ##  $ PROPDMGEXP: Factor w/ 19 levels "","-","?","+",..: 17 17 17 17 17 17 17 17 17 17 ...
    ##  $ CROPDMG   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ CROPDMGEXP: Factor w/ 9 levels "","?","0","2",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ WFO       : Factor w/ 542 levels ""," CI","$AC",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ STATEOFFIC: Factor w/ 250 levels "","ALABAMA, Central",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ ZONENAMES : Factor w/ 25112 levels "","                                                                                                               "| __truncated__,..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ LATITUDE  : num  3040 3042 3340 3458 3412 ...
    ##  $ LONGITUDE : num  8812 8755 8742 8626 8642 ...
    ##  $ LATITUDE_E: num  3051 0 0 0 0 ...
    ##  $ LONGITUDE_: num  8806 0 0 0 0 ...
    ##  $ REMARKS   : Factor w/ 436781 levels "","-2 at Deer Park\n",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ REFNUM    : num  1 2 3 4 5 6 7 8 9 10 ...

    head(data)

    ##   STATE__           BGN_DATE BGN_TIME TIME_ZONE COUNTY COUNTYNAME STATE  EVTYPE BGN_RANGE
    ## 1       1  4/18/1950 0:00:00     0130       CST     97     MOBILE    AL TORNADO         0
    ## 2       1  4/18/1950 0:00:00     0145       CST      3    BALDWIN    AL TORNADO         0
    ## 3       1  2/20/1951 0:00:00     1600       CST     57    FAYETTE    AL TORNADO         0
    ## 4       1   6/8/1951 0:00:00     0900       CST     89    MADISON    AL TORNADO         0
    ## 5       1 11/15/1951 0:00:00     1500       CST     43    CULLMAN    AL TORNADO         0
    ## 6       1 11/15/1951 0:00:00     2000       CST     77 LAUDERDALE    AL TORNADO         0
    ##   BGN_AZI BGN_LOCATI END_DATE END_TIME COUNTY_END COUNTYENDN END_RANGE END_AZI END_LOCATI
    ## 1                                               0         NA         0                   
    ## 2                                               0         NA         0                   
    ## 3                                               0         NA         0                   
    ## 4                                               0         NA         0                   
    ## 5                                               0         NA         0                   
    ## 6                                               0         NA         0                   
    ##   LENGTH WIDTH F MAG FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG CROPDMGEXP WFO
    ## 1   14.0   100 3   0          0       15    25.0          K       0               
    ## 2    2.0   150 2   0          0        0     2.5          K       0               
    ## 3    0.1   123 2   0          0        2    25.0          K       0               
    ## 4    0.0   100 2   0          0        2     2.5          K       0               
    ## 5    0.0   150 2   0          0        2     2.5          K       0               
    ## 6    1.5   177 2   0          0        6     2.5          K       0               
    ##   STATEOFFIC ZONENAMES LATITUDE LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
    ## 1                          3040      8812       3051       8806              1
    ## 2                          3042      8755          0          0              2
    ## 3                          3340      8742          0          0              3
    ## 4                          3458      8626          0          0              4
    ## 5                          3412      8642          0          0              5
    ## 6                          3450      8748          0          0              6

The original dataset is fairly tidy. However, considering columns
relating to property and crop damage (respectively `PROPDMG` and
`CROPDMG`), we see that they are both followed by columns indicating the
magtitude of damage (i.e. `PROPDMGEXP` and `CROPDMGEXP`).

For the convenience of comparison, we modify these columns so that the
damage is represented as **numeric values**.

As referred to Section 2.7, page 12 of the documentation, there are 3
characters used to signify the magtitude of damage (“K” for “thousands”,
“M” for “millions”, “B” for “billions”). We would use them as the basis
to convert damage values.

**NOTES:** If we dig a little bit deeper to `PROPDMGEXP` and
`CROPDMGEXP`, it is clear that these 2 variables do not contain only 3
kinds of values. For example, if we look at `PROPDMGEXP`, it is a factor
variable with more than 3 levels.

    unique(data$PROPDMGEXP)

    ##  [1] K M   B m + 0 5 6 ? 4 2 3 h 7 H - 1 8
    ## Levels:  - ? + 0 1 2 3 4 5 6 7 8 B h H K m M

Nonetheless, we lack the information about the magtitudes that the
remaining characters signify. Therefore, I choose to treat them as
“noise”/NAs. We would ignore them in the process of analysis.

I take advantage of `case_when()` in `dplyr` package that we have
imported. Here we create a new column containing converted values by
multiply the `PROPDMG`/`CROPDMG` with the corresponding magtitudes.

    #create PROPDMGVAL - property damage value 

    data <- data %>% mutate(PROPDMGVAL = case_when(
      data$PROPDMGEXP == 'K' ~ 1000,
      data$PROPDMGEXP == 'M' ~ 1000000,
      data$PROPDMGEXP == 'B' ~ 1000000000,
      TRUE                   ~ NA_real_
      
    )*data$PROPDMG)


    # similarly, create CROPDMGVAL - crop damage value

    data <- data %>% mutate(CROPDMGVAL = case_when(
      data$CROPDMGEXP == 'K' ~ 1000,
      data$CROPDMGEXP == 'M' ~ 1000000,
      data$CROPDMGEXP == 'B' ~ 1000000000,
      TRUE                   ~ NA_real_
      
    )*data$CROPDMG)

Results
-------

We are interested in which types of events creating greatest damage in
particular categories (i.e. most detrimental to population health and
most destructive in terms of economic consequences). Thus, I subset the
dataset to create a new datas, which includes only the columns we should
devote close attention to.

Let’s create a new dataset with only the columns of interest. They are
`EVTYPE`, `FATALITIES`, `INJURIES`, along with two newly created
`PROPDMGVAL` and `CROPDMGVAL`.

    newdata <- data %>% select(EVTYPE,FATALITIES,INJURIES,PROPDMGVAL,CROPDMGVAL)

    head(newdata)

    ##    EVTYPE FATALITIES INJURIES PROPDMGVAL CROPDMGVAL
    ## 1 TORNADO          0       15      25000         NA
    ## 2 TORNADO          0        0       2500         NA
    ## 3 TORNADO          0        2      25000         NA
    ## 4 TORNADO          0        2       2500         NA
    ## 5 TORNADO          0        2       2500         NA
    ## 6 TORNADO          0        6       2500         NA

### A. Which types of events are most harmful to human health?

There are numerous types of weather event in the dataset. We hence
restrict the definition of “harmful to human health” to the number of
“fatalities” and “injuries” that they have caused.

Let’s group our new data by event type, then calculate the total
fatalities and injuries in each type.

    harmful_group <- newdata %>% group_by(EVTYPE) %>% summarise(fatalities = sum(FATALITIES, na.rm = TRUE),injuries = sum(INJURIES, na.rm = TRUE))

We obtain a new data frame with types of events and their total
fatalities and injuries.

Now we could sort the data to find out which types are the most harmful
ones.

The multi-level sorting method I use is as follows: first, sort by the
total, then sort by the number of fatalities (since they are more
severe).

We create a new colum for the sum of fatalities and injuries:

    harmful_group <- harmful_group %>% mutate(total = fatalities + injuries)

Sort data based on the mentioned method:

    harmful_group <- harmful_group %>% arrange(desc(total), desc(fatalities))

We could limit the most harmful ones to top 5. Extract them to make the
plot:

    most_harmful <- head(harmful_group, n = 5)
    most_harmful

    ## # A tibble: 5 x 4
    ##   EVTYPE         fatalities injuries total
    ##   <fct>               <dbl>    <dbl> <dbl>
    ## 1 TORNADO              5633    91346 96979
    ## 2 EXCESSIVE HEAT       1903     6525  8428
    ## 3 TSTM WIND             504     6957  7461
    ## 4 FLOOD                 470     6789  7259
    ## 5 LIGHTNING             816     5230  6046

Plot the data obtained:

    g1 <- ggplot(most_harmful, aes(reorder(EVTYPE,total), total, fill = EVTYPE))

    g1 + geom_bar(stat = "identity", width = 0.8, position = position_dodge(width = 0.9)) + geom_text(aes(label = most_harmful$total), vjust = -0.5, size = 3.5) + ylim(0,100000) + labs(x = 'Event Type', y = 'Sum of Fatalities and Injuries', title = 'Top 5 Most Harmful Event Types to Population Health \n (1950 - 2011)') + theme(axis.text.x = element_text(size = 8.5), plot.title = element_text(hjust = 0.5))

![Fig 1: Most Harmful
Types](weather_event_analysis_NOAA_files/figure-markdown_strict/plot_1-1.png)

### B. Which types of event have greatest economic consequences?

Similarly, we would select top 5 types of event that cause greatest
damage by calculating the sum of property damage and crop damage.

Create a new data frame by grouping `newdata` by event types then
calculate the sum of property and crop damage in each type.

    damage_group <- newdata %>% group_by(EVTYPE) %>% summarise(propdmg = sum(PROPDMGVAL, na.rm = TRUE), cropdmg = sum(CROPDMGVAL,na.rm = TRUE))

Create a new column for total damage:

    damage_group <- damage_group %>% mutate(totaldmg = propdmg + cropdmg)

Sort data:

    damage_group <- damage_group %>% arrange(desc(totaldmg))

Extract the top 5:

    most_damage <- head(damage_group, n = 5)
    most_damage

    ## # A tibble: 5 x 4
    ##   EVTYPE                 propdmg    cropdmg     totaldmg
    ##   <fct>                    <dbl>      <dbl>        <dbl>
    ## 1 FLOOD             144657709800 5661968450 150319678250
    ## 2 HURRICANE/TYPHOON  69305840000 2607872800  71913712800
    ## 3 TORNADO            56925660480  414953110  57340613590
    ## 4 STORM SURGE        43323536000       5000  43323541000
    ## 5 HAIL               15727366720 3025537450  18752904170

As we could observe, the values in `totaldmg` column are very large.
Hence, for aesthetic purpose, let’s modify these numbers by shifting the
unit to billion and add comma separator.

    l <- sapply(most_damage$totaldmg, function(x) format(round(x/1000000000,2), big.mark = ",", scientific = FALSE))

Now we can make a plot:

    g2 <- ggplot(most_damage,aes(reorder(EVTYPE,totaldmg),totaldmg, fill = EVTYPE))

    g2 + geom_bar(stat = 'identity', width = 0.7, position = position_dodge(width = 0.7))+ geom_text(aes(label = l), vjust = -0.5, size = 3) + scale_y_continuous(labels = scales::unit_format(unit = "B", scale = 1e-9),limits = c(0,200000000000)) + labs(x = "Event Type", y = "Total Damage (billions)", title = "Top 5 Event Types With Greatest Economic Consequences \n (1950 - 2011)") + theme(axis.text.x = element_text(size = 8.5), plot.title = element_text(hjust = 0.5))

![Fig 2: Types Causing Greatest
Damage](weather_event_analysis_NOAA_files/figure-markdown_strict/plot_2-1.png)
