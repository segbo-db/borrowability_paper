Generate borrowability scores with a probabilistic model of segment
borrowability
================
Steven Moran and Elad Eisen
(09 November, 2021)

-   [Overview](#overview)
    -   [Random sampling](#random-sampling)
    -   [Collapse all inventories](#collapse-all-inventories)
-   [Compare the results](#compare-the-results)
-   [References](#references)

# Overview

This supplementary materials for “Defining and operationalizing
borrowability for language contact research: Phonological segment
borrowing as a case study” uses the R programming language (R Core Team
2021) and the following R libraries (Wickham et al. 2019; Xie 2021).

``` r
library(tidyverse)
library(knitr)
```

Load the data from the [CLDF format](https://cldf.clld.org) (Forkel et
al. 2018) and combine the tables into single dataframes for
[PHOIBLE](https://phoible.org) (Moran and McCloy 2019) and
[SegBo](https://github.com/segbo-db/segbo) (Grossman et al. 2020).

``` r
values <- read_csv('../data/segbo/cldf/values.csv')
languages <- read_csv('../data/segbo/cldf/languages.csv')
segbo <- left_join(values, languages, by=c("Language_ID"="ID"))

values <- read_csv('../data/phoible/cldf/values.csv')
languages <- read_csv('../data/phoible/cldf/languages.csv')
phoible <- left_join(values, languages, by=c("Language_ID"="ID"))
```

A segment’s borrowability factor `bS` is calculated by Eisen (2019, 60):

$$
b\_s = \\frac{P\_s}{f\_s - f\_s^2}
$$

where `Ps` is the probability of borrowing a segment and `fs` is its
typological frequency. Empirical estimations for the probability of
borrowing and the typological frequency are estimated from SegBo and
PHOIBLE, respectively.

Get the typological frequency of segments in phoible.

``` r
num_phoible_inventories <- phoible %>% select(Inventory_ID) %>% distinct() %>% nrow()
phoible_segment_counts <- as.data.frame(table(phoible$Value))
phoible_segment_counts <- phoible_segment_counts %>% rename(Segment = Var1, PhoibleFreq = Freq)
phoible_segment_counts$Segment <- as.character(phoible_segment_counts$Segment)
phoible_segment_counts$PhoibleCrossFreq <- phoible_segment_counts$PhoibleFreq / num_phoible_inventories
```

Get segbo segment frequencies.

``` r
num_segbo_inventories <- segbo %>% select(Inventory_ID) %>% distinct() %>% nrow()
segbo_segment_counts <- as.data.frame(table(segbo$Value))
segbo_segment_counts <- segbo_segment_counts %>% rename(Segment = Var1, SegboFreq = Freq)
segbo_segment_counts$Segment <- as.character(segbo_segment_counts$Segment)
segbo_segment_counts$SegboCrossFreq <- segbo_segment_counts$SegboFreq / num_segbo_inventories
```

Combine segbo and phoible.

``` r
bs_df_new <- left_join(segbo_segment_counts, phoible_segment_counts)
```

    ## Joining, by = "Segment"

Calculate all borrowability scores (can drop low frequency stuff later).

``` r
bs_df_new$BorrowabilityScore <- bs_df_new$SegboCrossFreq / (bs_df_new$PhoibleCrossFreq - bs_df_new$PhoibleCrossFreq ^ 2)
```

Let’s get the top 20 as a table for the publication.

``` r
t <- bs_df_new %>% filter(PhoibleCrossFreq > .05) %>% arrange(desc(BorrowabilityScore)) %>% head(n=20) %>% select(Segment, BorrowabilityScore, PhoibleCrossFreq, SegboCrossFreq)

t <- t %>% rename(Phoneme = Segment, Borrowability = BorrowabilityScore, `Typological Frequency` = PhoibleCrossFreq, `Borrowing Frequency` = SegboCrossFreq)

library(xtable)
# print(xtable(t), include.rownames=FALSE)
```

## Random sampling

The problem, however, is that cross-linguistic frequency is treated as
*all* the data points and not just a unique list of languages. So for
example, segments in either data source (phoible or segbo) are counted
multiple times – consider the four inventories for Akan in phoible.

``` r
phoible %>% filter(Glottocode == "akan1250") %>% group_by(Inventory_ID, Glottocode, ISO639P3code, Name) %>% summarize(SegmentCount = n()) %>% kable()
```

    ## `summarise()` has grouped output by 'Inventory_ID', 'Glottocode', 'ISO639P3code'. You can override using the `.groups` argument.

| Inventory\_ID | Glottocode | ISO639P3code | Name | SegmentCount |
|--------------:|:-----------|:-------------|:-----|-------------:|
|           140 | akan1250   | aka          | Akan |           40 |
|           208 | akan1250   | aka          | Akan |           35 |
|           655 | akan1250   | aka          | Akan |           31 |
|          1245 | akan1250   | aka          | Akan |           60 |

So, segments that are shared across these inventories are counted more
than one for the “same” language.

Let’s try random sampling one inventory per language from phoible and
then calculate the borrowability scores. Later we can compare the
results between various samples.

``` r
phoible %>%
    distinct(Inventory_ID, ISO639P3code) %>%
    group_by(ISO639P3code) %>%
    sample_n(1) %>%
    pull(Inventory_ID) ->
    inventory_ids_sampled_one_per_isocode

phoible %>%
    filter(Inventory_ID %in% inventory_ids_sampled_one_per_isocode) ->
    my_sample

num_phoible_inventories <- my_sample %>% select(Inventory_ID) %>% distinct() %>% nrow()
phoible_segment_counts <- as.data.frame(table(my_sample$Value))
phoible_segment_counts <- phoible_segment_counts %>% rename(Segment = Var1, PhoibleFreq = Freq)
phoible_segment_counts$Segment <- as.character(phoible_segment_counts$Segment)
phoible_segment_counts$PhoibleCrossFreq <- phoible_segment_counts$PhoibleFreq / num_phoible_inventories
```

We’ll use the same segbo counts from above, even though they produce the
same problem, i.e. there are multiple reports (“inventories”) for the
same language.

Combine segbo and random phoible.

``` r
bs_df_random <- left_join(segbo_segment_counts, phoible_segment_counts)
```

    ## Joining, by = "Segment"

Calculate all borrowability scores (can drop low frequency stuff later).

``` r
bs_df_random$BorrowabilityScore <- bs_df_random$SegboCrossFreq / (bs_df_random$PhoibleCrossFreq - bs_df_random$PhoibleCrossFreq ^ 2)
```

Let’s have a look.

``` r
bs_df_random %>% filter(PhoibleCrossFreq > .05) %>% arrange(desc(BorrowabilityScore)) %>% head(n=25) %>% kable()
```

| Segment | SegboFreq | SegboCrossFreq | PhoibleFreq | PhoibleCrossFreq | BorrowabilityScore |
|:--------|----------:|---------------:|------------:|-----------------:|-------------------:|
| f       |       148 |      0.2792453 |         923 |        0.4397332 |          1.1334483 |
| p       |        48 |      0.0905660 |        1801 |        0.8580276 |          0.7434646 |
| d̠ʒ      |        75 |      0.1415094 |         583 |        0.2777513 |          0.7054115 |
| ɡ       |        84 |      0.1584906 |        1205 |        0.5740829 |          0.6481921 |
| t̠ʃ      |        71 |      0.1339623 |         845 |        0.4025727 |          0.5569973 |
| z       |        60 |      0.1132075 |         626 |        0.2982373 |          0.5409077 |
| ʒ       |        34 |      0.0641509 |         295 |        0.1405431 |          0.5310915 |
| b       |        64 |      0.1207547 |        1337 |        0.6369700 |          0.5222069 |
| v       |        54 |      0.1018868 |         578 |        0.2753692 |          0.5106058 |
| h       |        64 |      0.1207547 |        1204 |        0.5736065 |          0.4937186 |
| k       |        19 |      0.0358491 |        1914 |        0.9118628 |          0.4460555 |
| d       |        54 |      0.1018868 |        1005 |        0.4787994 |          0.4082812 |
| j       |        19 |      0.0358491 |        1893 |        0.9018580 |          0.4050277 |
| r       |        52 |      0.0981132 |         955 |        0.4549786 |          0.3956607 |
| x       |        30 |      0.0566038 |         369 |        0.1757980 |          0.3906590 |
| ʃ       |        46 |      0.0867925 |         733 |        0.3492139 |          0.3819022 |
| l       |        42 |      0.0792453 |        1464 |        0.6974750 |          0.3755636 |
| ɸ       |         9 |      0.0169811 |         106 |        0.0505002 |          0.3541427 |
| q       |        12 |      0.0226415 |         161 |        0.0767032 |          0.3197059 |
| s       |        34 |      0.0641509 |        1434 |        0.6831825 |          0.2963856 |
| ɾ       |        30 |      0.0566038 |         541 |        0.2577418 |          0.2958731 |
| χ       |         9 |      0.0169811 |         133 |        0.0633635 |          0.2861254 |
| ɣ       |        17 |      0.0320755 |         280 |        0.1333969 |          0.2774644 |
| w       |        18 |      0.0339623 |        1771 |        0.8437351 |          0.2575901 |
| o       |        31 |      0.0584906 |        1331 |        0.6341115 |          0.2520992 |

## Collapse all inventories

Nikolaev in BSD model 2 simply collapses all of the duplicate
inventories into “single” languages, instead of randomizing or
bootstrapping the samples. Let’s do the same here for phoible and segbo.
Then we can compare the original, randomly sampled, and
all-inventories-as-one language approaches and the resulting
borrowability scores.

``` r
collapsed_inventories <- phoible %>% group_by(ISO639P3code) %>% select(ISO639P3code, Value)

num_phoible_inventories <- collapsed_inventories %>% select(ISO639P3code) %>% distinct() %>% nrow()
phoible_segment_counts <- as.data.frame(table(my_sample$Value))
phoible_segment_counts <- phoible_segment_counts %>% rename(Segment = Var1, PhoibleFreq = Freq)
phoible_segment_counts$Segment <- as.character(phoible_segment_counts$Segment)
phoible_segment_counts$PhoibleCrossFreq <- phoible_segment_counts$PhoibleFreq / num_phoible_inventories
```

And do the same for segbo.

``` r
segbo_collapsed <- segbo %>% group_by(ISO639P3code, Value)
num_segbo_inventories <- segbo %>% select(ISO639P3code) %>% distinct() %>% nrow()
segbo_segment_counts <- as.data.frame(table(segbo$Value))
segbo_segment_counts <- segbo_segment_counts %>% rename(Segment = Var1, SegboFreq = Freq)
segbo_segment_counts$Segment <- as.character(segbo_segment_counts$Segment)
segbo_segment_counts$SegboCrossFreq <- segbo_segment_counts$SegboFreq / num_segbo_inventories
```

Combine them and get the borrowability score.

``` r
bs_df_collapsed <- left_join(segbo_segment_counts, phoible_segment_counts)
```

    ## Joining, by = "Segment"

``` r
bs_df_collapsed$BorrowabilityScore <- bs_df_collapsed$SegboCrossFreq / (bs_df_collapsed$PhoibleCrossFreq - bs_df_collapsed$PhoibleCrossFreq ^ 2)
```

Let’s have a look.

``` r
bs_df_collapsed %>% filter(PhoibleCrossFreq > .05) %>% arrange(desc(BorrowabilityScore)) %>% head(n=25) %>% kable()
```

| Segment | SegboFreq | SegboCrossFreq | PhoibleFreq | PhoibleCrossFreq | BorrowabilityScore |
|:--------|----------:|---------------:|------------:|-----------------:|-------------------:|
| f       |       148 |      0.2989899 |         923 |        0.4397332 |          1.2135911 |
| p       |        48 |      0.0969697 |        1801 |        0.8580276 |          0.7960328 |
| d̠ʒ      |        75 |      0.1515152 |         583 |        0.2777513 |          0.7552890 |
| ɡ       |        84 |      0.1696970 |        1205 |        0.5740829 |          0.6940239 |
| t̠ʃ      |        71 |      0.1434343 |         845 |        0.4025727 |          0.5963810 |
| z       |        60 |      0.1212121 |         626 |        0.2982373 |          0.5791537 |
| ʒ       |        34 |      0.0686869 |         295 |        0.1405431 |          0.5686435 |
| b       |        64 |      0.1292929 |        1337 |        0.6369700 |          0.5591306 |
| v       |        54 |      0.1090909 |         578 |        0.2753692 |          0.5467092 |
| h       |        64 |      0.1292929 |        1204 |        0.5736065 |          0.5286280 |
| k       |        19 |      0.0383838 |        1914 |        0.9118628 |          0.4775948 |
| d       |        54 |      0.1090909 |        1005 |        0.4787994 |          0.4371496 |
| j       |        19 |      0.0383838 |        1893 |        0.9018580 |          0.4336661 |
| r       |        52 |      0.1050505 |         955 |        0.4549786 |          0.4236367 |
| x       |        30 |      0.0606061 |         369 |        0.1757980 |          0.4182813 |
| ʃ       |        46 |      0.0929293 |         733 |        0.3492139 |          0.4089054 |
| l       |        42 |      0.0848485 |        1464 |        0.6974750 |          0.4021186 |
| ɸ       |         9 |      0.0181818 |         106 |        0.0505002 |          0.3791831 |
| q       |        12 |      0.0242424 |         161 |        0.0767032 |          0.3423113 |
| s       |        34 |      0.0686869 |        1434 |        0.6831825 |          0.3173422 |
| ɾ       |        30 |      0.0606061 |         541 |        0.2577418 |          0.3167935 |
| χ       |         9 |      0.0181818 |         133 |        0.0633635 |          0.3063565 |
| ɣ       |        17 |      0.0343434 |         280 |        0.1333969 |          0.2970830 |
| w       |        18 |      0.0363636 |        1771 |        0.8437351 |          0.2758035 |
| o       |        31 |      0.0626263 |        1331 |        0.6341115 |          0.2699244 |

# Compare the results

Compare the frequencies.

``` r
bs_df_new_cut <- bs_df_new %>% select(Segment, PhoibleCrossFreq, BorrowabilityScore)
bs_df_collapsed_cut <- bs_df_collapsed %>% select(Segment, BorrowabilityScore)%>% rename(BorrowabilityScore_collapsed = BorrowabilityScore)
bs_df_random_cut <- bs_df_random %>% select(Segment, BorrowabilityScore) %>% rename(BorrowabilityScore_random = BorrowabilityScore)

results <- left_join(bs_df_new_cut, bs_df_collapsed_cut)
```

    ## Joining, by = "Segment"

``` r
results <- left_join(results, bs_df_random_cut)
```

    ## Joining, by = "Segment"

Have a look.

``` r
results %>% filter(PhoibleCrossFreq > .05) %>% arrange(desc(BorrowabilityScore)) %>% head(n=25) %>% kable()
```

| Segment | PhoibleCrossFreq | BorrowabilityScore | BorrowabilityScore\_collapsed | BorrowabilityScore\_random |
|:--------|-----------------:|-------------------:|------------------------------:|---------------------------:|
| f       |        0.4403974 |          1.1330821 |                     1.2135911 |                  1.1334483 |
| p       |        0.8586093 |          0.7460172 |                     0.7960328 |                  0.7434646 |
| d̠ʒ      |        0.2715232 |          0.7154228 |                     0.7552890 |                  0.7054115 |
| ɡ       |        0.5668874 |          0.6455142 |                     0.6940239 |                  0.6481921 |
| t̠ʃ      |        0.4033113 |          0.5566655 |                     0.5963810 |                  0.5569973 |
| z       |        0.2956954 |          0.5435886 |                     0.5791537 |                  0.5409077 |
| b       |        0.6311258 |          0.5186924 |                     0.5591306 |                  0.5222069 |
| v       |        0.2701987 |          0.5166900 |                     0.5467092 |                  0.5106058 |
| h       |        0.5639073 |          0.4910408 |                     0.5286280 |                  0.4937186 |
| ʒ       |        0.1582781 |          0.4815191 |                     0.5686435 |                  0.5310915 |
| k       |        0.9036424 |          0.4117135 |                     0.4775948 |                  0.4460555 |
| d       |        0.4556291 |          0.4107821 |                     0.4371496 |                  0.4082812 |
| r       |        0.4410596 |          0.3979832 |                     0.4236367 |                  0.3956607 |
| j       |        0.8993377 |          0.3959937 |                     0.4336661 |                  0.4050277 |
| ʃ       |        0.3655629 |          0.3742237 |                     0.4089054 |                  0.3819022 |
| x       |        0.1900662 |          0.3676977 |                     0.4182813 |                  0.3906590 |
| l       |        0.6768212 |          0.3622902 |                     0.4021186 |                  0.3755636 |
| ɸ       |        0.0506623 |          0.3530705 |                     0.3791831 |                  0.3541427 |
| ð       |        0.0529801 |          0.3384500 |                     0.4007277 |                  0.3742646 |
| ɾ       |        0.2562914 |          0.2969673 |                     0.3167935 |                  0.2958731 |
| q       |        0.0847682 |          0.2918376 |                     0.3423113 |                  0.3197059 |
| s       |        0.6692053 |          0.2897912 |                     0.3173422 |                  0.2963856 |
| ɣ       |        0.1443709 |          0.2596617 |                     0.2970830 |                  0.2774644 |
| χ       |        0.0711921 |          0.2568084 |                     0.3063565 |                  0.2861254 |
| o       |        0.6046358 |          0.2446778 |                     0.2699244 |                  0.2520992 |

Let’s write the data to disk.

``` r
write_csv(results, 'borrowability_scores.csv')
```

Compare the segment borrowability rankings.

``` r
bs_df_new_cut <- bs_df_new %>% filter(PhoibleCrossFreq > .05) %>% select(Segment, PhoibleCrossFreq, BorrowabilityScore) %>% arrange(desc(BorrowabilityScore))

bs_df_new_cut$OriginalRank <- seq.int(nrow(bs_df_new_cut))

bs_df_collapsed_cut <- bs_df_collapsed %>% filter(PhoibleCrossFreq > .05) %>% select(Segment, BorrowabilityScore) %>% arrange(desc(BorrowabilityScore))

bs_df_collapsed_cut$CollapsedRank <- seq.int(nrow(bs_df_collapsed_cut))

bs_df_random_cut <- bs_df_random %>% filter(PhoibleCrossFreq > .05) %>% select(Segment, BorrowabilityScore) %>% arrange(desc(BorrowabilityScore))

bs_df_random_cut$RandomizedRank <- seq.int(nrow(bs_df_random_cut))

results <- left_join(bs_df_new_cut, bs_df_collapsed_cut, by = c("Segment"="Segment"))
results <- left_join(results, bs_df_random_cut, by = c("Segment"="Segment"))
```

Let’s have a look.

``` r
tmp <- results %>% select(-BorrowabilityScore, -BorrowabilityScore.x, -BorrowabilityScore.y)
tmp %>% kable()
```

| Segment | PhoibleCrossFreq | OriginalRank | CollapsedRank | RandomizedRank |
|:--------|-----------------:|-------------:|--------------:|---------------:|
| f       |        0.4403974 |            1 |             1 |              1 |
| p       |        0.8586093 |            2 |             2 |              2 |
| d̠ʒ      |        0.2715232 |            3 |             3 |              3 |
| ɡ       |        0.5668874 |            4 |             4 |              4 |
| t̠ʃ      |        0.4033113 |            5 |             5 |              5 |
| z       |        0.2956954 |            6 |             6 |              6 |
| b       |        0.6311258 |            7 |             8 |              8 |
| v       |        0.2701987 |            8 |             9 |              9 |
| h       |        0.5639073 |            9 |            10 |             10 |
| ʒ       |        0.1582781 |           10 |             7 |              7 |
| k       |        0.9036424 |           11 |            11 |             11 |
| d       |        0.4556291 |           12 |            12 |             12 |
| r       |        0.4410596 |           13 |            14 |             14 |
| j       |        0.8993377 |           14 |            13 |             13 |
| ʃ       |        0.3655629 |           15 |            16 |             16 |
| x       |        0.1900662 |           16 |            15 |             15 |
| l       |        0.6768212 |           17 |            17 |             17 |
| ɸ       |        0.0506623 |           18 |            18 |             18 |
| ð       |        0.0529801 |           19 |            NA |             NA |
| ɾ       |        0.2562914 |           20 |            21 |             21 |
| q       |        0.0847682 |           21 |            19 |             19 |
| s       |        0.6692053 |           22 |            20 |             20 |
| ɣ       |        0.1443709 |           23 |            23 |             23 |
| χ       |        0.0711921 |           24 |            22 |             22 |
| o       |        0.6046358 |           25 |            25 |             25 |
| w       |        0.8221854 |           26 |            24 |             24 |
| ts      |        0.2208609 |           27 |            26 |             26 |
| dz      |        0.1033113 |           28 |            28 |             28 |
| e       |        0.6096026 |           29 |            27 |             27 |
| ʔ       |        0.3748344 |           30 |            29 |             29 |
| ŋ       |        0.6284768 |           31 |            32 |             32 |
| m       |        0.9652318 |           32 |            33 |             33 |
| ʁ       |        0.0513245 |           33 |            NA |             NA |
| ɲ       |        0.4158940 |           34 |            34 |             34 |
| c       |        0.1384106 |           35 |            35 |             35 |
| ɟ       |        0.1218543 |           36 |            36 |             36 |
| y       |        0.0579470 |           37 |            NA |             NA |
| β       |        0.1013245 |           38 |            38 |             38 |
| æ       |        0.0738411 |           39 |            37 |             37 |
| ə       |        0.2235099 |           40 |            41 |             41 |
| ɽ       |        0.0592715 |           41 |            39 |             39 |
| mb      |        0.1046358 |           42 |            42 |             42 |
| pʰ      |        0.1963576 |           43 |            40 |             40 |
| ɛ       |        0.3738411 |           44 |            43 |             43 |
| nd      |        0.0970199 |           45 |            45 |             45 |
| ɔː      |        0.1019868 |           46 |            46 |             46 |
| t       |        0.6834437 |           47 |            44 |             44 |
| ɔ       |        0.3543046 |           48 |            49 |             49 |
| oː      |        0.2099338 |           49 |            52 |             52 |
| pʼ      |        0.0592715 |           50 |            48 |             48 |
| ŋɡ      |        0.0960265 |           51 |            54 |             54 |
| tʰ      |        0.1337748 |           52 |            47 |             47 |
| ʂ       |        0.0655629 |           53 |            50 |             50 |
| kʰ      |        0.2006623 |           54 |            51 |             51 |
| ɑ       |        0.0745033 |           55 |            55 |             55 |
| t̠ʃʰ     |        0.0758278 |           56 |            53 |             53 |
| kp      |        0.1235099 |           57 |            60 |             60 |
| ɗ       |        0.0821192 |           58 |            62 |             62 |
| ɖ       |        0.0850993 |           59 |            56 |             56 |
| d̪       |        0.1440397 |           60 |            57 |             57 |
| eː      |        0.2122517 |           61 |            61 |             61 |
| ɪ       |        0.1470199 |           62 |            59 |             59 |
| n       |        0.7781457 |           63 |            58 |             58 |
| t̪       |        0.2344371 |           64 |            63 |             63 |
| ɛː      |        0.1069536 |           65 |            65 |             65 |
| ɭ       |        0.1188742 |           66 |            66 |             66 |
| u       |        0.8761589 |           67 |            64 |             64 |
| ɳ       |        0.1321192 |           68 |            69 |             69 |
| t̠ʃʼ     |        0.0612583 |           69 |            67 |             67 |
| ʊ       |        0.1354305 |           70 |            70 |             70 |
| ɡʷ      |        0.0629139 |           71 |            71 |             71 |
| tsʰ     |        0.0645695 |           72 |            68 |             68 |
| ʈ       |        0.1592715 |           73 |            72 |             72 |
| ɨ       |        0.1625828 |           74 |            75 |             75 |
| aː      |        0.2953642 |           75 |            76 |             76 |
| ɔ̃       |        0.0758278 |           76 |            78 |             78 |
| n̪       |        0.1764901 |           77 |            73 |             73 |
| ɛ̃       |        0.0794702 |           78 |            79 |             79 |
| kʼ      |        0.0804636 |           79 |            74 |             74 |
| o̞       |        0.0956954 |           80 |            77 |             77 |
| ɓ       |        0.0993377 |           81 |            80 |             80 |
| õ       |        0.1142384 |           82 |            81 |             81 |
| uː      |        0.2937086 |           83 |            82 |             82 |
| kʷ      |        0.1231788 |           84 |            85 |             85 |
| iː      |        0.3178808 |           85 |            84 |             84 |
| ɡb      |        0.1238411 |           86 |            86 |             86 |
| a       |        0.8609272 |           87 |            83 |             83 |
| ã       |        0.1725166 |           88 |            87 |             87 |
| ĩ       |        0.1794702 |           89 |            88 |             88 |

Let’s visualize the differences in ranks that are different. First we
need to get the data into a format for plotting.

``` r
diff_ranks <- tmp %>% select(Segment, OriginalRank, CollapsedRank) %>% filter(OriginalRank != CollapsedRank)

x <- diff_ranks %>% select(Segment, OriginalRank)
x$RankType <- 'OriginalRank'
x <- x %>% rename(Rank = OriginalRank)

y <- diff_ranks %>% select(Segment, CollapsedRank)
y$RankType <- 'CollapsedRank'
y <- y %>% rename(Rank = CollapsedRank)

z <- rbind(x, y)
```

Now we plot using a slope graph.

``` r
ggplot(data = z, aes(x = RankType, y = Rank, group = Segment)) +
  geom_line(aes(color = Segment, alpha = 1), size = 2) +
  geom_point(aes(color = Segment, alpha = 1), size = 4) +
#  scale_y_discrete(limits = rev(levels(Rank))) +
  #  Labelling as desired
  labs(
    title = "Original versus collapsed inventories",
    # subtitle = "(Mainstreet Research)",
    # caption = "https://www.mainstreetresearch.ca/gap-between-ndp-and-pcs-narrows-while-liberals-hold-steady/"
  )
```

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

This is quite a bit. Let’s plot the deltas of more than 3 rank changes.

``` r
diff_ranks$delta <- abs(diff_ranks$OriginalRank - diff_ranks$CollapsedRank)
top_deltas <- diff_ranks %>% arrange(desc(delta)) %>% filter(delta > 3)
to_plot <- z %>% filter(Segment %in% top_deltas$Segment)

ggplot(data = to_plot, aes(x = RankType, y = Rank, group = Segment)) +
  geom_line(aes(color = Segment, alpha = 1), size = 2) +
  geom_point(aes(color = Segment, alpha = 1), size = 4) +
#  scale_y_discrete(limits = rev(levels(Rank))) +
  #  Labelling as desired
  labs(
    title = "Original versus collapsed inventories",
    # subtitle = "(Mainstreet Research)",
    # caption = "https://www.mainstreetresearch.ca/gap-between-ndp-and-pcs-narrows-while-liberals-hold-steady/"
  )
```

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

Also perhaps not so helpful. How about in terms of the more frequent
segments with a delta.

``` r
tmp$OCdelta <- abs(tmp$OriginalRank - tmp$CollapsedRank)
tmp_by_freq_rank <- tmp %>% filter(OCdelta > 0) %>% arrange(desc(PhoibleCrossFreq, OriginalRank)) %>% head(n=10)

x <- tmp_by_freq_rank %>% select(Segment, OriginalRank)
x$RankType <- 'OriginalRank'
x <- x %>% rename(Rank = OriginalRank)

y <- tmp_by_freq_rank %>% select(Segment, CollapsedRank)
y$RankType <- 'CollapsedRank'
y <- y %>% rename(Rank = CollapsedRank)

z <- rbind(x, y)

ggplot(data = z, aes(x = RankType, y = Rank, group = Segment)) +
  geom_line(aes(color = Segment, alpha = 1), size = 2) +
  geom_point(aes(color = Segment, alpha = 1), size = 4) +
#  scale_y_discrete(limits = rev(levels(Rank))) +
  #  Labelling as desired
  labs(
    title = "Original versus collapsed inventories",
    # subtitle = "(Mainstreet Research)",
    # caption = "https://www.mainstreetresearch.ca/gap-between-ndp-and-pcs-narrows-while-liberals-hold-steady/"
  )
```

![](README_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Eisen2019" class="csl-entry">

Eisen, Elad. 2019. “The Typology of Phonological Segment Borrowing.”
Masters thesis, Jerusalem, Israel: Hebrew University of Jerusalem.

</div>

<div id="ref-Forkel_etal2018" class="csl-entry">

Forkel, Robert, Johann-Mattis List, Simon J. Greenhill, Christoph
Rzymski, Sebastian Bank, Michael Cysouw, Harald Hammarström, Martin
Haspelmath, Gereon A. Kaiping, and Russell D. Gray. 2018.
“Cross-Linguistic Data Formats, Advancing Data Sharing and Re-Use in
Comparative Linguistics.” *Scientific Data* 5: 180205.

</div>

<div id="ref-segbo" class="csl-entry">

Grossman, Eitan, Elad Eisen, Dmitry Nikolaev, and Steven Moran. 2020.
“SegBo: A Database of Borrowed Sounds in the World’s Language.” In
*Proceedings of the 12th Language Resources and Evaluation Conference*,
5316–22.

</div>

<div id="ref-MoranMcCloy2019" class="csl-entry">

Moran, Steven, and Daniel McCloy, eds. 2019. *PHOIBLE 2.0*. Jena: Max
Planck Institute for the Science of Human History.
<https://doi.org/10.5281/zenodo.2562766>.

</div>

<div id="ref-R" class="csl-entry">

R Core Team. 2021. *R: A Language and Environment for Statistical
Computing*. Vienna, Austria: R Foundation for Statistical Computing.
<https://www.R-project.org/>.

</div>

<div id="ref-tidyverse" class="csl-entry">

Wickham, Hadley, Mara Averick, Jennifer Bryan, Winston Chang, Lucy
D’Agostino McGowan, Romain François, Garrett Grolemund, et al. 2019.
“Welcome to the <span class="nocase">tidyverse</span>.” *Journal of Open
Source Software* 4 (43): 1686. <https://doi.org/10.21105/joss.01686>.

</div>

<div id="ref-knitr" class="csl-entry">

Xie, Yihui. 2021. *Knitr: A General-Purpose Package for Dynamic Report
Generation in r*. <https://yihui.org/knitr/>.

</div>

</div>
