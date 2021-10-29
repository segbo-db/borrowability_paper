Generate borrowability scores with a probabilistic model of segment
borrowability
================
Steven Moran and Elad Eisen
(29 October, 2021)

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
| f       |       148 |      0.2787194 |         929 |        0.4425917 |          1.1297712 |
| p       |        48 |      0.0903955 |        1796 |        0.8556455 |          0.7318509 |
| d̠ʒ      |        75 |      0.1412429 |         582 |        0.2772749 |          0.7048278 |
| ɡ       |        84 |      0.1581921 |        1204 |        0.5736065 |          0.6467853 |
| t̠ʃ      |        71 |      0.1337100 |         841 |        0.4006670 |          0.5568165 |
| z       |        60 |      0.1129944 |         618 |        0.2944259 |          0.5439238 |
| ʒ       |        34 |      0.0640301 |         296 |        0.1410195 |          0.5285935 |
| b       |        64 |      0.1205273 |        1335 |        0.6360172 |          0.5206378 |
| v       |        55 |      0.1035782 |         577 |        0.2748928 |          0.5196400 |
| h       |        64 |      0.1205273 |        1197 |        0.5702716 |          0.4918239 |
| k       |        19 |      0.0357815 |        1909 |        0.9094807 |          0.4346347 |
| d       |        54 |      0.1016949 |        1004 |        0.4783230 |          0.4075457 |
| r       |        52 |      0.0979284 |         950 |        0.4525965 |          0.3952666 |
| j       |        19 |      0.0357815 |        1885 |        0.8980467 |          0.3908038 |
| x       |        30 |      0.0564972 |         368 |        0.1753216 |          0.3907570 |
| ʃ       |        46 |      0.0866290 |         734 |        0.3496903 |          0.3809425 |
| l       |        43 |      0.0809793 |        1436 |        0.6841353 |          0.3747407 |
| q       |        12 |      0.0225989 |         162 |        0.0771796 |          0.3172977 |
| ɾ       |        30 |      0.0564972 |         529 |        0.2520248 |          0.2997066 |
| s       |        34 |      0.0640301 |        1417 |        0.6750834 |          0.2919141 |
| ɣ       |        17 |      0.0320151 |         270 |        0.1286327 |          0.2856287 |
| χ       |         9 |      0.0169492 |         137 |        0.0652692 |          0.2778135 |
| w       |        18 |      0.0338983 |        1762 |        0.8394474 |          0.2515168 |
| o       |        31 |      0.0583804 |        1316 |        0.6269652 |          0.2496171 |
| ts      |        21 |      0.0395480 |         464 |        0.2210576 |          0.2296751 |

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
| f       |       148 |      0.3155650 |         929 |        0.4425917 |          1.2791226 |
| p       |        48 |      0.1023454 |        1796 |        0.8556455 |          0.8285988 |
| d̠ʒ      |        75 |      0.1599147 |         582 |        0.2772749 |          0.7980034 |
| ɡ       |        84 |      0.1791045 |        1204 |        0.5736065 |          0.7322878 |
| t̠ʃ      |        71 |      0.1513859 |         841 |        0.4006670 |          0.6304255 |
| z       |        60 |      0.1279318 |         618 |        0.2944259 |          0.6158285 |
| ʒ       |        34 |      0.0724947 |         296 |        0.1410195 |          0.5984716 |
| b       |        64 |      0.1364606 |        1335 |        0.6360172 |          0.5894641 |
| v       |        55 |      0.1172708 |         577 |        0.2748928 |          0.5883344 |
| h       |        64 |      0.1364606 |        1197 |        0.5702716 |          0.5568411 |
| k       |        19 |      0.0405117 |        1909 |        0.9094807 |          0.4920918 |
| d       |        54 |      0.1151386 |        1004 |        0.4783230 |          0.4614216 |
| r       |        52 |      0.1108742 |         950 |        0.4525965 |          0.4475193 |
| j       |        19 |      0.0405117 |        1885 |        0.8980467 |          0.4424666 |
| x       |        30 |      0.0639659 |         368 |        0.1753216 |          0.4424135 |
| ʃ       |        46 |      0.0980810 |         734 |        0.3496903 |          0.4313017 |
| l       |        43 |      0.0916844 |        1436 |        0.6841353 |          0.4242800 |
| q       |        12 |      0.0255864 |         162 |        0.0771796 |          0.3592433 |
| ɾ       |        30 |      0.0639659 |         529 |        0.2520248 |          0.3393266 |
| s       |        34 |      0.0724947 |        1417 |        0.6750834 |          0.3305040 |
| ɣ       |        17 |      0.0362473 |         270 |        0.1286327 |          0.3233877 |
| χ       |         9 |      0.0191898 |         137 |        0.0652692 |          0.3145393 |
| w       |        18 |      0.0383795 |        1762 |        0.8394474 |          0.2847664 |
| o       |        31 |      0.0660981 |        1316 |        0.6269652 |          0.2826156 |
| ts      |        21 |      0.0447761 |         464 |        0.2210576 |          0.2600373 |

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
| f       |        0.4403974 |          1.1309483 |                     1.2791226 |                  1.1297712 |
| p       |        0.8586093 |          0.7446123 |                     0.8285988 |                  0.7318509 |
| d̠ʒ      |        0.2715232 |          0.7140754 |                     0.7980034 |                  0.7048278 |
| ɡ       |        0.5668874 |          0.6442985 |                     0.7322878 |                  0.6467853 |
| t̠ʃ      |        0.4033113 |          0.5556171 |                     0.6304255 |                  0.5568165 |
| z       |        0.2956954 |          0.5425649 |                     0.6158285 |                  0.5439238 |
| v       |        0.2701987 |          0.5252672 |                     0.5883344 |                  0.5196400 |
| b       |        0.6311258 |          0.5177156 |                     0.5894641 |                  0.5206378 |
| h       |        0.5639073 |          0.4901160 |                     0.5568411 |                  0.4918239 |
| ʒ       |        0.1582781 |          0.4806123 |                     0.5984716 |                  0.5285935 |
| k       |        0.9036424 |          0.4109381 |                     0.4920918 |                  0.4346347 |
| d       |        0.4556291 |          0.4100085 |                     0.4614216 |                  0.4075457 |
| r       |        0.4410596 |          0.3972337 |                     0.4475193 |                  0.3952666 |
| j       |        0.8993377 |          0.3952479 |                     0.4424666 |                  0.3908038 |
| ʃ       |        0.3655629 |          0.3735189 |                     0.4313017 |                  0.3809425 |
| l       |        0.6768212 |          0.3702176 |                     0.4242800 |                  0.3747407 |
| x       |        0.1900662 |          0.3670052 |                     0.4424135 |                  0.3907570 |
| ɸ       |        0.0506623 |          0.3524056 |                     0.4112414 |                  0.3632245 |
| ð       |        0.0529801 |          0.3378126 |                     0.4112414 |                  0.3632245 |
| ɾ       |        0.2562914 |          0.2964080 |                     0.3393266 |                  0.2997066 |
| q       |        0.0847682 |          0.2912880 |                     0.3592433 |                  0.3172977 |
| s       |        0.6692053 |          0.2892454 |                     0.3305040 |                  0.2919141 |
| ɣ       |        0.1443709 |          0.2591727 |                     0.3233877 |                  0.2856287 |
| χ       |        0.0711921 |          0.2563248 |                     0.3145393 |                  0.2778135 |
| o       |        0.6046358 |          0.2442170 |                     0.2826156 |                  0.2496171 |

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
| v       |        0.2701987 |            7 |             9 |              9 |
| b       |        0.6311258 |            8 |             8 |              8 |
| h       |        0.5639073 |            9 |            10 |             10 |
| ʒ       |        0.1582781 |           10 |             7 |              7 |
| k       |        0.9036424 |           11 |            11 |             11 |
| d       |        0.4556291 |           12 |            12 |             12 |
| r       |        0.4410596 |           13 |            13 |             13 |
| j       |        0.8993377 |           14 |            14 |             14 |
| ʃ       |        0.3655629 |           15 |            16 |             16 |
| l       |        0.6768212 |           16 |            17 |             17 |
| x       |        0.1900662 |           17 |            15 |             15 |
| ɸ       |        0.0506623 |           18 |            NA |             NA |
| ð       |        0.0529801 |           19 |            NA |             NA |
| ɾ       |        0.2562914 |           20 |            19 |             19 |
| q       |        0.0847682 |           21 |            18 |             18 |
| s       |        0.6692053 |           22 |            20 |             20 |
| ɣ       |        0.1443709 |           23 |            21 |             21 |
| χ       |        0.0711921 |           24 |            22 |             22 |
| o       |        0.6046358 |           25 |            24 |             24 |
| w       |        0.8221854 |           26 |            23 |             23 |
| ts      |        0.2208609 |           27 |            25 |             25 |
| dz      |        0.1033113 |           28 |            26 |             26 |
| e       |        0.6096026 |           29 |            27 |             27 |
| ʔ       |        0.3748344 |           30 |            29 |             29 |
| ŋ       |        0.6284768 |           31 |            31 |             31 |
| m       |        0.9652318 |           32 |            32 |             32 |
| ʁ       |        0.0513245 |           33 |            NA |             NA |
| ɲ       |        0.4158940 |           34 |            34 |             34 |
| c       |        0.1384106 |           35 |            35 |             35 |
| ɟ       |        0.1218543 |           36 |            36 |             36 |
| y       |        0.0579470 |           37 |            33 |             33 |
| β       |        0.1013245 |           38 |            38 |             38 |
| æ       |        0.0738411 |           39 |            37 |             37 |
| ə       |        0.2235099 |           40 |            40 |             40 |
| ɽ       |        0.0592715 |           41 |            41 |             41 |
| mb      |        0.1046358 |           42 |            42 |             42 |
| pʰ      |        0.1963576 |           43 |            39 |             39 |
| ɛ       |        0.3738411 |           44 |            43 |             43 |
| nd      |        0.0970199 |           45 |            46 |             46 |
| ɔː      |        0.1019868 |           46 |            45 |             45 |
| t       |        0.6834437 |           47 |            44 |             44 |
| ɔ       |        0.3543046 |           48 |            48 |             48 |
| oː      |        0.2099338 |           49 |            51 |             51 |
| pʼ      |        0.0592715 |           50 |            47 |             47 |
| ŋɡ      |        0.0960265 |           51 |            55 |             55 |
| tʰ      |        0.1337748 |           52 |            49 |             49 |
| ʂ       |        0.0655629 |           53 |            50 |             50 |
| kʰ      |        0.2006623 |           54 |            52 |             52 |
| ɑ       |        0.0745033 |           55 |            54 |             54 |
| t̠ʃʰ     |        0.0758278 |           56 |            53 |             53 |
| kp      |        0.1235099 |           57 |            60 |             60 |
| ɗ       |        0.0821192 |           58 |            61 |             61 |
| ɖ       |        0.0850993 |           59 |            56 |             56 |
| d̪       |        0.1440397 |           60 |            57 |             57 |
| eː      |        0.2122517 |           61 |            62 |             62 |
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
| ɔ̃       |        0.0758278 |           76 |            77 |             77 |
| n̪       |        0.1764901 |           77 |            73 |             73 |
| ɛ̃       |        0.0794702 |           78 |            78 |             78 |
| kʼ      |        0.0804636 |           79 |            74 |             74 |
| o̞       |        0.0956954 |           80 |            79 |             79 |
| ɓ       |        0.0993377 |           81 |            81 |             81 |
| õ       |        0.1142384 |           82 |            80 |             80 |
| uː      |        0.2937086 |           83 |            83 |             83 |
| kʷ      |        0.1231788 |           84 |            85 |             85 |
| iː      |        0.3178808 |           85 |            84 |             84 |
| ɡb      |        0.1238411 |           86 |            86 |             86 |
| a       |        0.8609272 |           87 |            82 |             82 |
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

![](README_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

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
