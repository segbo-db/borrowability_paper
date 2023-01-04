Model 1: A probabilistic model of segment borrowability
================
Steven Moran and Elad Eisen
(04 January, 2023)

-   <a href="#overview" id="toc-overview">Overview</a>
-   <a href="#data" id="toc-data">Data</a>
-   <a href="#analysis" id="toc-analysis">Analysis</a>
    -   <a href="#random-sampling" id="toc-random-sampling">Random sampling</a>
    -   <a href="#collapse-all-inventories"
        id="toc-collapse-all-inventories">Collapse all inventories</a>
    -   <a href="#intersection-of-segments-in-phoible-and-segbo"
        id="toc-intersection-of-segments-in-phoible-and-segbo">Intersection of
        segments in PHOIBLE and SEGBO</a>
-   <a href="#compare-the-results" id="toc-compare-the-results">Compare the
    results</a>
-   <a href="#references" id="toc-references">References</a>

<!-- author: "Steven Moran and Elad Eisen" -->

# Overview

This report includes supplementary materials for:

> > > Defining and operationalizing borrowability for language contact
> > > research: Phonological segment borrowing as a case study

And it uses the R programming language (R Core Team 2021) and the
following R libraries (Wickham et al. 2019; Xie 2021).

``` r
library(tidyverse)
library(knitr)
```

# Data

First, load the data from the [CLDF format](https://cldf.clld.org)
(Forkel et al. 2018) and combine the tables into single data frames
including [PHOIBLE](https://phoible.org) (Moran and McCloy 2019) and
[SegBo](https://github.com/segbo-db/segbo) (Grossman et al. 2020).

``` r
values <- read_csv('../data/segbo/cldf/values.csv')
languages <- read_csv('../data/segbo/cldf/languages.csv')
segbo <- left_join(values, languages, by=c("Language_ID"="ID"))

values <- read_csv('../data/phoible/cldf/values.csv')
languages <- read_csv('../data/phoible/cldf/languages.csv')
phoible <- left_join(values, languages, by=c("Language_ID"="ID"))
```

How many languages in SEGBO are also in PHOIBLE? There are currently 498
languages in SegBo, of which 199 are *not* in PHOIBLE.

``` r
tmp1 <- segbo %>% select(Language_ID) %>% distinct()
tmp2 <- phoible %>% select(Language_ID) %>% distinct()
nrow(tmp1)
```

    ## [1] 498

``` r
nrow(tmp1[which(!(tmp1$Language_ID %in% tmp2$Language_ID)),])
```

    ## [1] 199

# Analysis

A segment’s borrowability factor `bS` is calculated by Eisen (2019, 60)
as:

$$
b_s = \frac{P_s}{f_s - f_s^2}
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

Which segments are in segbo that are not in phoible?

``` r
segbo_segment_counts[which(!(segbo_segment_counts$Segment %in% phoible_segment_counts$Segment)),]
```

    ##     Segment SegboFreq SegboCrossFreq
    ## 74       ɨə̯         1    0.001886792
    ## 105     n̺d̺z̺         1    0.001886792
    ## 153       ɹ̤         1    0.001886792
    ## 196     ʊai         1    0.001886792
    ## 214       ʕ̞         1    0.001886792

``` r
# 154   halh1238        Mongolian   svantesson2005mongolian ʊai mand1415 - from Mandarian, perhaps uai
# 285   assa1263    ɹ̤  sans1269 -from Sanskrit
# 304   chra1242    ɨə̯ viet1252-- from Vietnamese
```

Combine SEGBO and PHOIBLE

``` r
bs_df_new <- left_join(segbo_segment_counts, phoible_segment_counts)
```

    ## Joining, by = "Segment"

Calculate all borrowability scores.

``` r
bs_df_new$BorrowabilityScore <- bs_df_new$SegboCrossFreq / (bs_df_new$PhoibleCrossFreq - bs_df_new$PhoibleCrossFreq ^ 2)
# write_csv(bs_df_new, 'all_borrowability_scores.csv')
```

Recall that these borrowability scores are not normalized (i.e. 0–1) and
that the typological frequencies and borrowing frequencies use two
different sets of languages, PHOIBLE and SEGBO samples respectively.
That is, some of the languages from SEGBO are not in PHOIBLE, so
borrowing frequencies can be in excess of their respective typological
frequenies – causing some rare segments borrowability coefficients to be
quite high. For example:

``` r
bs_df_new %>% arrange(desc(BorrowabilityScore)) %>% head(n=20) %>% select(Segment, BorrowabilityScore, PhoibleCrossFreq, SegboCrossFreq) %>% kable()
```

| Segment | BorrowabilityScore | PhoibleCrossFreq | SegboCrossFreq |
|:--------|-------------------:|-----------------:|---------------:|
| ðˤ      |          17.105668 |        0.0006623 |      0.0113208 |
| pˤ      |           5.701889 |        0.0006623 |      0.0037736 |
| ðˠ      |           5.700001 |        0.0003311 |      0.0018868 |
| d̤ɮ̤      |           5.700001 |        0.0003311 |      0.0018868 |
| d̠̤ʒ̤      |           5.700001 |        0.0003311 |      0.0018868 |
| l̪ˤ      |           5.700001 |        0.0003311 |      0.0018868 |
| ʟ       |           5.700001 |        0.0003311 |      0.0018868 |
| ndl     |           5.700001 |        0.0003311 |      0.0018868 |
| n̪ˤ      |           5.700001 |        0.0003311 |      0.0018868 |
| pʃʰ     |           5.700001 |        0.0003311 |      0.0018868 |
| tlʱ     |           5.700001 |        0.0003311 |      0.0018868 |
| tsʲʰ    |           5.700001 |        0.0003311 |      0.0018868 |
| ɕː      |           4.079537 |        0.0023179 |      0.0094340 |
| dˤ      |           4.079537 |        0.0023179 |      0.0094340 |
| zˤ      |           3.570780 |        0.0026490 |      0.0094340 |
| ɮʲ      |           2.852835 |        0.0013245 |      0.0037736 |
| ʡ       |           2.852835 |        0.0013245 |      0.0037736 |
| n̤d̤ɮ̤     |           2.850945 |        0.0006623 |      0.0018868 |
| pʷʰ     |           2.850945 |        0.0006623 |      0.0018868 |
| pʷʼ     |           2.850945 |        0.0006623 |      0.0018868 |

Here we display the top 20 with a cross-linguistic frequency greater
than .05 to get a clearer picture of those segments in both databases.
Below we take the intersection of the two, as per Model 2.

``` r
t <- bs_df_new %>% filter(PhoibleCrossFreq > .05) %>% arrange(desc(BorrowabilityScore)) %>% head(n=20) %>% select(Segment, BorrowabilityScore, PhoibleCrossFreq, SegboCrossFreq)

t <- t %>% rename(Phoneme = Segment, Borrowability = BorrowabilityScore, `Typological Frequency` = PhoibleCrossFreq, `Borrowing Frequency` = SegboCrossFreq)

library(xtable)
# print(xtable(t), include.rownames=FALSE)
t %>% kable()
```

| Phoneme | Borrowability | Typological Frequency | Borrowing Frequency |
|:--------|--------------:|----------------------:|--------------------:|
| f       |     1.1330821 |             0.4403974 |           0.2792453 |
| p       |     0.7460172 |             0.8586093 |           0.0905660 |
| d̠ʒ      |     0.7154228 |             0.2715232 |           0.1415094 |
| ɡ       |     0.6455142 |             0.5668874 |           0.1584906 |
| t̠ʃ      |     0.5566655 |             0.4033113 |           0.1339623 |
| z       |     0.5435886 |             0.2956954 |           0.1132075 |
| b       |     0.5186924 |             0.6311258 |           0.1207547 |
| v       |     0.5166900 |             0.2701987 |           0.1018868 |
| h       |     0.4910408 |             0.5639073 |           0.1207547 |
| ʒ       |     0.4815191 |             0.1582781 |           0.0641509 |
| k       |     0.4117135 |             0.9036424 |           0.0358491 |
| d       |     0.4107821 |             0.4556291 |           0.1018868 |
| r       |     0.3979832 |             0.4410596 |           0.0981132 |
| j       |     0.3959937 |             0.8993377 |           0.0358491 |
| ʃ       |     0.3742237 |             0.3655629 |           0.0867925 |
| x       |     0.3676977 |             0.1900662 |           0.0566038 |
| l       |     0.3622902 |             0.6768212 |           0.0792453 |
| ɸ       |     0.3530705 |             0.0506623 |           0.0169811 |
| ð       |     0.3384500 |             0.0529801 |           0.0169811 |
| ɾ       |     0.2969673 |             0.2562914 |           0.0566038 |

## Random sampling

One problem, however, is that cross-linguistic frequency is treated as
*all* the data points and not just a unique list of languages. So for
example, segments in either data source (PHOIBLE or SEGBO) are counted
multiple times – consider the four inventories for Akan in phoible.

``` r
phoible %>% filter(Glottocode == "akan1250") %>% group_by(Inventory_ID, Glottocode, ISO639P3code, Name) %>% summarize(SegmentCount = n()) %>% kable()
```

    ## `summarise()` has grouped output by 'Inventory_ID', 'Glottocode',
    ## 'ISO639P3code'. You can override using the `.groups` argument.

| Inventory_ID | Glottocode | ISO639P3code | Name | SegmentCount |
|-------------:|:-----------|:-------------|:-----|-------------:|
|          140 | akan1250   | aka          | Akan |           40 |
|          208 | akan1250   | aka          | Akan |           35 |
|          655 | akan1250   | aka          | Akan |           31 |
|         1245 | akan1250   | aka          | Akan |           60 |

So, segments that are shared across these inventories are counted more
than one for the “same” language.

Let’s try random sampling one inventory per language from PHOIBLE and
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

We’ll use the same SEGBO counts from above, even though they produce the
same problem, i.e. there are multiple reports (“inventories”) for the
same language.

Combine SEGBO and random PHOIBLE

``` r
bs_df_random <- left_join(segbo_segment_counts, phoible_segment_counts)
```

    ## Joining, by = "Segment"

Calculate all borrowability scores.

``` r
bs_df_random$BorrowabilityScore <- bs_df_random$SegboCrossFreq / (bs_df_random$PhoibleCrossFreq - bs_df_random$PhoibleCrossFreq ^ 2)
```

Let’s have a look.

``` r
bs_df_random %>% filter(PhoibleCrossFreq > .05) %>% arrange(desc(BorrowabilityScore)) %>% head(n=25) %>% kable()
```

| Segment | SegboFreq | SegboCrossFreq | PhoibleFreq | PhoibleCrossFreq | BorrowabilityScore |
|:--------|----------:|---------------:|------------:|-----------------:|-------------------:|
| f       |       148 |      0.2792453 |         929 |        0.4425917 |          1.1319028 |
| p       |        48 |      0.0905660 |        1803 |        0.8589805 |          0.7476577 |
| d̠ʒ      |        75 |      0.1415094 |         576 |        0.2744164 |          0.7107026 |
| ɡ       |        84 |      0.1584906 |        1207 |        0.5750357 |          0.6485690 |
| t̠ʃ      |        71 |      0.1339623 |         837 |        0.3987613 |          0.5587564 |
| z       |        60 |      0.1132075 |         621 |        0.2958552 |          0.5434183 |
| ʒ       |        34 |      0.0641509 |         294 |        0.1400667 |          0.5326027 |
| b       |        64 |      0.1207547 |        1336 |        0.6364936 |          0.5219128 |
| v       |        54 |      0.1018868 |         578 |        0.2753692 |          0.5106058 |
| h       |        64 |      0.1207547 |        1194 |        0.5688423 |          0.4923524 |
| k       |        19 |      0.0358491 |        1913 |        0.9113864 |          0.4438893 |
| d       |        54 |      0.1018868 |        1011 |        0.4816579 |          0.4080964 |
| j       |        19 |      0.0358491 |        1892 |        0.9013816 |          0.4032841 |
| r       |        52 |      0.0981132 |         950 |        0.4525965 |          0.3960123 |
| x       |        30 |      0.0566038 |         372 |        0.1772273 |          0.3881816 |
| ʃ       |        46 |      0.0867925 |         729 |        0.3473082 |          0.3828765 |
| l       |        42 |      0.0792453 |        1449 |        0.6903287 |          0.3706949 |
| ɸ       |         9 |      0.0169811 |         112 |        0.0533587 |          0.3361829 |
| q       |        12 |      0.0226415 |         163 |        0.0776560 |          0.3161093 |
| ɾ       |        30 |      0.0566038 |         531 |        0.2529776 |          0.2995227 |
| s       |        34 |      0.0641509 |        1433 |        0.6827061 |          0.2961471 |
| χ       |         9 |      0.0169811 |         137 |        0.0652692 |          0.2783376 |
| ɣ       |        17 |      0.0320755 |         279 |        0.1329204 |          0.2783059 |
| w       |        18 |      0.0339623 |        1773 |        0.8446879 |          0.2588780 |
| o       |        31 |      0.0584906 |        1325 |        0.6312530 |          0.2512777 |

## Collapse all inventories

In contrast to the approach above, [Model
2](../neighbor-graph_model/A%20language-graph-based%20model%20of%20segment%20borrowing.ipynb)
simply collapses all of the duplicate inventories into “single”
languages, instead of randomizing or bootstrapping the samples. Let’s do
the same here for PHOIBLE and SEGBO. Then we can compare the original,
randomly sampled, and all-inventories-as-one language approaches and the
resulting borrowability scores.

``` r
collapsed_inventories <- phoible %>% group_by(ISO639P3code) %>% select(ISO639P3code, Value)

num_phoible_inventories <- collapsed_inventories %>% select(ISO639P3code) %>% distinct() %>% nrow()
phoible_segment_counts <- as.data.frame(table(my_sample$Value))
phoible_segment_counts <- phoible_segment_counts %>% rename(Segment = Var1, PhoibleFreq = Freq)
phoible_segment_counts$Segment <- as.character(phoible_segment_counts$Segment)
phoible_segment_counts$PhoibleCrossFreq <- phoible_segment_counts$PhoibleFreq / num_phoible_inventories
```

And do the same for SEGBO

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
| f       |       148 |      0.2995951 |         929 |        0.4425917 |          1.2143897 |
| p       |        48 |      0.0971660 |        1803 |        0.8589805 |          0.8021429 |
| d̠ʒ      |        75 |      0.1518219 |         576 |        0.2744164 |          0.7624947 |
| ɡ       |        84 |      0.1700405 |        1207 |        0.5750357 |          0.6958331 |
| t̠ʃ      |        71 |      0.1437247 |         837 |        0.3987613 |          0.5994755 |
| z       |        60 |      0.1214575 |         621 |        0.2958552 |          0.5830196 |
| ʒ       |        34 |      0.0688259 |         294 |        0.1400667 |          0.5714159 |
| b       |        64 |      0.1295547 |        1336 |        0.6364936 |          0.5599470 |
| v       |        54 |      0.1093117 |         578 |        0.2753692 |          0.5478159 |
| h       |        64 |      0.1295547 |        1194 |        0.5688423 |          0.5282324 |
| k       |        19 |      0.0384615 |        1913 |        0.9113864 |          0.4762375 |
| d       |        54 |      0.1093117 |        1011 |        0.4816579 |          0.4378362 |
| j       |        19 |      0.0384615 |        1892 |        0.9013816 |          0.4326733 |
| r       |        52 |      0.1052632 |         950 |        0.4525965 |          0.4248715 |
| x       |        30 |      0.0607287 |         372 |        0.1772273 |          0.4164702 |
| ʃ       |        46 |      0.0931174 |         729 |        0.3473082 |          0.4107785 |
| l       |        42 |      0.0850202 |        1449 |        0.6903287 |          0.3977091 |
| ɸ       |         9 |      0.0182186 |         112 |        0.0533587 |          0.3606821 |
| q       |        12 |      0.0242915 |         163 |        0.0776560 |          0.3391456 |
| ɾ       |        30 |      0.0607287 |         531 |        0.2529776 |          0.3213502 |
| s       |        34 |      0.0688259 |        1433 |        0.6827061 |          0.3177287 |
| χ       |         9 |      0.0182186 |         137 |        0.0652692 |          0.2986214 |
| ɣ       |        17 |      0.0344130 |         279 |        0.1329204 |          0.2985872 |
| w       |        18 |      0.0364372 |        1773 |        0.8446879 |          0.2777436 |
| o       |        31 |      0.0627530 |        1325 |        0.6312530 |          0.2695894 |

## Intersection of segments in PHOIBLE and SEGBO

As per Model 2’s assumptions, here we take the intersection of languages
in SEGBO and PHOIBLE. Recall above that we calculate how many languages
are in both databases. There are 498 languages in SEGBO and 2177 in
PHOIBLE and 199 languages in SEGBO not in PHOIBLE, leaving 299 languages
that are shared by both databases.

``` r
tmp1 <- segbo %>% select(Language_ID) %>% distinct()
tmp2 <- phoible %>% select(Language_ID) %>% distinct()
nrow(tmp1)
```

    ## [1] 498

``` r
nrow(tmp2)
```

    ## [1] 2177

``` r
nrow(tmp1[which(!(tmp1$Language_ID %in% tmp2$Language_ID)),])
```

    ## [1] 199

Subset PHOIBLE to just the languages in Segbo.

``` r
phoible_intersection <- phoible %>% filter(phoible$Language_ID %in% tmp1$Language_ID)
nrow(phoible_intersection %>% distinct(Language_ID))
```

    ## [1] 299

We note again that PHOIBLE contains multiple doculects for the same
language.

``` r
phoible_intersection %>% group_by(Inventory_ID, Glottocode) %>% select(Language_ID, Glottocode) %>% distinct() %>% arrange(Glottocode) %>% head() %>% kable()
```

    ## Adding missing grouping variables: `Inventory_ID`

| Inventory_ID | Language_ID | Glottocode |
|-------------:|:------------|:-----------|
|         1160 | abau1245    | abau1245   |
|          235 | abip1241    | abip1241   |
|         1914 | abip1241    | abip1241   |
|         1093 | achi1257    | achi1257   |
|          712 | adam1253    | adam1253   |
|         1234 | adam1253    | adam1253   |

We simply collapse these different doculects into single phonological
inventories.

``` r
collapsed_inventories <- phoible_intersection %>% group_by(ISO639P3code) %>% select(ISO639P3code, Value)
```

Nevertheless, we still want the typological frequency of segments from
all of PHOIBLE for our borrowability scores.

``` r
num_phoible_inventories <- phoible %>% select(Inventory_ID) %>% distinct() %>% nrow()
phoible_segment_counts <- as.data.frame(table(phoible$Value))
phoible_segment_counts <- phoible_segment_counts %>% rename(Segment = Var1, PhoibleFreq = Freq)
phoible_segment_counts$Segment <- as.character(phoible_segment_counts$Segment)
phoible_segment_counts$PhoibleCrossFreq <- phoible_segment_counts$PhoibleFreq / num_phoible_inventories
```

We also want just the languages in SEGBO that are in PHOIBLE.

``` r
segbo_intersection <- segbo %>% filter(Language_ID %in% phoible$Language_ID)
segbo %>% select(Language_ID) %>% distinct() %>% nrow()
```

    ## [1] 498

``` r
segbo_intersection %>% select(Language_ID) %>% distinct() %>% nrow() # Should also be 299 like above
```

    ## [1] 299

We also collapse doculects in SEGBO.

``` r
segbo_collapsed <- segbo_intersection %>% group_by(ISO639P3code, Value)
```

Now get their segment counts.

``` r
num_segbo_inventories <- segbo_collapsed %>% select(ISO639P3code) %>% distinct() %>% nrow()
```

    ## Adding missing grouping variables: `Value`

``` r
segbo_segment_counts <- as.data.frame(table(segbo$Value))
segbo_segment_counts <- segbo_segment_counts %>% rename(Segment = Var1, SegboFreq = Freq)
segbo_segment_counts$Segment <- as.character(segbo_segment_counts$Segment)
segbo_segment_counts$SegboCrossFreq <- segbo_segment_counts$SegboFreq / num_segbo_inventories
```

Combine them.

``` r
bs_df_intersected <- inner_join(segbo_segment_counts, phoible_segment_counts)
```

    ## Joining, by = "Segment"

Calculate all borrowability scores.

``` r
bs_df_intersected$BorrowabilityScore <- bs_df_intersected$SegboCrossFreq / (bs_df_intersected$PhoibleCrossFreq - bs_df_intersected$PhoibleCrossFreq ^ 2)
```

Let’s have a look.

``` r
bs_df_intersected %>% arrange(desc(BorrowabilityScore)) %>% head(n=25) %>% kable()
```

| Segment | SegboFreq | SegboCrossFreq | PhoibleFreq | PhoibleCrossFreq | BorrowabilityScore |
|:--------|----------:|---------------:|------------:|-----------------:|-------------------:|
| ðˤ      |         6 |      0.0058309 |           2 |        0.0006623 |          8.8104995 |
| pˤ      |         2 |      0.0019436 |           2 |        0.0006623 |          2.9368332 |
| ðˠ      |         1 |      0.0009718 |           1 |        0.0003311 |          2.9358604 |
| d̤ɮ̤      |         1 |      0.0009718 |           1 |        0.0003311 |          2.9358604 |
| d̠̤ʒ̤      |         1 |      0.0009718 |           1 |        0.0003311 |          2.9358604 |
| l̪ˤ      |         1 |      0.0009718 |           1 |        0.0003311 |          2.9358604 |
| ʟ       |         1 |      0.0009718 |           1 |        0.0003311 |          2.9358604 |
| ndl     |         1 |      0.0009718 |           1 |        0.0003311 |          2.9358604 |
| n̪ˤ      |         1 |      0.0009718 |           1 |        0.0003311 |          2.9358604 |
| pʃʰ     |         1 |      0.0009718 |           1 |        0.0003311 |          2.9358604 |
| tlʱ     |         1 |      0.0009718 |           1 |        0.0003311 |          2.9358604 |
| tsʲʰ    |         1 |      0.0009718 |           1 |        0.0003311 |          2.9358604 |
| ɕː      |         5 |      0.0048591 |           7 |        0.0023179 |          2.1012191 |
| dˤ      |         5 |      0.0048591 |           7 |        0.0023179 |          2.1012191 |
| zˤ      |         5 |      0.0048591 |           8 |        0.0026490 |          1.8391771 |
| ɮʲ      |         2 |      0.0019436 |           4 |        0.0013245 |          1.4693903 |
| ʡ       |         2 |      0.0019436 |           4 |        0.0013245 |          1.4693903 |
| n̤d̤ɮ̤     |         1 |      0.0009718 |           2 |        0.0006623 |          1.4684166 |
| pʷʰ     |         1 |      0.0009718 |           2 |        0.0006623 |          1.4684166 |
| pʷʼ     |         1 |      0.0009718 |           2 |        0.0006623 |          1.4684166 |
| ɸʷ      |         1 |      0.0009718 |           2 |        0.0006623 |          1.4684166 |
| tl      |         1 |      0.0009718 |           2 |        0.0006623 |          1.4684166 |
| uə̯      |         1 |      0.0009718 |           2 |        0.0006623 |          1.4684166 |
| bˤ      |         1 |      0.0009718 |           3 |        0.0009934 |          0.9792689 |
| d̤z̤      |         1 |      0.0009718 |           3 |        0.0009934 |          0.9792689 |

# Compare the results

Compare the results (but not the intersection results).

``` r
bs_df_new_cut <- bs_df_new %>% select(Segment, PhoibleCrossFreq, BorrowabilityScore)
bs_df_collapsed_cut <- bs_df_collapsed %>% select(Segment, BorrowabilityScore)%>% rename(BorrowabilityScore_collapsed = BorrowabilityScore)
bs_df_random_cut <- bs_df_random %>% select(Segment, BorrowabilityScore) %>% rename(BorrowabilityScore_random = BorrowabilityScore)
bs_df_intersected <- bs_df_intersected %>% select(Segment, BorrowabilityScore) %>% rename(BorrowabilityScore_intersection = BorrowabilityScore)

results <- left_join(bs_df_new_cut, bs_df_collapsed_cut)
```

    ## Joining, by = "Segment"

``` r
results <- left_join(results, bs_df_random_cut)
```

    ## Joining, by = "Segment"

``` r
results <- left_join(results, bs_df_intersected)
```

    ## Joining, by = "Segment"

Have a look at all.

``` r
results %>% arrange(desc(BorrowabilityScore_intersection)) %>% head(n=25) %>% kable()
```

| Segment | PhoibleCrossFreq | BorrowabilityScore | BorrowabilityScore_collapsed | BorrowabilityScore_random | BorrowabilityScore_intersection |
|:--------|-----------------:|-------------------:|-----------------------------:|--------------------------:|--------------------------------:|
| ðˤ      |        0.0006623 |          17.105668 |                    25.506079 |                 23.773590 |                       8.8104995 |
| pˤ      |        0.0006623 |           5.701889 |                     4.253040 |                  3.964154 |                       2.9368332 |
| ðˠ      |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| d̤ɮ̤      |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| d̠̤ʒ̤      |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| l̪ˤ      |        0.0003311 |           5.700001 |                           NA |                        NA |                       2.9358604 |
| ʟ       |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| ndl     |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| n̪ˤ      |        0.0003311 |           5.700001 |                           NA |                        NA |                       2.9358604 |
| pʃʰ     |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| tlʱ     |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| tsʲʰ    |        0.0003311 |           5.700001 |                           NA |                        NA |                       2.9358604 |
| ɕː      |        0.0023179 |           4.079537 |                     4.259133 |                  3.969834 |                       2.1012191 |
| dˤ      |        0.0023179 |           4.079537 |                     4.259133 |                  3.969834 |                       2.1012191 |
| zˤ      |        0.0026490 |           3.570780 |                     3.550974 |                  3.309776 |                       1.8391771 |
| ɮʲ      |        0.0013245 |           2.852835 |                     2.836713 |                  2.644031 |                       1.4693903 |
| ʡ       |        0.0013245 |           2.852835 |                     4.253040 |                  3.964154 |                       1.4693903 |
| n̤d̤ɮ̤     |        0.0006623 |           2.850945 |                     2.126520 |                  1.982077 |                       1.4684166 |
| pʷʰ     |        0.0006623 |           2.850945 |                     4.251013 |                  3.962265 |                       1.4684166 |
| pʷʼ     |        0.0006623 |           2.850945 |                     4.251013 |                  3.962265 |                       1.4684166 |
| ɸʷ      |        0.0006623 |           2.850945 |                     4.251013 |                  3.962265 |                       1.4684166 |
| tl      |        0.0006623 |           2.850945 |                     2.126520 |                  1.982077 |                       1.4684166 |
| uə̯      |        0.0006623 |           2.850945 |                     4.251013 |                  3.962265 |                       1.4684166 |
| bˤ      |        0.0009934 |           1.901260 |                     1.418357 |                  1.322015 |                       0.9792689 |
| d̤z̤      |        0.0009934 |           1.901260 |                     1.418357 |                  1.322015 |                       0.9792689 |

Have a look at typologically frequent segments reported in PHOIBLE.

``` r
results %>% filter(PhoibleCrossFreq > .05) %>% arrange(desc(BorrowabilityScore)) %>% head(n=25) %>% kable()
```

| Segment | PhoibleCrossFreq | BorrowabilityScore | BorrowabilityScore_collapsed | BorrowabilityScore_random | BorrowabilityScore_intersection |
|:--------|-----------------:|-------------------:|-----------------------------:|--------------------------:|--------------------------------:|
| f       |        0.4403974 |          1.1330821 |                    1.2143897 |                 1.1319028 |                       0.5836089 |
| p       |        0.8586093 |          0.7460172 |                    0.8021429 |                 0.7476577 |                       0.3842460 |
| d̠ʒ      |        0.2715232 |          0.7154228 |                    0.7624947 |                 0.7107026 |                       0.3684879 |
| ɡ       |        0.5668874 |          0.6455142 |                    0.6958331 |                 0.6485690 |                       0.3324806 |
| t̠ʃ      |        0.4033113 |          0.5566655 |                    0.5994755 |                 0.5587564 |                       0.2867179 |
| z       |        0.2956954 |          0.5435886 |                    0.5830196 |                 0.5434183 |                       0.2799825 |
| b       |        0.6311258 |          0.5186924 |                    0.5599470 |                 0.5219128 |                       0.2671594 |
| v       |        0.2701987 |          0.5166900 |                    0.5478159 |                 0.5106058 |                       0.2661280 |
| h       |        0.5639073 |          0.4910408 |                    0.5282324 |                 0.4923524 |                       0.2529170 |
| ʒ       |        0.1582781 |          0.4815191 |                    0.5714159 |                 0.5326027 |                       0.2480127 |
| k       |        0.9036424 |          0.4117135 |                    0.4762375 |                 0.4438893 |                       0.2120585 |
| d       |        0.4556291 |          0.4107821 |                    0.4378362 |                 0.4080964 |                       0.2115787 |
| r       |        0.4410596 |          0.3979832 |                    0.4248715 |                 0.3960123 |                       0.2049865 |
| j       |        0.8993377 |          0.3959937 |                    0.4326733 |                 0.4032841 |                       0.2039618 |
| ʃ       |        0.3655629 |          0.3742237 |                    0.4107785 |                 0.3828765 |                       0.1927488 |
| x       |        0.1900662 |          0.3676977 |                    0.4164702 |                 0.3881816 |                       0.1893875 |
| l       |        0.6768212 |          0.3622902 |                    0.3977091 |                 0.3706949 |                       0.1866023 |
| ɸ       |        0.0506623 |          0.3530705 |                    0.3606821 |                 0.3361829 |                       0.1818536 |
| ð       |        0.0529801 |          0.3384500 |                    0.3904295 |                 0.3639098 |                       0.1743231 |
| ɾ       |        0.2562914 |          0.2969673 |                    0.3213502 |                 0.2995227 |                       0.1529569 |
| q       |        0.0847682 |          0.2918376 |                    0.3391456 |                 0.3161093 |                       0.1503148 |
| s       |        0.6692053 |          0.2897912 |                    0.3177287 |                 0.2961471 |                       0.1492608 |
| ɣ       |        0.1443709 |          0.2596617 |                    0.2985872 |                 0.2783059 |                       0.1337422 |
| χ       |        0.0711921 |          0.2568084 |                    0.2986214 |                 0.2783376 |                       0.1322725 |
| o       |        0.6046358 |          0.2446778 |                    0.2695894 |                 0.2512777 |                       0.1260245 |

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
| ɾ       |        0.2562914 |           20 |            20 |             20 |
| q       |        0.0847682 |           21 |            19 |             19 |
| s       |        0.6692053 |           22 |            21 |             21 |
| ɣ       |        0.1443709 |           23 |            23 |             23 |
| χ       |        0.0711921 |           24 |            22 |             22 |
| o       |        0.6046358 |           25 |            25 |             25 |
| w       |        0.8221854 |           26 |            24 |             24 |
| ts      |        0.2208609 |           27 |            26 |             26 |
| dz      |        0.1033113 |           28 |            28 |             28 |
| e       |        0.6096026 |           29 |            27 |             27 |
| ʔ       |        0.3748344 |           30 |            30 |             30 |
| ŋ       |        0.6284768 |           31 |            32 |             32 |
| m       |        0.9652318 |           32 |            33 |             33 |
| ʁ       |        0.0513245 |           33 |            NA |             NA |
| ɲ       |        0.4158940 |           34 |            35 |             35 |
| c       |        0.1384106 |           35 |            36 |             36 |
| ɟ       |        0.1218543 |           36 |            37 |             37 |
| y       |        0.0579470 |           37 |            34 |             34 |
| β       |        0.1013245 |           38 |            39 |             39 |
| æ       |        0.0738411 |           39 |            38 |             38 |
| ə       |        0.2235099 |           40 |            41 |             41 |
| ɽ       |        0.0592715 |           41 |            42 |             42 |
| mb      |        0.1046358 |           42 |            43 |             43 |
| pʰ      |        0.1963576 |           43 |            40 |             40 |
| ɛ       |        0.3738411 |           44 |            44 |             44 |
| nd      |        0.0970199 |           45 |            46 |             46 |
| ɔː      |        0.1019868 |           46 |            48 |             48 |
| t       |        0.6834437 |           47 |            45 |             45 |
| ɔ       |        0.3543046 |           48 |            49 |             49 |
| oː      |        0.2099338 |           49 |            52 |             52 |
| pʼ      |        0.0592715 |           50 |            47 |             47 |
| ŋɡ      |        0.0960265 |           51 |            55 |             55 |
| tʰ      |        0.1337748 |           52 |            50 |             50 |
| ʂ       |        0.0655629 |           53 |            51 |             51 |
| kʰ      |        0.2006623 |           54 |            53 |             53 |
| ɑ       |        0.0745033 |           55 |            56 |             56 |
| t̠ʃʰ     |        0.0758278 |           56 |            54 |             54 |
| kp      |        0.1235099 |           57 |            61 |             61 |
| ɗ       |        0.0821192 |           58 |            62 |             62 |
| ɖ       |        0.0850993 |           59 |            57 |             57 |
| d̪       |        0.1440397 |           60 |            58 |             58 |
| eː      |        0.2122517 |           61 |            63 |             63 |
| ɪ       |        0.1470199 |           62 |            60 |             60 |
| n       |        0.7781457 |           63 |            59 |             59 |
| t̪       |        0.2344371 |           64 |            64 |             64 |
| ɛː      |        0.1069536 |           65 |            66 |             66 |
| ɭ       |        0.1188742 |           66 |            67 |             67 |
| u       |        0.8761589 |           67 |            65 |             65 |
| ɳ       |        0.1321192 |           68 |            70 |             70 |
| t̠ʃʼ     |        0.0612583 |           69 |            68 |             68 |
| ʊ       |        0.1354305 |           70 |            71 |             71 |
| ɡʷ      |        0.0629139 |           71 |            72 |             72 |
| tsʰ     |        0.0645695 |           72 |            69 |             69 |
| ʈ       |        0.1592715 |           73 |            73 |             73 |
| ɨ       |        0.1625828 |           74 |            75 |             75 |
| aː      |        0.2953642 |           75 |            77 |             77 |
| ɔ̃       |        0.0758278 |           76 |            78 |             78 |
| n̪       |        0.1764901 |           77 |            74 |             74 |
| ɛ̃       |        0.0794702 |           78 |            80 |             80 |
| kʼ      |        0.0804636 |           79 |            76 |             76 |
| o̞       |        0.0956954 |           80 |            79 |             79 |
| ɓ       |        0.0993377 |           81 |            82 |             82 |
| õ       |        0.1142384 |           82 |            83 |             83 |
| uː      |        0.2937086 |           83 |            84 |             84 |
| kʷ      |        0.1231788 |           84 |            86 |             86 |
| iː      |        0.3178808 |           85 |            85 |             85 |
| ɡb      |        0.1238411 |           86 |            87 |             87 |
| a       |        0.8609272 |           87 |            81 |             81 |
| ã       |        0.1725166 |           88 |            88 |             88 |
| ĩ       |        0.1794702 |           89 |            89 |             89 |

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

![](README_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

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

![](README_files/figure-gfm/unnamed-chunk-39-1.png)<!-- -->

Also perhaps not so helpful. How about in terms of the more frequent
segments with a delta.

``` r
tmp$OCdelta <- abs(tmp$OriginalRank - tmp$CollapsedRank)
# tmp_by_freq_rank <- tmp %>% filter(OCdelta > 0) %>% arrange(desc(PhoibleCrossFreq, OriginalRank)) %>% head(n=10)
tmp_by_freq_rank <- tmp %>% filter(OCdelta > 0) %>% arrange(desc(PhoibleCrossFreq)) %>% head(n=10)

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

![](README_files/figure-gfm/unnamed-chunk-40-1.png)<!-- -->

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
