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

> > > Operationalizing borrowability: A case study from phonological
> > > segments

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
| f       |       148 |      0.2792453 |         919 |        0.4378275 |          1.1345227 |
| p       |        48 |      0.0905660 |        1800 |        0.8575512 |          0.7413897 |
| d̠ʒ      |        75 |      0.1415094 |         570 |        0.2715579 |          0.7153654 |
| ɡ       |        84 |      0.1584906 |        1202 |        0.5726536 |          0.6476366 |
| t̠ʃ      |        71 |      0.1339623 |         830 |        0.3954264 |          0.5603607 |
| z       |        60 |      0.1132075 |         623 |        0.2968080 |          0.5424077 |
| b       |        64 |      0.1207547 |        1331 |        0.6341115 |          0.5204628 |
| ʒ       |        34 |      0.0641509 |         303 |        0.1443545 |          0.5193725 |
| v       |        54 |      0.1018868 |         571 |        0.2720343 |          0.5144975 |
| h       |        64 |      0.1207547 |        1195 |        0.5693187 |          0.4924846 |
| k       |        19 |      0.0358491 |        1910 |        0.9099571 |          0.4375296 |
| d       |        54 |      0.1018868 |        1006 |        0.4792758 |          0.4082485 |
| j       |        19 |      0.0358491 |        1889 |        0.8999524 |          0.3981543 |
| r       |        52 |      0.0981132 |         957 |        0.4559314 |          0.3955253 |
| x       |        30 |      0.0566038 |         372 |        0.1772273 |          0.3881816 |
| ʃ       |        46 |      0.0867925 |         728 |        0.3468318 |          0.3831228 |
| l       |        42 |      0.0792453 |        1454 |        0.6927108 |          0.3722838 |
| ɸ       |         9 |      0.0169811 |         111 |        0.0528823 |          0.3390410 |
| q       |        12 |      0.0226415 |         159 |        0.0757504 |          0.3233936 |
| ɾ       |        30 |      0.0566038 |         529 |        0.2520248 |          0.3002721 |
| s       |        34 |      0.0641509 |        1438 |        0.6850881 |          0.2973498 |
| χ       |         9 |      0.0169811 |         133 |        0.0633635 |          0.2861254 |
| ɣ       |        17 |      0.0320755 |         279 |        0.1329204 |          0.2783059 |
| w       |        18 |      0.0339623 |        1768 |        0.8423059 |          0.2556885 |
| o       |        31 |      0.0584906 |        1331 |        0.6341115 |          0.2520992 |

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
phoible_segment_counts <- as.data.frame(table(collapsed_inventories$Value))
phoible_segment_counts <- phoible_segment_counts %>% rename(Segment = Var1, PhoibleFreq = Freq)
phoible_segment_counts$Segment <- as.character(phoible_segment_counts$Segment)
phoible_segment_counts$PhoibleCrossFreq <- phoible_segment_counts$PhoibleFreq / num_phoible_inventories
```

And do the same for SEGBO

``` r
segbo_collapsed <- segbo %>% group_by(ISO639P3code, Value)
num_segbo_inventories <- segbo %>% select(ISO639P3code) %>% distinct() %>% nrow()
segbo_segment_counts <- as.data.frame(table(segbo_collapsed$Value))
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
| l       |        42 |      0.0850202 |        2044 |        0.9737970 |          3.3319896 |
| s       |        34 |      0.0688259 |        2021 |        0.9628394 |          1.9236051 |
| b       |        64 |      0.1295547 |        1906 |        0.9080515 |          1.5516640 |
| f       |       148 |      0.2995951 |        1330 |        0.6336351 |          1.2905703 |
| ɡ       |        84 |      0.1700405 |        1712 |        0.8156265 |          1.1307393 |
| t       |         9 |      0.0182186 |        2064 |        0.9833254 |          1.1111244 |
| h       |        64 |      0.1295547 |        1703 |        0.8113387 |          0.8463852 |
| d̠ʒ      |        75 |      0.1518219 |         820 |        0.3906622 |          0.6377857 |
| t̠ʃ      |        71 |      0.1437247 |        1218 |        0.5802763 |          0.5901101 |
| o       |        31 |      0.0627530 |        1826 |        0.8699381 |          0.5546209 |
| ŋ       |        22 |      0.0445344 |        1898 |        0.9042401 |          0.5143140 |
| z       |        60 |      0.1214575 |         893 |        0.4254407 |          0.4968787 |
| d       |        54 |      0.1093117 |        1376 |        0.6555503 |          0.4840999 |
| e       |        25 |      0.0506073 |        1841 |        0.8770843 |          0.4694231 |
| v       |        54 |      0.1093117 |         816 |        0.3887566 |          0.4600181 |
| r       |        52 |      0.1052632 |        1332 |        0.6345879 |          0.4539434 |
| ʒ       |        34 |      0.0688259 |         478 |        0.2277275 |          0.3913505 |
| ʃ       |        46 |      0.0931174 |        1104 |        0.5259647 |          0.3734768 |
| ʎ       |        10 |      0.0202429 |         147 |        0.0700333 |          0.3108141 |
| x       |        30 |      0.0607287 |         574 |        0.2734636 |          0.3056592 |
| ɸ       |         9 |      0.0182186 |         153 |        0.0728919 |          0.2695915 |
| ɾ       |        30 |      0.0607287 |         774 |        0.3687470 |          0.2608930 |
| ð       |         9 |      0.0182186 |         160 |        0.0762268 |          0.2587275 |
| q       |        12 |      0.0242915 |         256 |        0.1219628 |          0.2268370 |
| ɣ       |        17 |      0.0344130 |         436 |        0.2077180 |          0.2091068 |

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
| ðˤ      |        0.0006623 |          17.105668 |                    12.759121 |                 23.773590 |                       8.8104995 |
| pˤ      |        0.0006623 |           5.701889 |                     4.253040 |                  3.964154 |                       2.9368332 |
| ðˠ      |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| d̤ɮ̤      |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| d̠̤ʒ̤      |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| l̪ˤ      |        0.0003311 |           5.700001 |                     4.251013 |                        NA |                       2.9358604 |
| ʟ       |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| ndl     |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| n̪ˤ      |        0.0003311 |           5.700001 |                     4.251013 |                        NA |                       2.9358604 |
| pʃʰ     |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| tlʱ     |        0.0003311 |           5.700001 |                     4.251013 |                  3.962265 |                       2.9358604 |
| tsʲʰ    |        0.0003311 |           5.700001 |                     4.251013 |                        NA |                       2.9358604 |
| ɕː      |        0.0023179 |           4.079537 |                     3.045147 |                  3.969834 |                       2.1012191 |
| dˤ      |        0.0023179 |           4.079537 |                     3.045147 |                  4.959924 |                       2.1012191 |
| zˤ      |        0.0026490 |           3.570780 |                     2.665778 |                  4.959924 |                       1.8391771 |
| ɮʲ      |        0.0013245 |           2.852835 |                     2.128550 |                  3.964154 |                       1.4693903 |
| ʡ       |        0.0013245 |           2.852835 |                     2.128550 |                  3.964154 |                       1.4693903 |
| n̤d̤ɮ̤     |        0.0006623 |           2.850945 |                     2.126520 |                  1.982077 |                       1.4684166 |
| pʷʰ     |        0.0006623 |           2.850945 |                     2.126520 |                  3.962265 |                       1.4684166 |
| pʷʼ     |        0.0006623 |           2.850945 |                     2.126520 |                  1.982077 |                       1.4684166 |
| ɸʷ      |        0.0006623 |           2.850945 |                     2.126520 |                  3.962265 |                       1.4684166 |
| tl      |        0.0006623 |           2.850945 |                     2.126520 |                  1.982077 |                       1.4684166 |
| uə̯      |        0.0006623 |           2.850945 |                     2.126520 |                  3.962265 |                       1.4684166 |
| bˤ      |        0.0009934 |           1.901260 |                     1.418357 |                  1.982077 |                       0.9792689 |
| d̤z̤      |        0.0009934 |           1.901260 |                     1.418357 |                  1.322015 |                       0.9792689 |

Have a look at typologically frequent segments reported in PHOIBLE.

``` r
results %>% filter(PhoibleCrossFreq > .05) %>% arrange(desc(BorrowabilityScore)) %>% head(n=25) %>% kable()
```

| Segment | PhoibleCrossFreq | BorrowabilityScore | BorrowabilityScore_collapsed | BorrowabilityScore_random | BorrowabilityScore_intersection |
|:--------|-----------------:|-------------------:|-----------------------------:|--------------------------:|--------------------------------:|
| f       |        0.4403974 |          1.1330821 |                    1.2905703 |                 1.1345227 |                       0.5836089 |
| p       |        0.8586093 |          0.7460172 |                   -0.3342025 |                 0.7413897 |                       0.3842460 |
| d̠ʒ      |        0.2715232 |          0.7154228 |                    0.6377857 |                 0.7153654 |                       0.3684879 |
| ɡ       |        0.5668874 |          0.6455142 |                    1.1307393 |                 0.6476366 |                       0.3324806 |
| t̠ʃ      |        0.4033113 |          0.5566655 |                    0.5901101 |                 0.5603607 |                       0.2867179 |
| z       |        0.2956954 |          0.5435886 |                    0.4968787 |                 0.5424077 |                       0.2799825 |
| b       |        0.6311258 |          0.5186924 |                    1.5516640 |                 0.5204628 |                       0.2671594 |
| v       |        0.2701987 |          0.5166900 |                    0.4600181 |                 0.5144975 |                       0.2661280 |
| h       |        0.5639073 |          0.4910408 |                    0.8463852 |                 0.4924846 |                       0.2529170 |
| ʒ       |        0.1582781 |          0.4815191 |                    0.3913505 |                 0.5193725 |                       0.2480127 |
| k       |        0.9036424 |          0.4117135 |                   -0.0985615 |                 0.4375296 |                       0.2120585 |
| d       |        0.4556291 |          0.4107821 |                    0.4840999 |                 0.4082485 |                       0.2115787 |
| r       |        0.4410596 |          0.3979832 |                    0.4539434 |                 0.3955253 |                       0.2049865 |
| j       |        0.8993377 |          0.3959937 |                   -0.1011199 |                 0.3981543 |                       0.2039618 |
| ʃ       |        0.3655629 |          0.3742237 |                    0.3734768 |                 0.3831228 |                       0.1927488 |
| x       |        0.1900662 |          0.3676977 |                    0.3056592 |                 0.3881816 |                       0.1893875 |
| l       |        0.6768212 |          0.3622902 |                    3.3319896 |                 0.3722838 |                       0.1866023 |
| ɸ       |        0.0506623 |          0.3530705 |                    0.2695915 |                 0.3390410 |                       0.1818536 |
| ð       |        0.0529801 |          0.3384500 |                    0.2587275 |                 0.3815209 |                       0.1743231 |
| ɾ       |        0.2562914 |          0.2969673 |                    0.2608930 |                 0.3002721 |                       0.1529569 |
| q       |        0.0847682 |          0.2918376 |                    0.2268370 |                 0.3233936 |                       0.1503148 |
| s       |        0.6692053 |          0.2897912 |                    1.9236051 |                 0.2973498 |                       0.1492608 |
| ɣ       |        0.1443709 |          0.2596617 |                    0.2091068 |                 0.2783059 |                       0.1337422 |
| χ       |        0.0711921 |          0.2568084 |                    0.1981623 |                 0.2861254 |                       0.1322725 |
| o       |        0.6046358 |          0.2446778 |                    0.5546209 |                 0.2520992 |                       0.1260245 |

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
| f       |        0.4403974 |            1 |             4 |              1 |
| p       |        0.8586093 |            2 |           102 |              2 |
| d̠ʒ      |        0.2715232 |            3 |             8 |              3 |
| ɡ       |        0.5668874 |            4 |             5 |              4 |
| t̠ʃ      |        0.4033113 |            5 |             9 |              5 |
| z       |        0.2956954 |            6 |            12 |              6 |
| b       |        0.6311258 |            7 |             3 |              7 |
| v       |        0.2701987 |            8 |            15 |              9 |
| h       |        0.5639073 |            9 |             7 |             10 |
| ʒ       |        0.1582781 |           10 |            17 |              8 |
| k       |        0.9036424 |           11 |            99 |             11 |
| d       |        0.4556291 |           12 |            13 |             12 |
| r       |        0.4410596 |           13 |            16 |             14 |
| j       |        0.8993377 |           14 |           100 |             13 |
| ʃ       |        0.3655629 |           15 |            18 |             16 |
| x       |        0.1900662 |           16 |            20 |             15 |
| l       |        0.6768212 |           17 |             1 |             17 |
| ɸ       |        0.0506623 |           18 |            21 |             18 |
| ð       |        0.0529801 |           19 |            23 |             NA |
| ɾ       |        0.2562914 |           20 |            22 |             20 |
| q       |        0.0847682 |           21 |            24 |             19 |
| s       |        0.6692053 |           22 |             2 |             21 |
| ɣ       |        0.1443709 |           23 |            25 |             23 |
| χ       |        0.0711921 |           24 |            26 |             22 |
| o       |        0.6046358 |           25 |            10 |             25 |
| w       |        0.8221854 |           26 |           101 |             24 |
| ts      |        0.2208609 |           27 |            27 |             26 |
| dz      |        0.1033113 |           28 |            31 |             28 |
| e       |        0.6096026 |           29 |            14 |             27 |
| ʔ       |        0.3748344 |           30 |            28 |             31 |
| ŋ       |        0.6284768 |           31 |            11 |             32 |
| m       |        0.9652318 |           32 |            96 |             33 |
| ʁ       |        0.0513245 |           33 |            34 |             NA |
| ɲ       |        0.4158940 |           34 |            30 |             34 |
| c       |        0.1384106 |           35 |            35 |             35 |
| ɟ       |        0.1218543 |           36 |            36 |             36 |
| y       |        0.0579470 |           37 |            38 |             NA |
| β       |        0.1013245 |           38 |            39 |             38 |
| æ       |        0.0738411 |           39 |            42 |             37 |
| ə       |        0.2235099 |           40 |            40 |             40 |
| ɽ       |        0.0592715 |           41 |            46 |             39 |
| mb      |        0.1046358 |           42 |            45 |             42 |
| pʰ      |        0.1963576 |           43 |            44 |             41 |
| ɛ       |        0.3738411 |           44 |            41 |             43 |
| nd      |        0.0970199 |           45 |            48 |             45 |
| ɔː      |        0.1019868 |           46 |            51 |             46 |
| t       |        0.6834437 |           47 |             6 |             44 |
| ɔ       |        0.3543046 |           48 |            47 |             48 |
| oː      |        0.2099338 |           49 |            52 |             51 |
| pʼ      |        0.0592715 |           50 |            54 |             47 |
| ŋɡ      |        0.0960265 |           51 |            55 |             55 |
| tʰ      |        0.1337748 |           52 |            53 |             49 |
| ʂ       |        0.0655629 |           53 |            57 |             50 |
| kʰ      |        0.2006623 |           54 |            56 |             52 |
| ɑ       |        0.0745033 |           55 |            58 |             54 |
| t̠ʃʰ     |        0.0758278 |           56 |            59 |             53 |
| kp      |        0.1235099 |           57 |            60 |             60 |
| ɗ       |        0.0821192 |           58 |            63 |             61 |
| ɖ       |        0.0850993 |           59 |            66 |             56 |
| d̪       |        0.1440397 |           60 |            67 |             57 |
| eː      |        0.2122517 |           61 |            65 |             63 |
| ɪ       |        0.1470199 |           62 |            68 |             59 |
| n       |        0.7781457 |           63 |            98 |             58 |
| t̪       |        0.2344371 |           64 |            69 |             62 |
| ɛː      |        0.1069536 |           65 |            72 |             65 |
| ɭ       |        0.1188742 |           66 |            73 |             66 |
| u       |        0.8761589 |           67 |            97 |             64 |
| ɳ       |        0.1321192 |           68 |            74 |             69 |
| t̠ʃʼ     |        0.0612583 |           69 |            76 |             68 |
| ʊ       |        0.1354305 |           70 |            75 |             70 |
| ɡʷ      |        0.0629139 |           71 |            78 |             71 |
| tsʰ     |        0.0645695 |           72 |            79 |             67 |
| ʈ       |        0.1592715 |           73 |            80 |             72 |
| ɨ       |        0.1625828 |           74 |            81 |             75 |
| aː      |        0.2953642 |           75 |            77 |             76 |
| ɔ̃       |        0.0758278 |           76 |            83 |             78 |
| n̪       |        0.1764901 |           77 |            82 |             73 |
| ɛ̃       |        0.0794702 |           78 |            84 |             79 |
| kʼ      |        0.0804636 |           79 |            85 |             74 |
| o̞       |        0.0956954 |           80 |            86 |             77 |
| ɓ       |        0.0993377 |           81 |            88 |             80 |
| õ       |        0.1142384 |           82 |            90 |             82 |
| uː      |        0.2937086 |           83 |            87 |             83 |
| kʷ      |        0.1231788 |           84 |            91 |             85 |
| iː      |        0.3178808 |           85 |            89 |             84 |
| ɡb      |        0.1238411 |           86 |            92 |             86 |
| a       |        0.8609272 |           87 |            95 |             81 |
| ã       |        0.1725166 |           88 |            93 |             87 |
| ĩ       |        0.1794702 |           89 |            94 |             88 |

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
