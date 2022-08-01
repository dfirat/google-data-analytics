Google Data Analytics Capstone
================
Deniz Firat

## PREPARE

### Load needed packages

``` r
library("tidyverse")
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.8     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library("janitor")
```

    ## 
    ## Attaching package: 'janitor'
    ## 
    ## The following objects are masked from 'package:stats':
    ## 
    ##     chisq.test, fisher.test

``` r
library("lubridate")
```

    ## 
    ## Attaching package: 'lubridate'
    ## 
    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
library("hms")
```

    ## 
    ## Attaching package: 'hms'
    ## 
    ## The following object is masked from 'package:lubridate':
    ## 
    ##     hms

### Import data sets [(Source)](https://divvy-tripdata.s3.amazonaws.com/index.html)

``` r
jul_2021 <- read.csv("202107-divvy-tripdata.csv")
aug_2021 <- read.csv("202108-divvy-tripdata.csv")
sep_2021 <- read.csv("202109-divvy-tripdata.csv")
oct_2021 <- read.csv("202110-divvy-tripdata.csv")
nov_2021 <- read.csv("202111-divvy-tripdata.csv")
dec_2021 <- read.csv("202112-divvy-tripdata.csv")
jan_2022 <- read.csv("202201-divvy-tripdata.csv")
feb_2022 <- read.csv("202202-divvy-tripdata.csv")
mar_2022 <- read.csv("202203-divvy-tripdata.csv")
apr_2022 <- read.csv("202204-divvy-tripdata.csv")
may_2022 <- read.csv("202205-divvy-tripdata.csv")
jun_2022 <- read.csv("202206-divvy-tripdata.csv")
```

### Check if data sets can be merged

``` r
compare_df_cols_same(
  jul_2021,
  aug_2021,
  sep_2021,
  oct_2021,
  nov_2021,
  dec_2021,
  jan_2022,
  feb_2022,
  mar_2022,
  apr_2022,
  may_2022,
  jun_2022,
  bind_method = c("bind_rows", "rbind"),
  verbose = TRUE
)
```

    ## [1] TRUE

### Merge all data sets

``` r
cyclistic  <- rbind (
  jul_2021,
  aug_2021,
  sep_2021,
  oct_2021,
  nov_2021,
  dec_2021,
  jan_2022,
  feb_2022,
  mar_2022,
  apr_2022,
  may_2022,
  jun_2022
)
```

### Inspect new data set

``` r
summary(cyclistic)
```

    ##    ride_id          rideable_type       started_at          ended_at        
    ##  Length:5900385     Length:5900385     Length:5900385     Length:5900385    
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##  start_station_name start_station_id   end_station_name   end_station_id    
    ##  Length:5900385     Length:5900385     Length:5900385     Length:5900385    
    ##  Class :character   Class :character   Class :character   Class :character  
    ##  Mode  :character   Mode  :character   Mode  :character   Mode  :character  
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##                                                                             
    ##    start_lat       start_lng         end_lat         end_lng      
    ##  Min.   :41.64   Min.   :-87.84   Min.   :41.39   Min.   :-88.97  
    ##  1st Qu.:41.88   1st Qu.:-87.66   1st Qu.:41.88   1st Qu.:-87.66  
    ##  Median :41.90   Median :-87.64   Median :41.90   Median :-87.64  
    ##  Mean   :41.90   Mean   :-87.65   Mean   :41.90   Mean   :-87.65  
    ##  3rd Qu.:41.93   3rd Qu.:-87.63   3rd Qu.:41.93   3rd Qu.:-87.63  
    ##  Max.   :45.64   Max.   :-73.80   Max.   :42.17   Max.   :-87.49  
    ##                                   NA's   :5374    NA's   :5374    
    ##  member_casual     
    ##  Length:5900385    
    ##  Class :character  
    ##  Mode  :character  
    ##                    
    ##                    
    ##                    
    ## 

## PROCESS

### Remove geographical indication contained in columns 9 to 12

``` r
cyclistic <- cyclistic[ -c(9:12) ]
```

### Check for duplicates

``` r
length(unique(cyclistic$ride_id)) == nrow(cyclistic)
```

    ## [1] TRUE

``` r
#use "cyclistic <- distinct(cyclistic)" to remove duplicates
```

### Remove rows contating NA and NULL values

``` r
cyclistic[cyclistic == '' ] <- NA
cyclistic<- na.omit(cyclistic)
```

### Check for misspelled strings/categories

``` r
unique(cyclistic$rideable_type)
```

    ## [1] "docked_bike"   "classic_bike"  "electric_bike"

``` r
unique(cyclistic$member_casual)
```

    ## [1] "casual" "member"

### Convert character to datetime

``` r
cyclistic$started_at <- as.POSIXct(cyclistic$started_at, format="%Y-%m-%d %H:%M:%S")
cyclistic$ended_at <- as.POSIXct(cyclistic$ended_at, format="%Y-%m-%d %H:%M:%S")
```

### Calculate ride length

``` r
cyclistic$ride_length <- difftime(cyclistic$ended_at,cyclistic$started_at, units = "mins")
cyclistic$ride_length <- round(cyclistic$ride_length, digits = 2)
```

### Remove negative and/or zero ride lengths

``` r
cyclistic <- subset(cyclistic, ride_length > 0)
```

### Create new date columns

``` r
cyclistic$month <- format(as.Date(cyclistic$started_at), "%m")
cyclistic$day <- format(as.Date(cyclistic$started_at), "%d")
cyclistic$year <- format(as.Date(cyclistic$started_at), "%Y")
cyclistic$weekday <- format(as.Date(cyclistic$started_at), "%A")
cyclistic$time <- as_hms((cyclistic$started_at))
```

## ANALYZE

### Count member type

``` r
cyclistic %>%
  group_by(member_casual) %>% 
  count(member_casual)
```

    ## # A tibble: 2 × 2
    ## # Groups:   member_casual [2]
    ##   member_casual       n
    ##   <chr>           <int>
    ## 1 casual        2006989
    ## 2 member        2671578

### Count bike type

``` r
cyclistic %>%
  group_by(rideable_type) %>% 
  count(rideable_type)
```

    ## # A tibble: 3 × 2
    ## # Groups:   rideable_type [3]
    ##   rideable_type       n
    ##   <chr>           <int>
    ## 1 classic_bike  3181680
    ## 2 docked_bike    252042
    ## 3 electric_bike 1244845

### Total rides by month

``` r
cyclistic %>%
    count(month)
```

    ##    month      n
    ## 1     01  79983
    ## 2     02  89191
    ## 3     03 215955
    ## 4     04 272918
    ## 5     05 502303
    ## 6     06 620416
    ## 7     07 693488
    ## 8     08 673023
    ## 9     09 621282
    ## 10    10 477704
    ## 11    11 255830
    ## 12    12 176474

### Total rides by member type and month

``` r
cyclistic %>%
    group_by(member_casual) %>% 
    count(month) %>% 
    print(n = 24)
```

    ## # A tibble: 24 × 3
    ## # Groups:   member_casual [2]
    ##    member_casual month      n
    ##    <chr>         <chr>  <int>
    ##  1 casual        01     12529
    ##  2 casual        02     15150
    ##  3 casual        03     67146
    ##  4 casual        04     92115
    ##  5 casual        05    220131
    ##  6 casual        06    292139
    ##  7 casual        07    370229
    ##  8 casual        08    340514
    ##  9 casual        09    292992
    ## 10 casual        10    188962
    ## 11 casual        11     69942
    ## 12 casual        12     45140
    ## 13 member        01     67454
    ## 14 member        02     74041
    ## 15 member        03    148809
    ## 16 member        04    180803
    ## 17 member        05    282172
    ## 18 member        06    328277
    ## 19 member        07    323259
    ## 20 member        08    332509
    ## 21 member        09    328290
    ## 22 member        10    288742
    ## 23 member        11    185888
    ## 24 member        12    131334

### Total rides by member, bike type and month

``` r
cyclistic %>%
    group_by(member_casual, rideable_type) %>% 
    count(month) %>% 
    print(n = 60)
```

    ## # A tibble: 60 × 4
    ## # Groups:   member_casual, rideable_type [5]
    ##    member_casual rideable_type month      n
    ##    <chr>         <chr>         <chr>  <int>
    ##  1 casual        classic_bike  01      6859
    ##  2 casual        classic_bike  02      8062
    ##  3 casual        classic_bike  03     35263
    ##  4 casual        classic_bike  04     47496
    ##  5 casual        classic_bike  05    125642
    ##  6 casual        classic_bike  06    169515
    ##  7 casual        classic_bike  07    241162
    ##  8 casual        classic_bike  08    228458
    ##  9 casual        classic_bike  09    194771
    ## 10 casual        classic_bike  10    105043
    ## 11 casual        classic_bike  11     31695
    ## 12 casual        classic_bike  12     19716
    ## 13 casual        docked_bike   01       933
    ## 14 casual        docked_bike   02      1344
    ## 15 casual        docked_bike   03      8217
    ## 16 casual        docked_bike   04     12008
    ## 17 casual        docked_bike   05     26115
    ## 18 casual        docked_bike   06     30231
    ## 19 casual        docked_bike   07     57770
    ## 20 casual        docked_bike   08     44961
    ## 21 casual        docked_bike   09     35350
    ## 22 casual        docked_bike   10     22667
    ## 23 casual        docked_bike   11      7560
    ## 24 casual        docked_bike   12      4886
    ## 25 casual        electric_bike 01      4737
    ## 26 casual        electric_bike 02      5744
    ## 27 casual        electric_bike 03     23666
    ## 28 casual        electric_bike 04     32611
    ## 29 casual        electric_bike 05     68374
    ## 30 casual        electric_bike 06     92393
    ## 31 casual        electric_bike 07     71297
    ## 32 casual        electric_bike 08     67095
    ## 33 casual        electric_bike 09     62871
    ## 34 casual        electric_bike 10     61252
    ## 35 casual        electric_bike 11     30687
    ## 36 casual        electric_bike 12     20538
    ## 37 member        classic_bike  01     47755
    ## 38 member        classic_bike  02     51169
    ## 39 member        classic_bike  03     99009
    ## 40 member        classic_bike  04    119233
    ## 41 member        classic_bike  05    197825
    ## 42 member        classic_bike  06    236568
    ## 43 member        classic_bike  07    265269
    ## 44 member        classic_bike  08    272318
    ## 45 member        classic_bike  09    266378
    ## 46 member        classic_bike  10    209963
    ## 47 member        classic_bike  11    121897
    ## 48 member        classic_bike  12     80614
    ## 49 member        electric_bike 01     19699
    ## 50 member        electric_bike 02     22872
    ## 51 member        electric_bike 03     49800
    ## 52 member        electric_bike 04     61570
    ## 53 member        electric_bike 05     84347
    ## 54 member        electric_bike 06     91709
    ## 55 member        electric_bike 07     57990
    ## 56 member        electric_bike 08     60191
    ## 57 member        electric_bike 09     61912
    ## 58 member        electric_bike 10     78779
    ## 59 member        electric_bike 11     63991
    ## 60 member        electric_bike 12     50720

### Total rides by weekday

``` r
cyclistic %>%
    count(weekday)
```

    ##     weekday      n
    ## 1    Friday 668508
    ## 2    Monday 609042
    ## 3  Saturday 793855
    ## 4    Sunday 669328
    ## 5  Thursday 674639
    ## 6   Tuesday 628861
    ## 7 Wednesday 634334

### Total rides by member type and weekday

``` r
cyclistic %>%
    group_by(member_casual) %>% 
    count(weekday)
```

    ## # A tibble: 14 × 3
    ## # Groups:   member_casual [2]
    ##    member_casual weekday        n
    ##    <chr>         <chr>      <int>
    ##  1 casual        Friday    291481
    ##  2 casual        Monday    233001
    ##  3 casual        Saturday  441114
    ##  4 casual        Sunday    362640
    ##  5 casual        Thursday  251833
    ##  6 casual        Tuesday   210087
    ##  7 casual        Wednesday 216833
    ##  8 member        Friday    377027
    ##  9 member        Monday    376041
    ## 10 member        Saturday  352741
    ## 11 member        Sunday    306688
    ## 12 member        Thursday  422806
    ## 13 member        Tuesday   418774
    ## 14 member        Wednesday 417501

### Total rides by member, bike type and weekday

``` r
cyclistic %>%
    group_by(member_casual, rideable_type) %>% 
    count(weekday) %>% 
    print(n = 35)
```

    ## # A tibble: 35 × 4
    ## # Groups:   member_casual, rideable_type [5]
    ##    member_casual rideable_type weekday        n
    ##    <chr>         <chr>         <chr>      <int>
    ##  1 casual        classic_bike  Friday    176409
    ##  2 casual        classic_bike  Monday    135254
    ##  3 casual        classic_bike  Saturday  279809
    ##  4 casual        classic_bike  Sunday    225234
    ##  5 casual        classic_bike  Thursday  150183
    ##  6 casual        classic_bike  Tuesday   120203
    ##  7 casual        classic_bike  Wednesday 126590
    ##  8 casual        docked_bike   Friday     34534
    ##  9 casual        docked_bike   Monday     30745
    ## 10 casual        docked_bike   Saturday   62107
    ## 11 casual        docked_bike   Sunday     53104
    ## 12 casual        docked_bike   Thursday   26343
    ## 13 casual        docked_bike   Tuesday    22793
    ## 14 casual        docked_bike   Wednesday  22416
    ## 15 casual        electric_bike Friday     80538
    ## 16 casual        electric_bike Monday     67002
    ## 17 casual        electric_bike Saturday   99198
    ## 18 casual        electric_bike Sunday     84302
    ## 19 casual        electric_bike Thursday   75307
    ## 20 casual        electric_bike Tuesday    67091
    ## 21 casual        electric_bike Wednesday  67827
    ## 22 member        classic_bike  Friday    275072
    ## 23 member        classic_bike  Monday    275825
    ## 24 member        classic_bike  Saturday  268012
    ## 25 member        classic_bike  Sunday    234283
    ## 26 member        classic_bike  Thursday  307933
    ## 27 member        classic_bike  Tuesday   302980
    ## 28 member        classic_bike  Wednesday 303893
    ## 29 member        electric_bike Friday    101955
    ## 30 member        electric_bike Monday    100216
    ## 31 member        electric_bike Saturday   84729
    ## 32 member        electric_bike Sunday     72405
    ## 33 member        electric_bike Thursday  114873
    ## 34 member        electric_bike Tuesday   115794
    ## 35 member        electric_bike Wednesday 113608

### Total rides by day

``` r
cyclistic %>%
    count(day)
```

    ##    day      n
    ## 1   01 155286
    ## 2   02 147879
    ## 3   03 148281
    ## 4   04 157369
    ## 5   05 160242
    ## 6   06 137289
    ## 7   07 146961
    ## 8   08 139881
    ## 9   09 166952
    ## 10  10 165914
    ## 11  11 147773
    ## 12  12 151373
    ## 13  13 150658
    ## 14  14 174774
    ## 15  15 154445
    ## 16  16 172493
    ## 17  17 171742
    ## 18  18 158746
    ## 19  19 158324
    ## 20  20 161613
    ## 21  21 157913
    ## 22  22 142911
    ## 23  23 163586
    ## 24  24 134367
    ## 25  25 128939
    ## 26  26 145402
    ## 27  27 135594
    ## 28  28 150637
    ## 29  29 145992
    ## 30  30 151260
    ## 31  31  93971

### Total rides by member type and day

``` r
cyclistic %>%
    group_by(member_casual) %>% 
    count(day) %>% 
    print(n = 62)
```

    ## # A tibble: 62 × 3
    ## # Groups:   member_casual [2]
    ##    member_casual day        n
    ##    <chr>         <chr>  <int>
    ##  1 casual        01     62352
    ##  2 casual        02     58648
    ##  3 casual        03     66728
    ##  4 casual        04     71849
    ##  5 casual        05     76935
    ##  6 casual        06     58657
    ##  7 casual        07     62786
    ##  8 casual        08     53693
    ##  9 casual        09     69455
    ## 10 casual        10     72977
    ## 11 casual        11     64957
    ## 12 casual        12     62179
    ## 13 casual        13     59391
    ## 14 casual        14     74812
    ## 15 casual        15     63482
    ## 16 casual        16     71883
    ## 17 casual        17     78775
    ## 18 casual        18     73391
    ## 19 casual        19     67966
    ## 20 casual        20     67758
    ## 21 casual        21     65250
    ## 22 casual        22     58895
    ## 23 casual        23     70696
    ## 24 casual        24     59744
    ## 25 casual        25     59021
    ## 26 casual        26     62584
    ## 27 casual        27     51785
    ## 28 casual        28     65401
    ## 29 casual        29     64634
    ## 30 casual        30     67507
    ## 31 casual        31     42798
    ## 32 member        01     92934
    ## 33 member        02     89231
    ## 34 member        03     81553
    ## 35 member        04     85520
    ## 36 member        05     83307
    ## 37 member        06     78632
    ## 38 member        07     84175
    ## 39 member        08     86188
    ## 40 member        09     97497
    ## 41 member        10     92937
    ## 42 member        11     82816
    ## 43 member        12     89194
    ## 44 member        13     91267
    ## 45 member        14     99962
    ## 46 member        15     90963
    ## 47 member        16    100610
    ## 48 member        17     92967
    ## 49 member        18     85355
    ## 50 member        19     90358
    ## 51 member        20     93855
    ## 52 member        21     92663
    ## 53 member        22     84016
    ## 54 member        23     92890
    ## 55 member        24     74623
    ## 56 member        25     69918
    ## 57 member        26     82818
    ## 58 member        27     83809
    ## 59 member        28     85236
    ## 60 member        29     81358
    ## 61 member        30     83753
    ## 62 member        31     51173

### Total rides by member, bike type and day

``` r
cyclistic %>%
    group_by(member_casual, rideable_type) %>% 
    count(day) %>% 
    print(n = 155)
```

    ## # A tibble: 155 × 4
    ## # Groups:   member_casual, rideable_type [5]
    ##     member_casual rideable_type day       n
    ##     <chr>         <chr>         <chr> <int>
    ##   1 casual        classic_bike  01    38441
    ##   2 casual        classic_bike  02    35035
    ##   3 casual        classic_bike  03    40419
    ##   4 casual        classic_bike  04    42805
    ##   5 casual        classic_bike  05    47581
    ##   6 casual        classic_bike  06    35521
    ##   7 casual        classic_bike  07    38407
    ##   8 casual        classic_bike  08    32044
    ##   9 casual        classic_bike  09    41632
    ##  10 casual        classic_bike  10    44733
    ##  11 casual        classic_bike  11    39816
    ##  12 casual        classic_bike  12    37065
    ##  13 casual        classic_bike  13    34985
    ##  14 casual        classic_bike  14    47752
    ##  15 casual        classic_bike  15    38557
    ##  16 casual        classic_bike  16    41688
    ##  17 casual        classic_bike  17    47955
    ##  18 casual        classic_bike  18    45834
    ##  19 casual        classic_bike  19    40550
    ##  20 casual        classic_bike  20    39354
    ##  21 casual        classic_bike  21    38642
    ##  22 casual        classic_bike  22    35505
    ##  23 casual        classic_bike  23    41794
    ##  24 casual        classic_bike  24    36236
    ##  25 casual        classic_bike  25    36348
    ##  26 casual        classic_bike  26    37833
    ##  27 casual        classic_bike  27    29860
    ##  28 casual        classic_bike  28    39845
    ##  29 casual        classic_bike  29    40248
    ##  30 casual        classic_bike  30    40130
    ##  31 casual        classic_bike  31    27067
    ##  32 casual        docked_bike   01     7511
    ##  33 casual        docked_bike   02     7196
    ##  34 casual        docked_bike   03     9129
    ##  35 casual        docked_bike   04    10611
    ##  36 casual        docked_bike   05    11098
    ##  37 casual        docked_bike   06     7539
    ##  38 casual        docked_bike   07     8017
    ##  39 casual        docked_bike   08     6394
    ##  40 casual        docked_bike   09     8601
    ##  41 casual        docked_bike   10     9493
    ##  42 casual        docked_bike   11     8182
    ##  43 casual        docked_bike   12     7876
    ##  44 casual        docked_bike   13     7579
    ##  45 casual        docked_bike   14     9111
    ##  46 casual        docked_bike   15     7776
    ##  47 casual        docked_bike   16     9017
    ##  48 casual        docked_bike   17    10615
    ##  49 casual        docked_bike   18     9642
    ##  50 casual        docked_bike   19     8510
    ##  51 casual        docked_bike   20     8061
    ##  52 casual        docked_bike   21     7861
    ##  53 casual        docked_bike   22     6926
    ##  54 casual        docked_bike   23     9104
    ##  55 casual        docked_bike   24     7186
    ##  56 casual        docked_bike   25     7500
    ##  57 casual        docked_bike   26     7401
    ##  58 casual        docked_bike   27     5672
    ##  59 casual        docked_bike   28     7619
    ##  60 casual        docked_bike   29     7440
    ##  61 casual        docked_bike   30     7718
    ##  62 casual        docked_bike   31     5657
    ##  63 casual        electric_bike 01    16400
    ##  64 casual        electric_bike 02    16417
    ##  65 casual        electric_bike 03    17180
    ##  66 casual        electric_bike 04    18433
    ##  67 casual        electric_bike 05    18256
    ##  68 casual        electric_bike 06    15597
    ##  69 casual        electric_bike 07    16362
    ##  70 casual        electric_bike 08    15255
    ##  71 casual        electric_bike 09    19222
    ##  72 casual        electric_bike 10    18751
    ##  73 casual        electric_bike 11    16959
    ##  74 casual        electric_bike 12    17238
    ##  75 casual        electric_bike 13    16827
    ##  76 casual        electric_bike 14    17949
    ##  77 casual        electric_bike 15    17149
    ##  78 casual        electric_bike 16    21178
    ##  79 casual        electric_bike 17    20205
    ##  80 casual        electric_bike 18    17915
    ##  81 casual        electric_bike 19    18906
    ##  82 casual        electric_bike 20    20343
    ##  83 casual        electric_bike 21    18747
    ##  84 casual        electric_bike 22    16464
    ##  85 casual        electric_bike 23    19798
    ##  86 casual        electric_bike 24    16322
    ##  87 casual        electric_bike 25    15173
    ##  88 casual        electric_bike 26    17350
    ##  89 casual        electric_bike 27    16253
    ##  90 casual        electric_bike 28    17937
    ##  91 casual        electric_bike 29    16946
    ##  92 casual        electric_bike 30    19659
    ##  93 casual        electric_bike 31    10074
    ##  94 member        classic_bike  01    69520
    ##  95 member        classic_bike  02    66026
    ##  96 member        classic_bike  03    60099
    ##  97 member        classic_bike  04    62262
    ##  98 member        classic_bike  05    62592
    ##  99 member        classic_bike  06    58197
    ## 100 member        classic_bike  07    62721
    ## 101 member        classic_bike  08    63230
    ## 102 member        classic_bike  09    71800
    ## 103 member        classic_bike  10    68894
    ## 104 member        classic_bike  11    61141
    ## 105 member        classic_bike  12    66296
    ## 106 member        classic_bike  13    67352
    ## 107 member        classic_bike  14    75014
    ## 108 member        classic_bike  15    66929
    ## 109 member        classic_bike  16    72985
    ## 110 member        classic_bike  17    68421
    ## 111 member        classic_bike  18    63047
    ## 112 member        classic_bike  19    65983
    ## 113 member        classic_bike  20    68543
    ## 114 member        classic_bike  21    67399
    ## 115 member        classic_bike  22    61090
    ## 116 member        classic_bike  23    68086
    ## 117 member        classic_bike  24    53991
    ## 118 member        classic_bike  25    51532
    ## 119 member        classic_bike  26    61533
    ## 120 member        classic_bike  27    60910
    ## 121 member        classic_bike  28    62186
    ## 122 member        classic_bike  29    60010
    ## 123 member        classic_bike  30    61333
    ## 124 member        classic_bike  31    38876
    ## 125 member        electric_bike 01    23414
    ## 126 member        electric_bike 02    23205
    ## 127 member        electric_bike 03    21454
    ## 128 member        electric_bike 04    23258
    ## 129 member        electric_bike 05    20715
    ## 130 member        electric_bike 06    20435
    ## 131 member        electric_bike 07    21454
    ## 132 member        electric_bike 08    22958
    ## 133 member        electric_bike 09    25697
    ## 134 member        electric_bike 10    24043
    ## 135 member        electric_bike 11    21675
    ## 136 member        electric_bike 12    22898
    ## 137 member        electric_bike 13    23915
    ## 138 member        electric_bike 14    24948
    ## 139 member        electric_bike 15    24034
    ## 140 member        electric_bike 16    27625
    ## 141 member        electric_bike 17    24546
    ## 142 member        electric_bike 18    22308
    ## 143 member        electric_bike 19    24375
    ## 144 member        electric_bike 20    25312
    ## 145 member        electric_bike 21    25264
    ## 146 member        electric_bike 22    22926
    ## 147 member        electric_bike 23    24804
    ## 148 member        electric_bike 24    20632
    ## 149 member        electric_bike 25    18386
    ## 150 member        electric_bike 26    21285
    ## 151 member        electric_bike 27    22899
    ## 152 member        electric_bike 28    23050
    ## 153 member        electric_bike 29    21348
    ## 154 member        electric_bike 30    22420
    ## 155 member        electric_bike 31    12297

### Average ride length

``` r
cyclistic %>% 
    summarise(Mean = mean(ride_length))
```

    ##            Mean
    ## 1 19.26432 mins

### Average ride length by member type

``` r
cyclistic %>% 
    group_by(member_casual) %>% 
    summarise(Mean = mean(ride_length))
```

    ## # A tibble: 2 × 2
    ##   member_casual Mean         
    ##   <chr>         <drtn>       
    ## 1 casual        28.08960 mins
    ## 2 member        12.63444 mins

### Average ride length by bike type

``` r
cyclistic %>% 
    group_by(rideable_type) %>% 
    summarise(Mean = mean(ride_length))
```

    ## # A tibble: 3 × 2
    ##   rideable_type Mean         
    ##   <chr>         <drtn>       
    ## 1 classic_bike  17.63914 mins
    ## 2 docked_bike   63.38972 mins
    ## 3 electric_bike 14.48409 mins

### Average ride length by member type and month

``` r
cyclistic %>% 
    group_by(member_casual, month) %>% 
    summarise(Mean = mean(ride_length)) %>%
    print(n = 24)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 24 × 3
    ## # Groups:   member_casual [2]
    ##    member_casual month Mean         
    ##    <chr>         <chr> <drtn>       
    ##  1 casual        01    27.39278 mins
    ##  2 casual        02    24.84334 mins
    ##  3 casual        03    28.42243 mins
    ##  4 casual        04    25.93427 mins
    ##  5 casual        05    27.71626 mins
    ##  6 casual        06    25.02614 mins
    ##  7 casual        07    33.26832 mins
    ##  8 casual        08    28.56429 mins
    ##  9 casual        09    28.09373 mins
    ## 10 casual        10    26.34168 mins
    ## 11 casual        11    22.48419 mins
    ## 12 casual        12    24.84249 mins
    ## 13 member        01    10.26502 mins
    ## 14 member        02    10.64437 mins
    ## 15 member        03    11.79644 mins
    ## 16 member        04    11.61323 mins
    ## 17 member        05    13.30309 mins
    ## 18 member        06    13.68226 mins
    ## 19 member        07    13.79220 mins
    ## 20 member        08    13.55207 mins
    ## 21 member        09    13.13019 mins
    ## 22 member        10    12.02429 mins
    ## 23 member        11    10.95229 mins
    ## 24 member        12    10.58330 mins

### Average ride length by member type and weekday

``` r
cyclistic %>% 
    group_by(member_casual, weekday) %>% 
    summarise(Mean = mean(ride_length)) %>%
    print(n = 14)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 14 × 3
    ## # Groups:   member_casual [2]
    ##    member_casual weekday   Mean         
    ##    <chr>         <chr>     <drtn>       
    ##  1 casual        Friday    26.11325 mins
    ##  2 casual        Monday    28.98815 mins
    ##  3 casual        Saturday  30.65512 mins
    ##  4 casual        Sunday    32.63316 mins
    ##  5 casual        Thursday  25.31752 mins
    ##  6 casual        Tuesday   24.27012 mins
    ##  7 casual        Wednesday 23.88299 mins
    ##  8 member        Friday    12.37553 mins
    ##  9 member        Monday    12.24853 mins
    ## 10 member        Saturday  14.25601 mins
    ## 11 member        Sunday    14.42716 mins
    ## 12 member        Thursday  12.09998 mins
    ## 13 member        Tuesday   11.77665 mins
    ## 14 member        Wednesday 11.93057 mins

### Average ride length by member type and day

``` r
cyclistic %>% 
    group_by(member_casual, day) %>% 
    summarise(Mean = mean(ride_length)) %>%
    print(n = 62)
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 62 × 3
    ## # Groups:   member_casual [2]
    ##    member_casual day   Mean         
    ##    <chr>         <chr> <drtn>       
    ##  1 casual        01    29.01449 mins
    ##  2 casual        02    28.05755 mins
    ##  3 casual        03    30.28286 mins
    ##  4 casual        04    32.57471 mins
    ##  5 casual        05    33.03447 mins
    ##  6 casual        06    29.04510 mins
    ##  7 casual        07    27.30683 mins
    ##  8 casual        08    30.08160 mins
    ##  9 casual        09    27.97190 mins
    ## 10 casual        10    30.14879 mins
    ## 11 casual        11    27.46964 mins
    ## 12 casual        12    28.39759 mins
    ## 13 casual        13    27.27638 mins
    ## 14 casual        14    28.01550 mins
    ## 15 casual        15    26.81736 mins
    ## 16 casual        16    27.51056 mins
    ## 17 casual        17    27.67553 mins
    ## 18 casual        18    29.04067 mins
    ## 19 casual        19    27.87177 mins
    ## 20 casual        20    25.79946 mins
    ## 21 casual        21    26.26966 mins
    ## 22 casual        22    26.48891 mins
    ## 23 casual        23    28.58989 mins
    ## 24 casual        24    27.10763 mins
    ## 25 casual        25    26.75112 mins
    ## 26 casual        26    27.30622 mins
    ## 27 casual        27    24.43799 mins
    ## 28 casual        28    26.79480 mins
    ## 29 casual        29    26.98472 mins
    ## 30 casual        30    25.98923 mins
    ## 31 casual        31    28.57322 mins
    ## 32 member        01    12.40943 mins
    ## 33 member        02    12.31326 mins
    ## 34 member        03    12.64885 mins
    ## 35 member        04    12.57836 mins
    ## 36 member        05    13.38731 mins
    ## 37 member        06    12.48501 mins
    ## 38 member        07    12.58860 mins
    ## 39 member        08    12.26782 mins
    ## 40 member        09    12.67568 mins
    ## 41 member        10    12.83967 mins
    ## 42 member        11    12.77472 mins
    ## 43 member        12    12.54463 mins
    ## 44 member        13    12.33304 mins
    ## 45 member        14    12.78445 mins
    ## 46 member        15    12.39508 mins
    ## 47 member        16    12.52345 mins
    ## 48 member        17    12.86749 mins
    ## 49 member        18    13.13993 mins
    ## 50 member        19    12.89281 mins
    ## 51 member        20    12.55741 mins
    ## 52 member        21    12.67643 mins
    ## 53 member        22    12.46713 mins
    ## 54 member        23    12.82177 mins
    ## 55 member        24    12.45092 mins
    ## 56 member        25    12.76171 mins
    ## 57 member        26    12.79983 mins
    ## 58 member        27    11.97778 mins
    ## 59 member        28    12.53466 mins
    ## 60 member        29    12.71826 mins
    ## 61 member        30    12.59350 mins
    ## 62 member        31    13.01600 mins

### Average ride length by member and bike type

``` r
cyclistic %>% 
    group_by(member_casual, rideable_type) %>% 
    summarise(Mean = mean(ride_length))
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 3
    ## # Groups:   member_casual [2]
    ##   member_casual rideable_type Mean         
    ##   <chr>         <chr>         <drtn>       
    ## 1 casual        classic_bike  24.99549 mins
    ## 2 casual        docked_bike   63.38972 mins
    ## 3 casual        electric_bike 18.58991 mins
    ## 4 member        classic_bike  13.10241 mins
    ## 5 member        electric_bike 11.32548 mins

### Average ride length by member, bike type and month

``` r
cyclistic %>% 
    group_by(member_casual, rideable_type, month) %>% 
    summarise(Mean = mean(ride_length)) %>%
    print(n = 60)
```

    ## `summarise()` has grouped output by 'member_casual', 'rideable_type'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 60 × 4
    ## # Groups:   member_casual, rideable_type [5]
    ##    member_casual rideable_type month Mean           
    ##    <chr>         <chr>         <chr> <drtn>         
    ##  1 casual        classic_bike  01     20.422232 mins
    ##  2 casual        classic_bike  02     23.286011 mins
    ##  3 casual        classic_bike  03     27.270241 mins
    ##  4 casual        classic_bike  04     26.095355 mins
    ##  5 casual        classic_bike  05     27.239980 mins
    ##  6 casual        classic_bike  06     24.554301 mins
    ##  7 casual        classic_bike  07     25.629271 mins
    ##  8 casual        classic_bike  08     24.833112 mins
    ##  9 casual        classic_bike  09     24.442006 mins
    ## 10 casual        classic_bike  10     23.879100 mins
    ## 11 casual        classic_bike  11     21.048974 mins
    ## 12 casual        classic_bike  12     21.946778 mins
    ## 13 casual        docked_bike   01    158.971919 mins
    ## 14 casual        docked_bike   02     83.734405 mins
    ## 15 casual        docked_bike   03     63.038613 mins
    ## 16 casual        docked_bike   04     48.391500 mins
    ## 17 casual        docked_bike   05     52.000554 mins
    ## 18 casual        docked_bike   06     48.976732 mins
    ## 19 casual        docked_bike   07     79.834202 mins
    ## 20 casual        docked_bike   08     59.253208 mins
    ## 21 casual        docked_bike   09     63.197759 mins
    ## 22 casual        docked_bike   10     62.059616 mins
    ## 23 casual        docked_bike   11     60.243173 mins
    ## 24 casual        docked_bike   12     83.102569 mins
    ## 25 casual        electric_bike 01     11.570023 mins
    ## 26 casual        electric_bike 02     13.249615 mins
    ## 27 casual        electric_bike 03     18.120238 mins
    ## 28 casual        electric_bike 04     17.430465 mins
    ## 29 casual        electric_bike 05     19.316237 mins
    ## 30 casual        electric_bike 06     18.055209 mins
    ## 31 casual        electric_bike 07     21.376323 mins
    ## 32 casual        electric_bike 08     20.703966 mins
    ## 33 casual        electric_bike 09     19.668900 mins
    ## 34 casual        electric_bike 10     17.347010 mins
    ## 35 casual        electric_bike 11     14.664301 mins
    ## 36 casual        electric_bike 12     13.762199 mins
    ## 37 member        classic_bike  01     10.598921 mins
    ## 38 member        classic_bike  02     11.003674 mins
    ## 39 member        classic_bike  03     12.449165 mins
    ## 40 member        classic_bike  04     12.246201 mins
    ## 41 member        classic_bike  05     13.848791 mins
    ## 42 member        classic_bike  06     14.331019 mins
    ## 43 member        classic_bike  07     13.981159 mins
    ## 44 member        classic_bike  08     13.710984 mins
    ## 45 member        classic_bike  09     13.308267 mins
    ## 46 member        classic_bike  10     12.395509 mins
    ## 47 member        classic_bike  11     11.530364 mins
    ## 48 member        classic_bike  12     11.139923 mins
    ## 49 member        electric_bike 01      9.455553 mins
    ## 50 member        electric_bike 02      9.840547 mins
    ## 51 member        electric_bike 03     10.498728 mins
    ## 52 member        electric_bike 04     10.387443 mins
    ## 53 member        electric_bike 05     12.023216 mins
    ## 54 member        electric_bike 06     12.008749 mins
    ## 55 member        electric_bike 07     12.927829 mins
    ## 56 member        electric_bike 08     12.833133 mins
    ## 57 member        electric_bike 09     12.364004 mins
    ## 58 member        electric_bike 10     11.034904 mins
    ## 59 member        electric_bike 11      9.851125 mins
    ## 60 member        electric_bike 12      9.698607 mins

### Average ride length by member, bike type and weekday

``` r
cyclistic %>% 
    group_by(member_casual, rideable_type, weekday) %>% 
    summarise(Mean = mean(ride_length)) %>%
    print(n = 35)
```

    ## `summarise()` has grouped output by 'member_casual', 'rideable_type'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 35 × 4
    ## # Groups:   member_casual, rideable_type [5]
    ##    member_casual rideable_type weekday   Mean         
    ##    <chr>         <chr>         <chr>     <drtn>       
    ##  1 casual        classic_bike  Friday    23.39286 mins
    ##  2 casual        classic_bike  Monday    25.74094 mins
    ##  3 casual        classic_bike  Saturday  27.04128 mins
    ##  4 casual        classic_bike  Sunday    28.24847 mins
    ##  5 casual        classic_bike  Thursday  22.21173 mins
    ##  6 casual        classic_bike  Tuesday   22.43601 mins
    ##  7 casual        classic_bike  Wednesday 21.85554 mins
    ##  8 casual        docked_bike   Friday    59.56076 mins
    ##  9 casual        docked_bike   Monday    65.46265 mins
    ## 10 casual        docked_bike   Saturday  62.46077 mins
    ## 11 casual        docked_bike   Sunday    68.38017 mins
    ## 12 casual        docked_bike   Thursday  67.85613 mins
    ## 13 casual        docked_bike   Tuesday   57.25971 mins
    ## 14 casual        docked_bike   Wednesday 58.18100 mins
    ## 15 casual        electric_bike Friday    17.72995 mins
    ## 16 casual        electric_bike Monday    18.80621 mins
    ## 17 casual        electric_bike Saturday  20.93548 mins
    ## 18 casual        electric_bike Sunday    21.82998 mins
    ## 19 casual        electric_bike Thursday  16.63099 mins
    ## 20 casual        electric_bike Tuesday   16.34854 mins
    ## 21 casual        electric_bike Wednesday 16.33187 mins
    ## 22 member        classic_bike  Friday    12.80128 mins
    ## 23 member        classic_bike  Monday    12.72262 mins
    ## 24 member        classic_bike  Saturday  14.71053 mins
    ## 25 member        classic_bike  Sunday    14.83612 mins
    ## 26 member        classic_bike  Thursday  12.54970 mins
    ## 27 member        classic_bike  Tuesday   12.24122 mins
    ## 28 member        classic_bike  Wednesday 12.38355 mins
    ## 29 member        electric_bike Friday    11.22686 mins
    ## 30 member        electric_bike Monday    10.94369 mins
    ## 31 member        electric_bike Saturday  12.81829 mins
    ## 32 member        electric_bike Sunday    13.10390 mins
    ## 33 member        electric_bike Thursday  10.89446 mins
    ## 34 member        electric_bike Tuesday   10.56110 mins
    ## 35 member        electric_bike Wednesday 10.71890 mins

### Average ride length by member, bike type and day

``` r
cyclistic %>% 
    group_by(member_casual, rideable_type, day) %>% 
    summarise(Mean = mean(ride_length)) %>%
    print(n = 155)
```

    ## `summarise()` has grouped output by 'member_casual', 'rideable_type'. You can
    ## override using the `.groups` argument.

    ## # A tibble: 155 × 4
    ## # Groups:   member_casual, rideable_type [5]
    ##     member_casual rideable_type day   Mean         
    ##     <chr>         <chr>         <chr> <drtn>       
    ##   1 casual        classic_bike  01    24.11465 mins
    ##   2 casual        classic_bike  02    24.28366 mins
    ##   3 casual        classic_bike  03    25.41391 mins
    ##   4 casual        classic_bike  04    26.17049 mins
    ##   5 casual        classic_bike  05    27.83888 mins
    ##   6 casual        classic_bike  06    25.29367 mins
    ##   7 casual        classic_bike  07    25.02142 mins
    ##   8 casual        classic_bike  08    23.68401 mins
    ##   9 casual        classic_bike  09    24.52136 mins
    ##  10 casual        classic_bike  10    25.79685 mins
    ##  11 casual        classic_bike  11    24.87201 mins
    ##  12 casual        classic_bike  12    25.15974 mins
    ##  13 casual        classic_bike  13    24.55929 mins
    ##  14 casual        classic_bike  14    25.32001 mins
    ##  15 casual        classic_bike  15    24.02767 mins
    ##  16 casual        classic_bike  16    24.19160 mins
    ##  17 casual        classic_bike  17    25.20516 mins
    ##  18 casual        classic_bike  18    26.11182 mins
    ##  19 casual        classic_bike  19    25.39729 mins
    ##  20 casual        classic_bike  20    23.78625 mins
    ##  21 casual        classic_bike  21    24.43935 mins
    ##  22 casual        classic_bike  22    24.90787 mins
    ##  23 casual        classic_bike  23    26.29833 mins
    ##  24 casual        classic_bike  24    24.17038 mins
    ##  25 casual        classic_bike  25    24.68391 mins
    ##  26 casual        classic_bike  26    24.95259 mins
    ##  27 casual        classic_bike  27    23.00694 mins
    ##  28 casual        classic_bike  28    24.85824 mins
    ##  29 casual        classic_bike  29    25.18382 mins
    ##  30 casual        classic_bike  30    24.48141 mins
    ##  31 casual        classic_bike  31    25.03317 mins
    ##  32 casual        docked_bike   01    78.38195 mins
    ##  33 casual        docked_bike   02    69.51138 mins
    ##  34 casual        docked_bike   03    72.85466 mins
    ##  35 casual        docked_bike   04    82.01221 mins
    ##  36 casual        docked_bike   05    75.11858 mins
    ##  37 casual        docked_bike   06    68.16598 mins
    ##  38 casual        docked_bike   07    55.55353 mins
    ##  39 casual        docked_bike   08    91.66244 mins
    ##  40 casual        docked_bike   09    65.16418 mins
    ##  41 casual        docked_bike   10    72.58527 mins
    ##  42 casual        docked_bike   11    58.34451 mins
    ##  43 casual        docked_bike   12    66.02285 mins
    ##  44 casual        docked_bike   13    59.55146 mins
    ##  45 casual        docked_bike   14    60.44645 mins
    ##  46 casual        docked_bike   15    60.96395 mins
    ##  47 casual        docked_bike   16    64.46875 mins
    ##  48 casual        docked_bike   17    55.48197 mins
    ##  49 casual        docked_bike   18    60.58116 mins
    ##  50 casual        docked_bike   19    59.80968 mins
    ##  51 casual        docked_bike   20    54.58629 mins
    ##  52 casual        docked_bike   21    54.21136 mins
    ##  53 casual        docked_bike   22    54.61900 mins
    ##  54 casual        docked_bike   23    59.14376 mins
    ##  55 casual        docked_bike   24    62.97685 mins
    ##  56 casual        docked_bike   25    53.46044 mins
    ##  57 casual        docked_bike   26    59.42563 mins
    ##  58 casual        docked_bike   27    53.56907 mins
    ##  59 casual        docked_bike   28    57.03386 mins
    ##  60 casual        docked_bike   29    55.04251 mins
    ##  61 casual        docked_bike   30    52.72682 mins
    ##  62 casual        docked_bike   31    62.95432 mins
    ##  63 casual        electric_bike 01    17.88985 mins
    ##  64 casual        electric_bike 02    17.94101 mins
    ##  65 casual        electric_bike 03    19.11641 mins
    ##  66 casual        electric_bike 04    18.98773 mins
    ##  67 casual        electric_bike 05    20.99252 mins
    ##  68 casual        electric_bike 06    18.67915 mins
    ##  69 casual        electric_bike 07    18.83123 mins
    ##  70 casual        electric_bike 08    17.70903 mins
    ##  71 casual        electric_bike 09    18.80335 mins
    ##  72 casual        electric_bike 10    19.04673 mins
    ##  73 casual        electric_bike 11    18.67249 mins
    ##  74 casual        electric_bike 12    18.16869 mins
    ##  75 casual        electric_bike 13    18.38856 mins
    ##  76 casual        electric_bike 14    18.72452 mins
    ##  77 casual        electric_bike 15    17.60624 mins
    ##  78 casual        electric_bike 16    18.30802 mins
    ##  79 casual        electric_bike 17    18.93022 mins
    ##  80 casual        electric_bike 18    19.55854 mins
    ##  81 casual        electric_bike 19    18.80313 mins
    ##  82 casual        electric_bike 20    18.28715 mins
    ##  83 casual        electric_bike 21    18.32582 mins
    ##  84 casual        electric_bike 22    18.06483 mins
    ##  85 casual        electric_bike 23    19.37740 mins
    ##  86 casual        electric_bike 24    17.83658 mins
    ##  87 casual        electric_bike 25    18.50088 mins
    ##  88 casual        electric_bike 26    18.73727 mins
    ##  89 casual        electric_bike 27    16.90090 mins
    ##  90 casual        electric_bike 28    18.25215 mins
    ##  91 casual        electric_bike 29    18.94345 mins
    ##  92 casual        electric_bike 30    18.57015 mins
    ##  93 casual        electric_bike 31    18.77815 mins
    ##  94 member        classic_bike  01    12.83524 mins
    ##  95 member        classic_bike  02    12.69453 mins
    ##  96 member        classic_bike  03    13.13475 mins
    ##  97 member        classic_bike  04    13.05568 mins
    ##  98 member        classic_bike  05    13.88457 mins
    ##  99 member        classic_bike  06    12.97683 mins
    ## 100 member        classic_bike  07    13.01360 mins
    ## 101 member        classic_bike  08    12.66920 mins
    ## 102 member        classic_bike  09    13.09206 mins
    ## 103 member        classic_bike  10    13.35435 mins
    ## 104 member        classic_bike  11    13.29011 mins
    ## 105 member        classic_bike  12    12.95112 mins
    ## 106 member        classic_bike  13    12.75684 mins
    ## 107 member        classic_bike  14    13.28432 mins
    ## 108 member        classic_bike  15    12.88867 mins
    ## 109 member        classic_bike  16    12.96838 mins
    ## 110 member        classic_bike  17    13.35732 mins
    ## 111 member        classic_bike  18    13.73722 mins
    ## 112 member        classic_bike  19    13.41673 mins
    ## 113 member        classic_bike  20    12.95058 mins
    ## 114 member        classic_bike  21    13.22377 mins
    ## 115 member        classic_bike  22    12.94050 mins
    ## 116 member        classic_bike  23    13.25026 mins
    ## 117 member        classic_bike  24    12.94582 mins
    ## 118 member        classic_bike  25    13.25674 mins
    ## 119 member        classic_bike  26    13.21754 mins
    ## 120 member        classic_bike  27    12.42963 mins
    ## 121 member        classic_bike  28    13.02261 mins
    ## 122 member        classic_bike  29    13.23099 mins
    ## 123 member        classic_bike  30    13.02488 mins
    ## 124 member        classic_bike  31    13.45456 mins
    ## 125 member        electric_bike 01    11.14515 mins
    ## 126 member        electric_bike 02    11.22842 mins
    ## 127 member        electric_bike 03    11.28769 mins
    ## 128 member        electric_bike 04    11.30057 mins
    ## 129 member        electric_bike 05    11.88480 mins
    ## 130 member        electric_bike 06    11.08434 mins
    ## 131 member        electric_bike 07    11.34611 mins
    ## 132 member        electric_bike 08    11.16233 mins
    ## 133 member        electric_bike 09    11.51227 mins
    ## 134 member        electric_bike 10    11.36486 mins
    ## 135 member        electric_bike 11    11.32087 mins
    ## 136 member        electric_bike 12    11.36775 mins
    ## 137 member        electric_bike 13    11.13949 mins
    ## 138 member        electric_bike 14    11.28144 mins
    ## 139 member        electric_bike 15    11.02056 mins
    ## 140 member        electric_bike 16    11.34795 mins
    ## 141 member        electric_bike 17    11.50214 mins
    ## 142 member        electric_bike 18    11.45187 mins
    ## 143 member        electric_bike 19    11.47456 mins
    ## 144 member        electric_bike 20    11.49274 mins
    ## 145 member        electric_bike 21    11.21623 mins
    ## 146 member        electric_bike 22    11.20575 mins
    ## 147 member        electric_bike 23    11.64556 mins
    ## 148 member        electric_bike 24    11.15583 mins
    ## 149 member        electric_bike 25    11.37423 mins
    ## 150 member        electric_bike 26    11.59228 mins
    ## 151 member        electric_bike 27    10.77590 mins
    ## 152 member        electric_bike 28    11.21822 mins
    ## 153 member        electric_bike 29    11.27695 mins
    ## 154 member        electric_bike 30    11.41338 mins
    ## 155 member        electric_bike 31    11.62950 mins

### Min ride length by member type

``` r
cyclistic %>% 
    group_by(member_casual) %>% 
    summarise(Min = min(ride_length))
```

    ## # A tibble: 2 × 2
    ##   member_casual Min      
    ##   <chr>         <drtn>   
    ## 1 casual        0.02 mins
    ## 2 member        0.02 mins

### Min ride length by member and bike type

``` r
cyclistic %>% 
    group_by(member_casual, rideable_type) %>% 
    summarise(Min = min(ride_length))
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 3
    ## # Groups:   member_casual [2]
    ##   member_casual rideable_type Min      
    ##   <chr>         <chr>         <drtn>   
    ## 1 casual        classic_bike  0.02 mins
    ## 2 casual        docked_bike   0.02 mins
    ## 3 casual        electric_bike 0.02 mins
    ## 4 member        classic_bike  0.02 mins
    ## 5 member        electric_bike 0.02 mins

### Max ride length by member type

``` r
cyclistic %>% 
    group_by(member_casual) %>% 
    summarise(Max = max(ride_length))
```

    ## # A tibble: 2 × 2
    ##   member_casual Max          
    ##   <chr>         <drtn>       
    ## 1 casual        49107.15 mins
    ## 2 member         1492.92 mins

### Max ride length by member and bike type

``` r
cyclistic %>% 
    group_by(member_casual, rideable_type) %>% 
    summarise(Max = max(ride_length))
```

    ## `summarise()` has grouped output by 'member_casual'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 5 × 3
    ## # Groups:   member_casual [2]
    ##   member_casual rideable_type Max          
    ##   <chr>         <chr>         <drtn>       
    ## 1 casual        classic_bike   1499.90 mins
    ## 2 casual        docked_bike   49107.15 mins
    ## 3 casual        electric_bike   480.00 mins
    ## 4 member        classic_bike   1492.92 mins
    ## 5 member        electric_bike   478.53 mins

### Save new data set

``` r
write.csv(cyclistic,"cyclistic.csv")
```
