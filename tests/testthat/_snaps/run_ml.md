# run_ml works for random forest with grouping & feature importance

    Code
      mikropml::run_ml(otu_mini_bin, "rf", outcome_colname = "dx",
        find_feature_importance = TRUE, seed = 2019, cv_times = 2, groups = otu_mini_group)
    Message
      Using 'dx' as the outcome column.
      Fraction of data in the training set: 0.8 
      	Groups in the training set: A B C D E 
      	Groups in the testing set: F
      Groups will be kept together in CV partitions
      Training the model...
      Training complete.
      Finding feature importance...
      Feature importance complete.
    Output
      $trained_model
      Random Forest 
      
      160 samples
       10 predictor
        2 classes: 'cancer', 'normal' 
      
      No pre-processing
      Resampling: Cross-Validated (5 fold, repeated 2 times) 
      Summary of sample sizes: 120, 97, 128, 135, 128, 120, ... 
      Resampling results across tuning parameters:
      
        mtry  logLoss    AUC        prAUC      Accuracy   Kappa      F1       
        2     0.6435884  0.7073270  0.6442578  0.6586670  0.3236935  0.6621000
        3     0.6564865  0.6946197  0.6331542  0.6466605  0.2990978  0.6525042
        6     0.6659419  0.6767137  0.6210179  0.6489894  0.3001880  0.6571748
        Sensitivity  Specificity  Pos_Pred_Value  Neg_Pred_Value  Precision
        0.6821043    0.6469879    0.6588016       0.6770246       0.6588016
        0.6783488    0.6255669    0.6428523       0.6674931       0.6428523
        0.6886603    0.6169424    0.6372331       0.6707107       0.6372331
        Recall     Detection_Rate  Balanced_Accuracy
        0.6821043  0.3321559       0.6645461        
        0.6783488  0.3311037       0.6519579        
        0.6886603  0.3362129       0.6528013        
      
      AUC was used to select the optimal model using the largest value.
      The final value used for the model was mtry = 2.
      
      $test_data
              dx Otu00009 Otu00005 Otu00010 Otu00001 Otu00008 Otu00004 Otu00003
      2   normal      119      671      115      568      204      293       13
      6   normal      155      332      122      167      251        4      213
      7   cancer       10       30       55      108      277      349      160
      11  normal      244       91      292      843      180     1684        0
      12  normal      342      343      319     1404      381       81      103
      28  cancer      277        9       23      115      324        8      139
      29  normal      299       26      281      287        1     2059      118
      33  normal      521       26       62     1491      398       87      280
      43  normal     1774        1      403     1636        3     1866        7
      45  normal      515      125      514      905       12     1365       53
      49  normal       12      197      641       45      159      738      131
      54  normal      162       53       68     1513        0      840        1
      60  normal      201       99      183      356      359        0      188
      62  normal      296      387      107     1063        8       31       64
      65  normal       37       83      121       26       38      542      130
      66  normal      216       80       90      222      838      940      757
      73  cancer       64      314       80      296      189      282      320
      75  normal       87      376      154      152      137      192      158
      83  normal      346      250      490     1334        7        1      128
      89  cancer       71      576      100      332      445      132      546
      95  normal       87      384       50      497      342       46      143
      104 normal      811      700      138      852      272      461      470
      106 normal       15      261        3      475      566       14      149
      110 cancer      170       52       50      223      271        0       63
      112 cancer      106        2        0      329        1        4      657
      116 normal       68      316      217      622      131      375      125
      130 cancer      107     1080       55      449        0     1646      973
      141 cancer      275       65       25       55        0        3      196
      142 cancer       13      230        2       30      180     1782      558
      146 cancer      196        9      143      193       67        1       40
      161 cancer      161        3      192      189      738        8       13
      162 cancer       29     1859      388      627        0        0        1
      163 cancer      121       27      196      468      699      201       80
      167 cancer       37       12      214       18      177      700      496
      168 cancer      227        2      667     2671      881        3        2
      178 cancer      480      126      226     1927        0       93      218
      180 cancer       50       92        6       62        6      121       96
      182 cancer      217      592       46       71      608        2      242
      187 cancer      535      233      655      693        0     1627       59
      199 cancer      217      582      987     1284       27        5       60
          Otu00002 Otu00007 Otu00006
      2       1320       48      103
      6        712      139      534
      7        120       26      971
      11         0      259      207
      12       386        8      171
      28        67      262        4
      29        87       85      932
      33        74      512       35
      43        15        2       86
      45       160        3      286
      49       343        6       28
      54       140     1389       44
      60       129      119       29
      62       410      212      514
      65        91        8       32
      66         0      373       55
      73       644      187      230
      75       548       36      132
      83       158      193      487
      89       805      197      243
      95       488      126      216
      104      298      317      158
      106      569       22       39
      110      144      138       53
      112        1      297        1
      116      343      513      309
      130      435      129       80
      141      556       47       50
      142      133       19       28
      146      134       53        2
      161       15      159       15
      162      649      288       45
      163       36      159      389
      167      655       49       91
      168        0      197        1
      178      365        2        0
      180      579       29      135
      182       68       53       50
      187      381        2        1
      199      198        2     1184
      
      $performance
      # A tibble: 1 x 17
        cv_metric_AUC logLoss   AUC prAUC Accuracy Kappa    F1 Sensitivity Specificity
                <dbl>   <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl>       <dbl>       <dbl>
      1         0.707   0.717 0.546 0.507    0.575  0.15 0.541         0.5        0.65
      # i 8 more variables: Pos_Pred_Value <dbl>, Neg_Pred_Value <dbl>,
      #   Precision <dbl>, Recall <dbl>, Detection_Rate <dbl>,
      #   Balanced_Accuracy <dbl>, method <chr>, seed <dbl>
      
      $feature_importance
         perf_metric perf_metric_diff     pvalue   lower   upper     feat method
      1    0.5459125        0.0003375 0.51485149 0.49125 0.60250 Otu00001     rf
      2    0.5682625       -0.0220125 0.72277228 0.50625 0.63125 Otu00002     rf
      3    0.5482875       -0.0020375 0.55445545 0.50500 0.59000 Otu00003     rf
      4    0.6314375       -0.0851875 1.00000000 0.55250 0.71250 Otu00004     rf
      5    0.4991750        0.0470750 0.08910891 0.44125 0.57125 Otu00005     rf
      6    0.5364875        0.0097625 0.28712871 0.50125 0.57375 Otu00006     rf
      7    0.5382875        0.0079625 0.39603960 0.47500 0.58750 Otu00007     rf
      8    0.5160500        0.0302000 0.09900990 0.46750 0.55750 Otu00008     rf
      9    0.5293375        0.0169125 0.16831683 0.49500 0.55625 Otu00009     rf
      10   0.4976500        0.0486000 0.12871287 0.41000 0.56250 Otu00010     rf
         perf_metric_name seed
      1               AUC 2019
      2               AUC 2019
      3               AUC 2019
      4               AUC 2019
      5               AUC 2019
      6               AUC 2019
      7               AUC 2019
      8               AUC 2019
      9               AUC 2019
      10              AUC 2019
      

