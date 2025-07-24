# run_ml works for random forest with grouping & feature importance

    Code
      res <- mikropml::run_ml(otu_mini_bin, "rf", outcome_colname = "dx",
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

