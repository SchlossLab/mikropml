# Testing the code

Test data: ```data/test_data.csv```

- Has 25 samples.
- All the OTUs that is in the original dataset and 1 column for the outcome (dx).
- Should take 1 minute to run.

To run the test code:

```
Rscript code/R/main.R data/input_data.csv 1 "L2_Logistic_Regression" 0 "dx"
```

- __1st argument:__ Input data

- __2nd argument:__ Set seed for Datasplit

- __3rd argument:__ Model type (we are focusing on logistic regression for now)

- __4th argument:__ To perform permutation importance or not (I'm working on this, so we will say FALSE/0 for this for now)

- __5th argument:__ The outcome we are trying to predict
