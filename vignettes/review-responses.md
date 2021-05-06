We thank the reviewers for their thoughtful feedback. We have responded to all reviewer comments below. Feel free to let us know if you have additional questions.

## JonnyTran reviews

> When testing `preprocess_data()` and `run_ml()` on a larger dataset with 1917 rows × 14426 columns, it would take an indeterminately long time. It should have a progress bar indicating the iteration #, and the estimated duration for each iteration, similar to Python's `tqdm`. 

We opened and resolved issue [#249](https://github.com/SchlossLab/mikropml/issues/249) related to this suggestion.
We have implemented this feature for `preprocess_data()` and the feature importance part of `run_ml()`.
Unfortunately, we cannot do it for model training as we use `caret::train()`, which does not support having a progress bar.
 
> Additionally, it can also provide live updates of the loss and other metrics as the model is being trained, to help the user stop the run if it is not satisfactory.

Another limitation of using `caret::train()` to train the model is that there is no option to display updates on relevant metrics as the model is being trained.
As much of our package is based on using this function, we unfortunately cannot easily implement this suggestion. 
(See issue [#250](https://github.com/SchlossLab/mikropml/issues/250))

> I also encountered a bug when running `run_ml()` on a dataset with class name containing spaces in the label column. I can mitigate the bug by replacing all `" "`'s with `"_"`'s. 

Thank you for bringing this to our attention. We fixed this issue (issue [#244](https://github.com/SchlossLab/mikropml/issues/244)).

> Currently, it doesn't seem to be able to support multi-label classifications.

Unfortunately `caret` does not support this functionality so we believe it's outside the scope of this package (see issue [#252](https://github.com/SchlossLab/mikropml/issues/252)).

> The `run_ml()` function currently implements 5 off-the-shelve ML algorithms, while providing 12 other parameters for training criteria, hyperparameters, feature importance, etc.. If in the future it would support more algorithms, custom metrics, or training parameters, I'd imagine there'll be limitations imposed by the function arguments. I'd suggest the function to take in 3 objects, e.g. `run_ml(dataset, model, metrics, [args])`, where a metrics object can allow the user select standard metrics or define their own metric functions given the model output and true labels.

This is a very good point that we have responded to in issue [#251](https://github.com/SchlossLab/mikropml/issues/251). 
We chose to structure `run_ml` in a way that is easier for our users, rather than extremely flexible. 
If users want a more customizable experience, especially related to what models they want to use, we expect them to use other packages not developed for machine learning novices.
Furthermore, `run_ml()` already takes these objects:

1. `dataset`: The input dataset.
1.  `method`: The ML model to be used. While we only officially support 5 models (as these are the ones we tested), all of the models supported by `caret` (https://topepo.github.io/caret/available-models.html) should work in our package. If `caret` supports additional models in the future, these should also work in `mikropml`. We realize that the model options are not as generalizable as e.g. PyTorch, since users must choose from options supported by `caret`. However, our code heavily relies on `caret` to perform the underlying model training. Additionally, as `mikropml` is oriented toward beginner practitioners, we believe that it does not need to provide the option to include custom models.
1. `perf_metric_function` and `perf_metric_name`: The performance metric to be used. We chose sensible defaults, but the user can provide their own performance metrics if they would like.
1. `hyperparameters`: The values of hyperparameters in the model that the user would like to tune.

## FedericoComoglio reviews

> I agree with @JonnyTran 's suggestion to display time to completion estimates and relevant metrics (e.g. loss or chosen metrics for current iteration) during training. The average user will find this helpful. This would also be the case when parallelizing `run_ml()` execution.

See response above.

> In a number of places across the vignette, "machine learning" (e.g. "run machine learning" in the "Preprocessing data" vignette) is used in place of a likely more appropriate "model training"/"train model"

Thank you for this suggestion. We have changed the wording in several places (issue [#253](https://github.com/SchlossLab/mikropml/issues/253) opened and resolved).

> For training random forest models, it is unfortunate that the number of trees can not be tuned. However, this is a consequence of design choices that pertain to the caret package and hence not directly related to this work.

We wholeheartedly agree.
