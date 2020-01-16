# How to Contribute

We use the [Gitflow](https://www.atlassian.com/git/tutorials/comparing-workflows/feature-branch-workflow) workflow to make our lives easier.
This means all edits should be made in separate branches -- not the `master` branch.
Only the project maintainer will make edits or merge on `master`.

View the open [issues](https://github.com/SchlossLab/ML_pipeline_microbiome/issues) to see the To-Do list.
If an issue isn't assigned to anyone, we would welcome your contribution!

If you need a refresher on Git or GitHub, see:
- Software Carpentry's lesson: [Version Control with Git](http://swcarpentry.github.io/git-novice/)
- Hadley Wickham's chapter on Git and GitHub in his book [R Packages](http://r-pkgs.had.co.nz/git.html)

## Contributing changes

1. Either `clone` or `fork` this repo.
    - **clone** if you have write access:
        ```
        git clone https://github.com/SchlossLab/ML_pipeline_microbiome
        ```
        (All Schlabbies have write access.)

    - **fork** if you do _not_ have write access by pressing the [Fork]((https://help.github.com/en/articles/fork-a-repo)) button on [GitHub](https://github.com/SchlossLab/ML_pipeline_microbiome), then clone your fork.

1. Move to the repo directory.

    ```
    cd ML_pipeline_microbiome
    ```

1. Create a new branch for your feature.

    Give it a short, descriptive name.
    ```
    git checkout -b new-branch-name
    ```
    The branch name should reflect the bug or feature it will resolve, or reference the issue number directly.

1. Make your edits.

1. Add & commit changes.
    ```
    git add filename.R
    git commit -m "Implement cool feature XX (Resolves #Y)"
    ```
    Use this [style guide](https://chris.beams.io/posts/git-commit/) for writing good commit messages.
    The highlights:
    - Capitalize the first word of the message.
    - The first word should be a verb in the imperative tense.
    - Keep the message short but descriptive of the changes.

    In the merge commit message, reference any [issues](https://github.com/SchlossLab/ML_pipeline_microbiome/issues)
    (our To-Do list) that the pull request resolves so the issue is closed automatically.
    For example, the [commit](https://github.com/SchlossLab/ML_pipeline_microbiome/commit/b7d0c295b71994e83cdd5aec6b26eb881523efdb)
    message I wrote when adding this file was `Add contributing instructions (Resolves #12)`.

1. Push your branch to GitHub.
    If you're pushing your branch for the first time, you'll have to set the upstream:
    ```
    git push --set-upstream origin new-branch-name
    ```

    Otherwise, just push like usual:
    ```
    git push origin new-branch-name
    ```

    If you forget the branch name:
    - Run `git status` to see the branch you currently have checked out (among other things).
    - You can list existing branches with `git branch --list`.

1. Open a pull request [[example](https://github.com/SchlossLab/ML_pipeline_microbiome/pull/1)].
    1. If you made multiple commits over a period of time, chances are high that your branch is behind the master branch. Follow these instructions to bring your local branch up to date with master on both your local and remote repository, run:
        ```
        git checkout master
        git pull
        git checkout <feature-branch>
        git merge master
        git push
        ```
    1. Open the [repo page](https://github.com/SchlossLab/ML_pipeline_microbiome) in your web browser.
    1. If you want to see what the modifications look like before opening a pull request, you can go to the document you
    modified and change the branch to the left of the file name.
    1. Go to the pull requests tab and click `new pull request`.
    1. Select your branch name to compare to master. If you forked the repo instead of making a branch, select `compare across forks` instead.
    1. Create the pull request.

    A maintainer will review your pull request and may ask you to make additional changes.
    If you have write access to the repo, **don't merge your branch into master**.
    The maintainer will merge it when they decide the branch is ready.

1. Continue to pull, commit, & push changes on your branch to update the open pull request as needed.

1. Once a pull request is merged, the maintainer may delete the branch if it will no longer be needed in the future.
