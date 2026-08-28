# How to cut a new release

Guidelines are based on the R Packages book chapter on releases:
<https://r-pkgs.org/release.html>.
See the book for detailed instructions.

1. Decide on the new version based on [semantic versioning guidelines](https://semver.org/).
  Only one of the major, minor, or patch should be incremented by 1.
  - if the new version contains only bug fixes and documentation improvements, bump the patch.
  - if there are any new features (new functions, new arguments), bump the minor version.
  - if there are any API-breaking changes (removed features, removed arguments, different default behavior), bump the major version.
1. Edit the version in the `DESCRIPTION` file.
1. Change the development header to the new version
  > `# mikropml development version` --> `# mikropml X.Y.Z`
  (where `X.Y.Z` is the semver you chose in step 1)
1. Run `devtools::check(remote = TRUE, manual = TRUE)`.
1. Run `devtools::check_win_devel()`
1. Fix all Errors and Warnings from `R CMD check`.
   Make every effort to eliminate Notes; any that can't be eliminated should be
   listed in `cran-comments.md` with an explanation.
1. Check for reverse dependencies. See [instructions from the R Packages book](https://r-pkgs.org/release.html#sec-release-revdep-checks).
1. Ensure `cran-comments.md` is up to date.
1. Make sure all of the edits you've made have been committed to git.
1. After you've triple-checked **everything**, submit it to CRAN with `devtools::submit_cran()`.
   Check your email and confirm the submission.
1. After the package is accepted by CRAN, cut the [release on GitHub](https://github.com/SchlossLab/mikropml/releases/new):
  - Create a new tag with `vX.Y.Z` matching the version you submitted to CRAN, targeting the same commit SHA.
    See the `CRAN-SUBMISSION` file (created by `submit_cran()`) in your local working directory,
    and delete the file after you no longer need it.
  - Copy and paste the version notes from `NEWS.md` into the release notes field.
  - Set it as the latest release and click `Publish Release`.
1. After cutting the release, bump the development version:
  - Add a new "development version" header to the top of `NEWS.md`.
  - Update the version in `DESCRIPTION` to `X.Y.Z.9000` (i.e. append `.9000` to the version you just released.)
