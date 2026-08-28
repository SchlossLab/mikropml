# How to cut a new release

Guidelines are based on the R Packages release chapter:
<https://r-pkgs.org/release.html>

1.  Decide on the new version based on [semantic versioning
    guidelines](https://semver.org/). Only one of the major, minor, or
    patch should be incremented by 1.

- if the new version contains only bug fixes and documentation
  improvements, bump the patch.
- if there are any new features (new functions, new arguments), bump the
  minor version.
- if there are any API-breaking changes (removed features, removed
  arguments, different default behavior), bump the major version.

1.  Edit the version in the `DESCRIPTION` file.
2.  Change the development header to the new version
    `# mikropml development version` –\> `# mikropml X.Y.Z`
3.  Run `devtools::check(remote = TRUE, manual = TRUE)`.
4.  Run `devtools::check_win_devel()`
5.  Fix all Errors and Warnings from `R CMD check`. Make every effort to
    eliminate Notes; any that can’t be eliminated should be listed in
    `cran-comments.md` with an explanation.
6.  Ensure `cran-comments.md` is up to date.
7.  After you’ve triple-checked everything, submit it to CRAN with
    `devtools::submit_cran()`
8.  After the package is accepted by CRAN, cut the release in GitHub
    with the same commit SHA.
