## Resubmission

- Fixed possible invalid links in the README and vignettes. 
- Moved the packages that the datasets use from Suggests to Imports.
- Checked the package with rhub's debian-clang-devel image, 
which should closely match CRAN's r-devel-linux-x86_64-debian-gcc flavor.

## Test environments

- local macOS install, R 4.0.2
- macOS 10.15.7 (on github-actions), R 4.0.2 and devel
- ubuntu 18.04.5 (on github-actions), R devel
- windows 10.0.17763 (on github-actions), R 4.0.2 and devel
- win-builder, R devel
- rhub debian-clang-devel

## R CMD check results

0 errors | 0 warnings | 1 note

- New submission - this is a new release.
- Possibly mis-spelled words in DESCRIPTION:
    mikropml (46:30) - this is the name of the package.
