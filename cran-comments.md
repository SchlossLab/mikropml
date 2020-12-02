## Changes

This release fixes several problems found by CRAN's check server:

- test failure on r-patched-solaris-x86.
- check note about unused Imports on r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, and r-release-macos-x86_64.
- multiple test failtures with r-oldrel due to `stringsAsFactors` behavior.

## Test environments

- local macOS install, R 4.0.3
- win-builder, R devel
- rhub debian-clang-devel
- macOS 10.15.7 (on github-actions), R 4.0.3, devel, and oldrel
- ubuntu 18.04.5 (on github-actions), R devel
- windows 10.0.17763 (on github-actions), R 4.0.3 and devel

## R CMD check results

0 errors | 0 warnings | 0 notes

## revdepcheck results

No reverse dependencies found.
