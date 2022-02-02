This patch fixes a test failure on the no long doubles platform.

## Test environments

* GitHub Actions (ubuntu-16.04): devel, release, oldrel
* GitHub Actions (windows): release
* GitHub Actions (macOS): release
* win-builder: devel
* rhub: debian-gcc-devel-nold

## R CMD check results

0 errors | 0 warnings | 1 note

```
  URL: https://anaconda.org/conda-forge/r-mikropml
    From: inst/doc/paper.html
          README.md
    Status: 400
    Message: Bad Request
```

This is a spurious note as the URL works in my local browser.

## revdepcheck results

No reverse dependencies found.
