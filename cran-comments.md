
## Test environments

* local macOS install: R 4.2.2
* GitHub Actions (ubuntu-16.04): devel, release, oldrel
* GitHub Actions (windows): release
* GitHub Actions (macOS): release
* win-builder: devel
* rhub: debian-gcc-devel-nold

## R CMD check results

0 errors | 0 warnings | 1 note

```
Found the following (possibly) invalid URLs:
  URL: https://anaconda.org/conda-forge/r-mikropml
    From: inst/doc/paper.html
          README.md
    Status: 400
    Message: Bad Request
  URL: https://doi.org/doi:10.1128/mBio.00434-20
    From: README.md
    Status: 503
    Message: Service Unavailable
  URL: https://journals.asm.org/doi/10.1128/mbio.00434-20
    From: man/otu_small.Rd
    Status: 503
    Message: Service Unavailable
```

This is a spurious note as these URLs work in my local browser, and the DOIs are
correct.

## revdepcheck results

No reverse dependencies found.
