## Test environments

- local macOS install; R 4.1.1
- win-builder; R release
- github-actions macOS-latest; R release
- github-actions ubuntu-latest; R devel, devel, and oldrel
- github-actions windows-latest; R release

## R CMD check results

0 errors | 0 warnings | 1 note

```
  URL: https://anaconda.org/conda-forge/r-mikropml
    From: inst/doc/paper.html
          README.md
    Status: 400
    Message: Bad Request
```

I believe this is a spurious note as the URL works in my local browser.

## revdepcheck results

No reverse dependencies found.
