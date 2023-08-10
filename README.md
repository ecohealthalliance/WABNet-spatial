
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Containerised R workflow template

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![container-workflow-template](https://github.com/ecohealthalliance/container-template/actions/workflows/container-workflow-template.yml/badge.svg)](https://github.com/ecohealthalliance/container-template/actions/workflows/container-workflow-template.yml)
<!-- badges: end -->

This is a template repository of a containerised R workflow built on the
`targets` framework, made portable using `renv`, and ran manually or
automatically using `GitHub Actions`. To use this template click on the
“use this template button” and then select create a new repository.

Check out the
[`containerTemplateUtils`](https://github.com/ecohealthalliance/containerTemplateUtils)
package for handling common tasks related to this repo (sending emails,
uploading files to AWS, etc. )

Note that `git-crypt` is not part of the template repo. See the [EHA M&A
handbook](https://ecohealthalliance.github.io/eha-ma-handbook/16-encryption.html#set-up-encryption-for-a-repo-that-did-not-previously-use-git-crypt)
for how to add git-crypt.

Follow the links for more information about:

- [`targets`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#targets)
- [`renv`](https://ecohealthalliance.github.io/eha-ma-handbook/3-projects.html#package-management-with-renv)  
- [git-crypt](https://ecohealthalliance.github.io/eha-ma-handbook/16-encryption.html)
- [Reproducible
  workflows](https://github.com/ecohealthalliance/building-blocks-of-reproducibility)

Recommendations:  
- One function per file in R/  
- Non-function R scripts in another directory like `scripts/`  
- Use the same names for targets and function arguments for those
targets unless a function  
- Nouns for targets, verbs for functions  
- Use common suffixes for target types: `_file` for files, `_raw` for
read-in but unprocessed data  
- Use `fnmate` and `tflow` RStudio Add-Ins to make this easy, create
shortcuts for these add-ins
([talk](https://www.youtube.com/watch?v=jU1Zv21GvT4)), or the `usethis`
package

## Quick start

- Create repo from template
- rename .Rproj file
- streamline packages in `packages.R`
- modify `.gitattributes` to include any files that may need encryption
- initialize `git-crypt` for repo
- add relevant environment variables to `.env` file
- rename github actions workflows
