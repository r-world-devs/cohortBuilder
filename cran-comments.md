## Test environments
* local check
  Ubuntu 18.04.6 LTS, R 4.1.2 (2021-11-01)
* win-builder
  R version 4.1.3 (2022-03-10)
  R version 4.2.0 (2022-04-22 ucrt)
  R Under development (unstable) (2022-04-24 r82436 ucrt)

## `R CMD check cohortBuilder_0.1.tar.gz --as-cran` results

```
Status: 1 NOTE
```

Note regarding checking CRAN incoming feasibility.

## `devtools::check()` results

```
0 errors ✓ | 0 warnings ✓ | 0 notes ✓
```

## win-builder result

```
* using log directory 'd:/RCompile/CRANguest/R-oldrelease/cohortBuilder.Rcheck'
* using R version 4.1.3 (2022-03-10)
* using platform: x86_64-w64-mingw32 (64-bit)
* using session charset: ISO8859-1
...
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Krystian Igras <krystian8207@gmail.com>'

New submission

Possibly mis-spelled words in DESCRIPTION:
  shinyCohortBuilder (14:54)
...
Status: 1 NOTE
```

```
* using log directory 'd:/RCompile/CRANguest/R-release/cohortBuilder.Rcheck'
* using R version 4.2.0 (2022-04-22 ucrt)
* using platform: x86_64-w64-mingw32 (64-bit)
...
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Krystian Igras <krystian8207@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  backend (14:42)
  shinyCohortBuilder (14:54)
...
Status: 1 NOTE
```

```
* using log directory 'd:/RCompile/CRANguest/R-devel/cohortBuilder.Rcheck'
* using R Under development (unstable) (2022-05-30 r82436 ucrt)
* using platform: x86_64-w64-mingw32 (64-bit)
...
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Krystian Igras <krystian8207@gmail.com>'

New submission

Possibly misspelled words in DESCRIPTION:
  backend (14:42)
  shinyCohortBuilder (14:54)
...
Status: 1 NOTE
```

Notes mention usage of 'backend' and 'shinyCohortBuilder' words in the description. 
The words are correct and intentionally used.
