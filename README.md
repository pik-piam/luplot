# Landuse Plot Library

R package **luplot**, version **3.51.2**

[![CRAN status](https://www.r-pkg.org/badges/version/luplot)](https://cran.r-project.org/package=luplot)     ![r-universe](https://pik-piam.r-universe.dev/badges/luplot)

## Purpose and Functionality

Some useful functions to plot data such as a map plot function for
    MAgPIE objects.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("luplot")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Questions / Problems

In case of questions / problems please contact Benjamin Bodirsky <bodirsky@pik-potsdam.de>.

## Citation

To cite package **luplot** in publications use:

Bodirsky B, Dietrich J, Krause M, Stevanovic M, Humpenoeder F, Weindl I, Baumstark L, Klein D, Rolinski S, Wang X (2021). _luplot: Landuse Plot Library_. R package version 3.51.2.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {luplot: Landuse Plot Library},
  author = {Benjamin Leon Bodirsky and Jan Philipp Dietrich and Michael Krause and Miodrag Stevanovic and Florian Humpenoeder and Isabelle Weindl and Lavinia Baumstark and David Klein and Susanne Rolinski and Xiaoxi Wang},
  year = {2021},
  note = {R package version 3.51.2},
}
```

