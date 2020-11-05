# The REMIND R package

R package **remind**, version **36.176.0**

  

## Purpose and Functionality

Contains the REMIND-specific routines for data and model output manipulation.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("remind")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Tutorial

The package comes with a vignette describing the basic functionality of the package and how to use it. You can load it with the following command (the package needs to be installed):

```r
vignette("remind_summary") # Adding plots to the REMIND_summary.pdf
```

## Questions / Problems

In case of questions / problems please contact Anastasis Giannousakis <giannou@pik-potsdam.de>.

## Citation

To cite package **remind** in publications use:

Giannousakis A, Pehl M (2020). _remind: The REMIND R package_. R
package version 36.176.0.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {remind: The REMIND R package},
  author = {Anastasis Giannousakis and Michaja Pehl},
  year = {2020},
  note = {R package version 36.176.0},
}
```

