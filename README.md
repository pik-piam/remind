# remind
Contains the REMIND-specific routines for data and model output manipulation.

## Testing

Due to software limitations on the PIK cluster, our R packages --- which
includes the package "remind", that post-processes our REMIND results
--- are currently not being installed under the condition of
successful testing each time a new commit happens (packages are being
tested but the installation happens regardless of the result).  This
means that if a user commits a version of **remind** that does not
work, then all REMIND runs happening at that moment will fail to report results, 
until the next working version is installed.  To avoid
this situation, until further notice, you are advised to run the tests
manually before committing a new version of this R package.  To do so,
use the `fulldata.gdx` from your most recent REMIND run (if you don't
have a very recent one, simply start one -- it's always good to keep up
with using the model) and the `old.gdx` distributed with this package.

Simply run the following command twice with the location of each GDX as argument:

```{r}
a <- convGDX2MIF("/where/the/first/GDX/is/fulldata.gdx")
a <- convGDX2MIF("/<repository>/inst/extdata/old.gdx")
## this might also work if the working directory is the repository main folder
a <- convGDX2MIF(system.file("extdata", "old.gdx", package = "remind"))
```

and proceed with `lucode::buildlibrary()` as usual.


As an alternative, if you want to get familiar with how testing works
in R, follow these steps:

- In `tests/testthat/test-convGDX2mif.R` add the location of the two GDX's. Example:
```{r}
my_gdxs <- c("/path/to/fulldata.gdx", "../../inst/extdata/old.gdx")

```
- In RStudio hit Ctr+Shift+T or run `devtools::test()` in the command
  line

If testing was successful proceed with `lucode::buildlibrary()` as
usual.
