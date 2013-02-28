future2.14
==========

The `future2.14` package is a convenience package for those stuck with R versions lower than 2.14 but want the functions introduced in 2.14.  If you are using a version of R that is older than 2.14 you would probably want to install the other 'future' packages as well to get the functions provided in those releases of R as well.
    
## Installation

Currently there isn't a release on [CRAN](http://cran.r-project.org/).

You can, however, download the [zip ball](https://github.com/Dasonk/future2.14/zipball/master) or [tar ball](https://github.com/Dasonk/future2.14/tarball/master), decompress and run `R CMD INSTALL` on it, or use the **devtools** package to install the development version:

```r
## Make sure your current packages are up to date
update.packages()
## devtools is required
library(devtools)
install_github("future2.14", "Dasonk")

## You probably should install the other future
## packages which provide the functions for newer version of R too
install_github("future2.15", "Dasonk")
install_github("future3.0", "Dasonk")
# The 'future' package loads the other packages so you don't need
# to remember to load all of them at once.
install_github("future")

```
