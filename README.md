# cbmbtools
General purpose package containing useful functions for microbiome analysis pipelines  

## Installation
First, install the devtools package (if it isn't already) and then load it.  
```
if (!require(devtools)) install.packages("devtools")
library(devtools)
```
Next, install and load cbmbtools.  
```
install_github("https://github.com/cb-42/cbmbtools")  
library(cbmbtools)
```
The functions, templates, and datasets contained within cbmbtools should now be accessible within your R environment.  

To update a previous installation to the latest version, use:
```
devtools::update_packages("cbmbtools")
```

Note that this package is in active development. If a newer version of cbmbtools is installed after the library has already been loaded in the current R session, you may need to restart your R session and then update/load the latest version.  


