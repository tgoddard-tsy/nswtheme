## How to install `nswtheme`
The package can be installed from GitHub using `devtools` or similar packages, which you may need to install first.

```{r installation, eval = FALSE}
#| warning: false
# install.packages('devtools')
devtools::install_github("tgoddard-tsy/nswtheme")

# Or remotes
remotes::install_github("tgoddard-tsy/nswtheme")

# Or pak
pak::pkg_install("tgoddard-tsy/nswtheme")
```

After installing the package, you can load it like any other package.

```{r load-packages}
#| warning: false
library(tidyverse)
library(lubridate)
library(nswtheme)
library(nswtools)
```

## Themes
The `nswtheme` package contains NSW Government themes for charts (`ggplot2`) and tables (`flextable`). The default themes are updated by running the `set_defaults_nsw()` function.

```{r set_defaults_nsw}
set_defaults_nsw()
```

Now, any compatible chart or table you create will be consistent with the NSW Government brand.

You can also specify your preferred `style` to suit your department's unique brand, if it has one. For example, "Treasury".

```{r set_defaults_nsw_tsy}
set_defaults_nsw("Treasury")
```
