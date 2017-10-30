# insetmap

Prepare inset maps using simple wrappers based on existing tools.


### Installation

You can install this package and some of its dependencies using the `devtools`
package with:

```
devtools::install_github("r-spatial/sf")
devtools::install_github("tidyverse/ggplot2")
devtools::install_github("odeleongt/insetmap")
```

Note that the development versions of `sf`, `ggplot2`, and `ggforce` are
required.


### Use

To prepare a map of the world showing a country or countries in a inset map use:

```
# Load the package
library(package = "insetmap")

# Prepare the map
inset_world(emphasize = c("Guatemala", "Costa Rica"))
```

Nothe that the returned value is a ggplot object, so you can add additional data
or change the appearance by adding ggplot calls:

```
# Load ggplot2 package
library(package = "ggplot2")

# Modify the produced plot
inset_world(emphasize = c("Guatemala", "Costa Rica")) +
  scale_fill_manual(values = c("#ffaaaa", "blue"))
```



