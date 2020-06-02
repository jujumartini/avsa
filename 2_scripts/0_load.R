# Ctrl + Shift + F10   :   restart session
# Ctrl + Shift + Enter :   run whole script

# load packages
library(tidyverse)
library(vroom)
library(lubridate)
library(padr)
library(irr)
library(lme4)
# library(formattable)
library(reshape2)
library(ggrepel)
library(RColorBrewer)
# library(grid)
# library(gridSVG)
library(installr)

# remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern)

Q# show warnings for partial matches
options(warnPartialMatchDollar = TRUE)
options(warnPartialMatchArgs = TRUE)

# when condition in if (condition) has length > 1, throws error
Sys.setenv("_R_CHECK_LENGTH_1_CONDITION_" = "true")

tidyverse_conflicts()
