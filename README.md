
Links to data
-------------
[PISA AUS motivation csv Data](https://www.dropbox.com/s/2r5tvgw3wgrauzp/PISA12-AUS-motivation.csv?dl=0)

[PISA AUS motivation SPSS Data](https://www.dropbox.com/s/w6q1z2zo0h6bm5f/PISA12-AUS-motivation.sav?dl=0)


Introduction
-------------

* Introduction `dplyr` package 
* Introduction `sjPlot` package
* Introduction Exploratory Factor Analysis (EFA) 

### Package installation

```r
# for dplyr
install.packages("dplyr") 
# sjPlot
install.packages("sjPlot")
install.packages("sjmisc") # somes functions in `sjPlot` have been moved into the sjmisc-package
install.packages("devtools") 
library(devtools)
devtools::install_github("sjPlot/devel") #  install the latest development snapshot of `sjPlot`
install.packages(c("haven", "effects")) # used in `sjPlot` introduction
# for EFA
install,packages("psych") #if you haven't installed it.
```

Links to topics
--------------------
[dplyr Introduction](https://rawgit.com/JiesiGuo/IPPE_Rcourse/master/dplyr_introduction/dplyr_Jiesi.html)

[sjPlot Introduction](https://rawgit.com/JiesiGuo/IPPE_Rcourse/master/sjPlot_Introduction/sjPlot_Jiesi.html)

[EFA Introduction](https://rawgit.com/JiesiGuo/IPPE_Rcourse/master/EFA_introduction/EFA_Jiesi.html)

