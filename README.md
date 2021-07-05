![logo_final_6](https://user-images.githubusercontent.com/84511468/124519869-09562800-ddc1-11eb-86d8-14a836b0c8f8.png)

polyCID is an user friendly R shiny app based on preprocessed GBS data for detection and classification of putative contaminants in biparental polyploid populations, including apomictic clones, self-fertilization, half-siblings and/or full contaminants individuals. You can explore our simulated data to learn how to interact with the tool, as well as access, modify all datasets and adapt them to your research needs. By going through the workflow below, you will become familiar with all the steps involved in the analysis. Enjoy it!

![workflow](https://user-images.githubusercontent.com/84511468/123974273-e24bc080-d992-11eb-9b59-08a9f9ea3770.jpg)

## Citation

polyCID is provided under a free-of-charge, open-source license (A-GPL3). All we require is that you cite/attribute the following in any work that benefits from this code or application.

Martins and Moraes et al. (2021)
[An Automated SNP-Based Approach for Contaminant Identification in Biparental Polyploid Populations of Tropical Forage Grasses](https://www.biorxiv.org/content/10.1101/2021.07.01.450796v1.full).

## Launching polyCID Local Session

The polyCID shiny app must be launched from a R session on your local machine. After launching the back-end, an user interface will be openned on a R window, this window can be openned on your web browser too. 

### Quick install/launch instructions

Before installing polyCID, update your R to a version > 4.1 and Rstudio to the latest version. We strongly recomend to update all packages, it can be done in Rstudio (Tools > Check for Package Updates). Before launching polyCID, install the following required packages: 

```r
install.packages("shiny") 
install.packages("shinycssloaders")
install.packages("shinydashboard")
install.packages("devtools")
  devtools::install_github('emitanaka/anicon')
install.packages("BiocManager")
  BiocManager::install("pcaMethods")
install.packages('DT')
install.packages('NbClust')
install.packages("tidyverse")
```

After installing those packages, the app can be launched using the following command:

```r
shiny::runGitHub("polycid","lagmunicamp")
```
Other packages may be required and the launch will fail, a message with the missing package will be shown. In this case, install all the new required packages until the app is launched. 







