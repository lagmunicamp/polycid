# polyCID

polyCID is an user friendly R shiny app based on preprocessed GBS data for detect and classify putative contaminants in biparental polyploid population, including apomictic clones, self-fertilization, half-siblings and/or full contaminants individuals. You can explore our simulated data to learn how to interact with the tool, as well as access, modify all datasets and adapt them to your research needs. By going through the workflow below, you will become familiar with all the steps involved in the analysis. Enjoy it!

![workflow](https://user-images.githubusercontent.com/84511468/123974273-e24bc080-d992-11eb-9b59-08a9f9ea3770.jpg)

## Citation

polyCID is provided under a free-of-charge, open-source license (A-GPL3). All we require is that you cite/attribute the following in any work that benefits from this code or application.

Martins and Moraes et al. (2022)
[An Automated SNP-Based Approach for Contaminant Identification in Biparental Polyploid Populations of Tropical Forage Grasses](https://www.biorxiv.org/).

## Launching polyCID Local Session

The polyCID shiny app must be launched from a R session on your local machine. After launching the back-end, an user interface will be openned on a R window, this window can be openned on your web browser too. 

### Quick install/launch instructions

To install polyCID first you need to install some required packages: 

```r
install.packages("shiny") 
install.packages("shinycssloaders")
install.packages("shinydashboard")
install.packages("devtools")
  devtools::install_github('emitanaka/anicon')
install.packages("BiocManager")
  BiocManager::install("pcaMethods")
install.packages('DT')
install.packages('nbclust')
install.packages("tidyverse")
```

After installing those packages, the app can be obtained using the following command:

```r
shiny::runGitHub("polycid","lagmunicamp")
```
Other packages may be required or updated.







