<p align="center">
  <img width="350" src="https://user-images.githubusercontent.com/84511468/124522586-2f33fa80-ddca-11eb-8ac1-1b4126000038.png">
  
polyCID is a user-friendly R shiny app based on preprocessed GBS data for detection and classification of putative contaminants in biparental polyploid populations, including apomictic clones, self-fertilization, half-siblings and/or full contaminants individuals. You can explore our simulated data to learn how to interact with the tool, as well as access, modify all datasets and adapt them to your research needs. By going through the workflow below, you will become familiar with all the steps involved in the analysis. Enjoy it!

![workflow](https://user-images.githubusercontent.com/84511468/123974273-e24bc080-d992-11eb-9b59-08a9f9ea3770.jpg)
## Citation

polyCID is provided under a free-of-charge. All we require is that you cite/attribute the following in any work that benefits from this code or application.

Martins and Moraes et al. (2021)
[An Automated SNP-Based Approach for Contaminant Identification in Biparental Polyploid Populations of Tropical Forage Grasses](https://www.biorxiv.org/content/10.1101/2021.07.01.450796v1.full).

## Launching polyCID Local Session

The polyCID shiny app must be launched from a R session on your local machine. After launching the back-end, a user interface will be opened on a R window, this window can be openned on your web browser too. 

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

## polyCID app usage guide

After launching the app, your will be directed to the Project page, showing a presentation of the app and the workflow. In the menu, you can go to the Tool page, where the analysis is performed; or to the Info page, containing information about the paper and the authors.
![1](https://user-images.githubusercontent.com/84511468/124525323-3dd3df00-ddd5-11eb-80d0-0ebf10848c17.png)

In the Tool page, the analysis can be performed using the 8 tabs. In the tab Input Data, a dataset must be loaded. To get familiar with the app, you can run the analysis using a provided dataset, just click in Analyze.
![2](https://user-images.githubusercontent.com/84511468/124632573-da8c9000-de5a-11eb-9dd1-7262baf7a5ae.png)

In the Selecting Parents tab, it is necessary to define the progeny parents. Beyond that, it is possible to add artificial clones of the parents and use the next tab to analyze how the PCA scatterplot dispersion pattern changes with this insertion. Click in Select to run the PCA.
![3](https://user-images.githubusercontent.com/84511468/124526938-922d8d80-ddda-11eb-935f-c53eed702417.png)
  
The PCA tab shows the PCA scatterplot of the population, it can be redone by clicking Select again in the Selecting Parents tab. Clicking in the Yes! button makes the app run the GA and CA, it may take several minutes depending on the dataset size. A dataset with 238 samples and 1195 markers takes approximately 6 minutes, while a data set with 278 samples and 7253 markers takes approximately 46min in a modern CPU. 
![4](https://user-images.githubusercontent.com/84511468/124537577-8d73d400-ddf0-11eb-93b2-807d6dd16b42.png)

In the GA-I tab it is necessary to select one of the three most indicated clustering schemes to use in the analysis. It is possible to go through all further tabs analyzing if the clustering schemes fits the data, you always can come back to GA-I tab and change the clustering scheme. Furthermore, it is possible to change the threshold that defines if a sample is considered as a contaminant. For this dataset, the clustering scheme that correctly separate the samples is the second.
![5](https://user-images.githubusercontent.com/84511468/124638790-c4360280-de61-11eb-86ce-d5e6b1c887cc.png)
  
The Contaminant Identification tab shows a PCA scatterplot with the samples labeled as their type, and a final confirmation of the samples that are going to be removed.
![6](https://user-images.githubusercontent.com/84511468/124699494-4570b200-dec1-11eb-8d9e-3485cce34574.png)

Finally, in the Results tab there is a PCA scatterplot without the contaminants identified and it is possible to download the filtered dataset. Furthermore, the GA measures and the clustering results can be downloaded too. The GA results can be used in the Input Data tab to reduce the time of analysis if you want to run again. 
![7](https://user-images.githubusercontent.com/84511468/124779651-5567b080-df18-11eb-9e5a-968e97a60afa.png)








