# Chemometrics-Web-Apps-Data_Handling_app
App to perform sample and variable preprocessing and general descriptive analysis

# Chemometrics-Web-Apps (LEAMS/UERJ)
![image](https://static.wixstatic.com/media/1f581c_8d6a8a367d8042509d4843bf27ddd8d4~mv2.jpg/v1/fill/w_979,h_396,al_c,q_85,enc_auto/1f581c_8d6a8a367d8042509d4843bf27ddd8d4~mv2.jpg)

This repository is the colection of all chemometrics apps developed by our group, with the help of the LEAMS team. You can contact us through [our website](https://www.leamsuerj.com/) or by the developers e-mails:

- Bernardo Cardeal (bernardocardeal@outlook.com)
- Licarion Pinto (licarion@gmail.com)
- Aderval Luna (adsluna@gmail.com)

## Data Handling: Chemometrics R-Web App part 1 ![image](https://img.shields.io/badge/Version-DH--1.2-blueviolet)
Manuscript link: https://www.sciencedirect.com/science/article/pii/S0169743922002076
- This app is currently available for online usage in [this link](https://leams-uerj-chemometrics.shinyapps.io/Data_Handling_app/)
- Complete change log can be seen inside the code
 - Please answer this [questionnaire](https://docs.google.com/forms/d/1js1Wyf81-8oG6u2w3F9lswMV3MUjX6srkBYWkkOj7Zw/edit?ts=633b6a1f) to help us improve the interface
- Features:
  - Descriptive analysis:
    - General data information
    - Per variable Analysis (generaly or by class)
    - Multivariate normality testing
    - Interactive Plots
      - Can be plotted generaly or by class (if provided)
    - Missing data analysis
    - Correlation Heatmap

  - Data Imputation:
    - Simple substituitions (mean, median and 0's)
    - Random-forest
    - K-Nearest-Neighbours
    - Single Value Decomposition (SVD)
    - PCA imputation (Nipals, Bayesian and Probabilistic)

  - Spectral Transformation:
    - Select region of interest
    - Smoothing and Derivative (Mooving avarage, simple derivatives and Savitzky-Golay)
    - Scatter Correction (SNV and MSC)
    - Baseline Correction (Offset, Polynomial, ALS, Low-Pass Fast FT filtering, Robust Baseline Estimation and IRLS)
    - Normalization (Maximum intensity and by Internal Standard)
    - Peak Alignment (Parametric time Warping)

  - Spectral Visualization
    - Custom spectral image 2D generation (view by class, classes means, select axis names and image definition...)
    - Spectral Interactive Plot

  - Variables Preprocessing
    - Simple Operations (mean centering, scaling, autoscaling, logarithmic transformation and frobenius normalization)
    - Normality inducing transformations (Box-Cox and Yeo-Jhonson)   
    
## Installation

It is necessary to have the R and RStudio previously installed in your computer.

### Packages and Dependencies Installation
- Search the GitHub directory for the .R file contaning the packages for the required app (the names of the files for dependencies and the app itself should be similar). For Data_Handling_app this file is called Data_handling_Packages.R

   > - Open the file
   > - Righ-Click the "raw" option
   > - Click "Save link as" and change the file extension from ".txt" to ".R"
   > - Open the file in RStudio and run all of it

### App Installation

#### From GitHub:
- Install the required packages as explained above, then:

   > - Open the file
   > - Righ-Click the "raw" option
   > - Click "Save link as" and change the file extension from ".txt" to ".R" 
   > - Open the downloaded file into RStudio then click "Run App" on the top righ corner of the file

#### From .txt file:
- Install the required packages as explained above, then:
- Open RStudio the go to:

  > - File
  > - New File
  > - Shiny Web App 
  > - Paste the code then click "Run App" on the top righ corner of the file
- Then erase all the file content and copy the code into the new file
- When running the app for the first time, an option to install the required packages should appear ate the top of the window if you did not install them previously.
- If you have problems with the encoding when running the app, go to: 
  
  > - File
  > - Save with encoding
  > - UTF-8

## Usage
After the instalation process the app usage consist in simple open the app in Rstudio enviroment and click the Run App button.
