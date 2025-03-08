# PilotSize

**PilotSize** is a program developed to help researchers calculate sample sizes for pilot trials. It is designed as a companion tool to the forthcoming article, *"Determining Sample Size for Pilot Trials: A Tutorial,"* in XX.

### Execute Calculations Using Code

To quickly perform calculations, you can download R, Stata, or SAS code, copy the code, and paste it into the appropriate software to run.

### Run the App on the Web

You can perform calculations via an RShiny app available [here]. If this web version is inaccessible, follow the instructions below to install and run the app locally on your computer.

### Run the App on Your Computer

To run the app locally, ensure you have R installed and the following R packages: `shiny`, `pwr`, `presize`, `statpsych`, and `lrstat`.

```r
install.packages(c('shiny', 'pwr', 'presize', 'statpsych', 'lrstat'))
```

Then, open R and run the app from the GitHub repository (you will need the `devtools` package, which you can install using `install.packages('devtools')`):

```r
library(shiny)
shiny::runGitHub(repo = "PilotSize", username = "xiying5")
```

Alternatively, you can manually download the repository as a ZIP file from GitHub. After extracting, navigate to the folder containing `app.R` and run the app with:

```r
library(shiny)
shiny::runApp('path/to/your/app')
```
