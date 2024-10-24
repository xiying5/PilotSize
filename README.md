# PilotSize
PilotSize is a program designed to help researchers calculate sample sizes for pilot and feasibility trials. It serves as a companion application to the forthcoming article titled "Determining sample size for pilot and feasibility trials: A tutorial" in XX.

### Test PilotSize on the Web

A working version of the PilotSize application is available here. If this web version cannot be accessed, you can install and run the app locally on your computer by following the instructions below.

### Run PilotSize in R

Before PilotSize will run on your computer, you will need to make sure you have R installed, and that you have installed the `shiny`, `pwr`, `presize`, and `statpsych` packages:

```r
install.packages(c('shiny', 'pwr', 'presize', 'statpsych'))

```

The quickest way to run the app natively on your computer is to open R, load Shiny, and run from the GitHub repository. You must have the `devtools` package installed (`install.packages('devtools')`).

```r
library(shiny)
shiny::runGitHub(repo = "PilotSize", username = "xiying5")
```

If you prefer to download the repository manually, you can download the ZIP file from GitHub, navigate to the folder containing the `app.R`, and run the app using:

```r
library(shiny)
shiny::runApp('path/to/your/app')
```
