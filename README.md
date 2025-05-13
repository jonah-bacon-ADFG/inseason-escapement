# inseason-escapement
This repo visualizes current in-season escapements for salmon stocks/districts in Lower Cook Inlet and compares those in-season escapements to historical run timing curves.

The "input" folder contains .csv files containing the necessary data for the Shiny App.

The "output" folder contains .csv files produced by cleaning the input data, but is not necessary for the running of the Shiny App.

The "code" folder contains 3 important files:
"helpers.R" - Takes input files, cleans them, and produces dataframes necessary for the "server.R" file.
"ui.R" - Produces the user interface for the Shiny App
"server.R" - Contains the function that produces the actual Shiny App. This file has lines commented out at the bottom for producing the Shiny App.

The "shiny-app-code" is an alternative sub-directory that includes the content of the 3 files ("helpers.R","ui.R","server.R") in one file ("app.R") aimed at publishing the Shiny App to a web browser.

# Instructions to deploy

0. Ensure that shiny app structure is correct and app runs locally
1. Log in to [shinyapps.io](https://www.shinyapps.io/) and determine your account
   name, token and secret key as described in [here](https://shiny.posit.co/r/articles/share/shinyapps/)
2. Open an R terminal, set working directory to `inseason-escapement/` (project root)
3. Execute the following command in R terminal. It may take a few minutes.
```
rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
rsconnect::deployApp()
```
