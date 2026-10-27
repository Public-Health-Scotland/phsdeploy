# Minimal phsshiny app for the phsdeploy checks. phsshiny is installed from
# GitHub, so this tests that Connect Cloud can install a PHS package from
# there, and that the PHS theme renders. `data.csv` is rewritten by the update
# check, so the page shows which data version is live.
library(shiny)
library(bslib)
library(phsshiny)

data <- read.csv("data.csv")

phs_publication(
  title = "Test application for phsdeploy",
  home_page = phs_page(
    title = "Check phsdeploy status",
    phs_section(
      phs_kpi(
        value = data$version,
        title = "Data version"
      ),
      phs_kpi(
        value = data$written,
        title = "Data written",
        format = "none"
      )
    )
  )
)
