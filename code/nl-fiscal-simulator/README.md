# Nuevo León fiscal-formula simulator

Shiny app that recalculates how Nuevo León distributes federal revenue-sharing funds (*participaciones*) among its 51 municipalities under the state's Ley de Coordinación Hacendaria, and shows which municipalities gain or lose when the formula's weights change.

Interface in Spanish. Built in R (2023).

## Run it

```r
install.packages(c("tidyverse", "readxl", "shiny", "scales", "janitor", "sf",
                   "leaflet", "fuzzyjoin", "colorRamps", "bslib", "bsicons",
                   "thematic", "sass", "bootstrap"))
shiny::runApp("code/nl-fiscal-simulator")
```

## Inputs (in `data/`)

| File | Contents |
|---|---|
| `coeficientes_municipios.xlsx` | Formula coefficients by municipality (sheet `part_23` = 2023 distribution) |
| `participaciones_pmunicipio_anual.csv` / `.xlsx` | Revenue-sharing received per municipality, 2018–2022 |
| `poblacion_municipios_nl.xlsx` | Municipal population |
| `municipio_corregido.*` | Municipal boundaries (shapefile) |

<!-- TODO (Alejandro): add the original source for each file (e.g., Periódico Oficial del Estado, INEGI). -->

## Method

See the methodology note: [`assets/docs/simulator-methodology.pdf`](../../assets/docs/simulator-methodology.pdf) (Spanish).

## Changes from the original

Only the input paths were changed (files moved into `data/`). The logic is as written in 2023.
