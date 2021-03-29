## ----output, include = FALSE--------------------------------------------------
output <- if (requireNamespace("BiocStyle", quietly = TRUE)) {
  BiocStyle::html_document
} else if (requireNamespace("bookdown", quietly = TRUE)) {
  bookdown::html_document2
} else html_document

## ----setup, include = FALSE---------------------------------------------------
library(knitr)
# if render using rmarkdown, use output format to decide table format
table.format <- opts_knit$get("rmarkdown.pandoc.to")
if (!identical(table.format, "latex")) table.format <- "html"
opts_knit$set(knitr.table.format = table.format)
opts_chunk$set(message = FALSE)

## -----------------------------------------------------------------------------
library(prefmod)
head(salad, 4)

## -----------------------------------------------------------------------------
features <- data.frame(salad = LETTERS[1:4],
                       acetic = c(0.5, 0.5, 1, 0),
                       gluconic = c(0, 10, 0, 10))

## -----------------------------------------------------------------------------
library(PlackettLuce)
standardPL <- pladmm(salad, ~ salad, data = features, rho = 8)
summary(standardPL)

## -----------------------------------------------------------------------------
sum(exp(standardPL$x %*% coef(standardPL)))

## -----------------------------------------------------------------------------
standardPL_PlackettLuce <- PlackettLuce(salad, npseudo = 0)
summary(standardPL_PlackettLuce)

## -----------------------------------------------------------------------------
standardPL <- pladmm(salad, ~ salad, data = features, rho = 8, rtol = 1e-6)
summary(standardPL)

## -----------------------------------------------------------------------------
itempar(standardPL)

## -----------------------------------------------------------------------------
regressionPL <- pladmm(salad, ~ acetic + gluconic, data = features, rho = 8)
summary(regressionPL)

## -----------------------------------------------------------------------------
deviance(regressionPL) - deviance(standardPL)

## -----------------------------------------------------------------------------
features2 <- data.frame(salad = LETTERS[5:6],
                        acetic = c(0.5, 0),
                        gluconic = c(5, 5))

## -----------------------------------------------------------------------------
predict(regressionPL, features2)

## -----------------------------------------------------------------------------
fitted(regressionPL)

## -----------------------------------------------------------------------------
predict(regressionPL, features2, type = "itempar", log  = FALSE, ref = NULL)

## -----------------------------------------------------------------------------
predict(regressionPL, features2, type = "itempar", log  = FALSE, ref = NULL,
        se.fit = TRUE)

## -----------------------------------------------------------------------------
regressionPL$pi

## -----------------------------------------------------------------------------
regressionPL$tilde_pi

