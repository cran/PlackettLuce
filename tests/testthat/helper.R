# salad data --------------------------------------------------------------

if (require("prefmod")) {

    ## analysis of salad data from Critchlow, D. E. & Fligner, M. A. (1991).
    ## salad is a data.frame of rankings for items A B C D
    ## 1 = most tart, 4 = least tart
    data("salad", package = "prefmod", envir = environment())

    ## convert to rankings object
    salad_rankings <- as.rankings(salad)
    ## create data frame of corresponding features
    ## (acetic and gluconic acid concentrations in salad dressings)
    features <- data.frame(salad = LETTERS[1:4],
                           acetic = c(0.5, 0.5, 1, 0),
                           gluconic = c(0, 10, 0, 10))
    ## convert rankings to long-form (explode rankings)
    salad_long_rankings <-
        data.frame(features[t(col(salad)),],
                   ranking = c(t(salad)),
                   chid = c(t(row(salad))))
    ## salad pairs (treat 3rd and 4th as unranked)
    salad_pairs <- as.matrix(salad)
    salad_pairs[salad_pairs %in% c(3, 4)] <- 0
    salad_pairs <- as.rankings(salad_pairs)
}
