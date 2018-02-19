#' Results from 2002 NASCAR Season
#'
#' This is an example dataset from \cite{Hunter 2004} recording the results of
#' 36 car races in the 2002 NASCAR season in the United States. Each record is
#' an ordering of the drivers according to their finishing position.
#'
#' @format A matrix with 36 rows corresponding to the races and 87 columns
#' corresponding to the positions. Columns 1 to 43 contain the ID for the driver
#' that came first to last place respectively. Columns 44 to 87 are zero, since
#' only 43 out of 87 drivers participated in each race. The \code{"drivers"}
#' attribute contains the names of the 87 drivers.
#' @references
#' Hunter, D. R. (2004) MM algorithms for generalized Bradley-Terry models.
#' \emph{The Annals of Statistics}, \bold{32(1)}, 384--406.
#' @examples
#'
#' # convert orderings to rankings
#' nascar[1:2, 1:45]
#' R <- as.rankings(nascar, input = "ordering")
#' colnames(R) <- attr(nascar, "drivers")
#' R[1:3, 1:3, as.rankings = FALSE]
#' R[1:3]
#'
#' # fit model as in Hunter 2004, excluding drivers that only lose
#' keep <- seq_len(83)
#' R2 <- R[, keep]
#' mod <- PlackettLuce(R2, npseudo = 0)
#'
#' # show coefficients as in Table 2 of Hunter 2004
#' avRank <- apply(R, 2, function(x) mean(x[x > 0]))
#' coefs <- round(coef(mod)[order(avRank[keep])], 2)
#' head(coefs, 3)
#' tail(coefs, 3)
"nascar"