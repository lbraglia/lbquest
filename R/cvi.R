#' function to compute CVI indexes (both I-CVI and S-CVI) for content
#' validity evaluation of a questionnaire/scale
#' 
#' @param x a data.frame of dummy (or factor) with 0 (first level) as 
#' "not relevant" and 1 (second level) "as relevant"
#'
#' @param strict if TRUE, for I-CVI NA are treated like "not relevant"
#'
#' @references Polit, Beck, "The Content Validity Index: are you
#' sure you know whats' being reported? Critique and recommendations"
#' 
#' @export
cvi <- function(x, strict = TRUE){
    ## check data
    ok1 <- all(unlist(lapply(x, lbmisc::is.percentage)))
    ok2 <- all(unlist(lapply(x, function(y){
        is.factor(y) && (nlevels(y) == 2L)
    })))
    
    if (! (ok1 || ok2)) {
        msg <- "x must be a data.frame of dummies or two level factors"
        stop(msg)
    }

    ## uniform input as factor
    if (ok1) {
        x <- lapply(x, factor)
    }
    
    ## set levels
    x <- lapply(x, function(y) {
        levels(y) <- c("Not relevant", "Relevant")
        y
    })
        
    ## handle strict
    if (strict) {
        x <- lapply(x, function(y) {
            y[is.na(y)] <- "Not relevant"
            y
        })
    }

    ## stats
    not_missing <- unlist(lapply(x, function(y) sum(!is.na(y))))
    relevant <- unlist(lapply(x, function(y) sum(y == 'Relevant')))
    icvi <- (relevant / not_missing)
    tab <- data.frame('item' = names(x),
                      'not_missing' = not_missing,
                      'relevant' = relevant,
                      'i_cvi' = icvi)
    rownames(tab) <- NULL
    
    list("tab" = tab, 's_cvi' = mean(icvi))
}
