insert_into_sequence <- function(sequence, ins, after) {
    ## insert ins into sequence, so that it appears directly after the last element in after
    idx <- tail(which(sequence %in% after), 1)
    if (length(idx) < 1) {
        c(sequence, ins)
    } else {
        c(sequence[seq_len(idx)], ins, sequence[-seq_len(idx)])
    }
}


## internal plotting routine, called by SOmap and SOmanagement
## iterate through the object's plot_sequence vector, running the plotfun with plotargs for each
plot_all <- function(x) {
    assert_that(inherits(x, c("SOmap_management", "SOmap")))
    ## interate through each plottable element in turn
    for (toplot in intersect(x$plot_sequence, names(x))) {
        allpf <- x[[toplot]] ## all the stuff to plot for this element
        if (is.list(allpf) && length(allpf) > 1 && (is.null(names(allpf)) || length(setdiff(names(allpf), c("", "labels"))) < 1)) {
            ## a list of plotfun/args to iterate over
            ## needs a more robust way of detecting this
        } else {
            allpf <- list(allpf)
        }
        for (thispf in allpf) {
            thisfun <- thispf$plotfun
            ##if (is.character(thisfun)) thisfun <- parse(text = thisfun)
            ##eval(thisfun, envir = x[[toplot]]$plotenv)
            this_plotargs <- thispf$plotargs
            if (is.character(thisfun)) do.call(eval(parse(text = thisfun)), this_plotargs) else do.call(thisfun, this_plotargs)
        }
        if (!is.null(thispf$labels)) {
            allpf <- thispf$labels ## all the stuff to plot for this element
            if (is.list(allpf) && length(allpf) > 1 && is.null(names(allpf))) {
                ## a list of plotfun/args to iterate over
            } else {
                allpf <- list(allpf)
            }
            for (thispf in allpf) {
                thisfun <- thispf$plotfun
                this_plotargs <- thispf$plotargs
                if (is.character(thisfun)) do.call(eval(parse(text = thisfun)), this_plotargs) else do.call(thisfun, this_plotargs)
            }
        }
    }
    invisible(NULL)
}
