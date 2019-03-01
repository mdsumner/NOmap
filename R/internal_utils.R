insert_into_sequence <- function(sequence, ins, after) {
    ## insert ins into sequence, so that it appears directly after the last element in after
    idx <- tail(which(sequence %in% after), 1)
    if (length(idx) < 1) {
        c(sequence, ins)
    } else {
        c(sequence[seq_len(idx)], ins, sequence[-seq_len(idx)])
    }
}
