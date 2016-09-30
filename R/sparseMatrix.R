as.sparse.matrix <- function(M) {
  
  n <- which(M != 0)
  j <- ceiling(n/nrow(M))
  i <- 1 + (n-1) %% nrow(M)
  
  if (!is.null(colnames(M))) j <- colnames(M)[j]
  if (!is.null(rownames(M))) j <- rownames(M)[i]
  
  out <- list(row = i, col = j, value = M[n], dim = dim(M))
  class(out) <- "sparse.matrix"
  return(out)
  
}

"[.sparse.matrix" <- function(M, i, j, drop = FALSE) {
  if (missing(i)) i <- 1:M$dim[1]
  if (missing(j)) j <- 1:M$dim[2]
  select <- (M$row %in% i & M$col %in% j)
  M$row <- M$row[select]
  M$col <- M$col[select]
  M$value <- M$value[select]
  M$dim <- c(length(i), length(j))
  return(M)
  
}

as.matrix.sparse.matrix <- function(M) {
  out <- matrix(0, nrow = M$dim[1], ncol = M$dim[2])
  for (n in 1:length(M$row)) {
    out[M$row[n], M$col[n]] <- M$value[n]
  }
  return(out)
}

t.sparse.matrix <- function(M) {
  rows <- M$row
  cols <- M$col
  
  M$col <- rows
  M$row <- cols
  
  M$dim <- rev(M$dim)
  
  return(M)
  
}

prod.sparse.matrix <- function(M, N) {
  
  
  js <- intersect(M$col, N$row)
  
  lapply(js, function(j) {
    is <- which(M$col == j)
    ks <- which(N$row == j)
    list(row = M$row[is], col = N$col[ks], value = M$value[is]*M$value[ks])
  })
  
  
}