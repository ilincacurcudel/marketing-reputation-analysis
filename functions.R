## Additional plotting functionality for a textmodel_lda object
plotTerms <- function(textmodel_lda, n = 10) {
  dt <- data.table::as.data.table(as.matrix(textmodel_lda$words))
  colnames(dt) <- rownames(textmodel_lda$phi)
  dt$overall <- rowSums(dt)
  dt$word <- colnames(textmodel_lda$phi)
  dt <- melt(dt, id.vars = c('word', "overall"))
  dt <- dt[order(-value), head(.SD, n), by = variable]
  ggplot(dt, aes(x = tidytext::reorder_within(word, value, variable), y = value, fill = variable )) +
    geom_col(aes(y = overall), show.legend = FALSE, fill = "grey", alpha = .8) +
    geom_col(show.legend = FALSE) + coord_flip() +
    tidytext::scale_x_reordered() +
    facet_wrap( ~ variable, scales = "free") +
    labs(x = "word", y = "frequency")
}




## Topic aggregation functionality for a textmodel_lda object
mergeTopics <- function(textmodel_lda, merging_list){
  if (length(merging_list) < 2) stop("The aggregation list should include at least two new topics.")
  if (is.null(names(merging_list)) | any(is.na(names(merging_list))) ) names(merging_list) <- paste0("theme", 1:length(merging_list))
  flag <- all(
    length(unlist(merging_list)) == textmodel_lda$k,
    length(unique(unlist(merging_list))) == textmodel_lda$k,
    all(unlist(merging_list) %in% seq_len(nrow(textmodel_lda$phi))),
    all(unlist(merging_list) %in% seq_len(ncol(textmodel_lda$theta)))
  )
  if (!flag) stop("Error in reading the aggregation list. Be sure to include all existing topics and to avoid duplicates.")
  phi <- matrix(nrow = length(merging_list), ncol = ncol(textmodel_lda$phi))
  colnames(phi) <- colnames(textmodel_lda$phi)
  theta <- matrix(ncol = length(merging_list), nrow = nrow(textmodel_lda$theta))
  words_init <- t(as.matrix(textmodel_lda$words))
  rownames(words_init) <- rownames(textmodel_lda$phi)
  words <- unname(phi)
  rownames(theta) <- rownames(textmodel_lda$theta)
  rownames(phi) <- colnames(theta) <- names(merging_list)
  for (i in seq_along(merging_list)) {
    # phi[i, ] <- colSums(textmodel_lda$phi[merging_list[[i]], ,drop = FALSE])
    words[i, ] <- colSums(words_init[merging_list[[i]], ,drop = FALSE])
    theta[, i] <- rowSums(textmodel_lda$theta[, merging_list[[i]],drop = FALSE])
  }
  phi[] <- words + textmodel_lda$beta
  sum_phi <- rowSums(words) + textmodel_lda$beta * ncol(words)
  for (i in 1:nrow(phi)) {
    phi[i, ] <- phi[i, ] / sum_phi[i]
  }

  words <- Matrix::Matrix(t(words), sparse = TRUE)
  flag2 <- all(
    identical(dim(textmodel_lda$words)[1], dim(words)[1]),
    sum(words) == sum(textmodel_lda$words),
    all.equal(sum(phi) / length(merging_list), sum(textmodel_lda$phi) / textmodel_lda$k),
    all.equal(sum(theta), sum(textmodel_lda$theta))
  )
  if (!flag2) stop("Internal error, aggregation did not go well.. Please refrain from using this function for now.")
  phi <- t(apply(phi, 1, function(x) x / sum(x)))
  modifyList(textmodel_lda, list(words = words, phi = phi, theta = theta, k = length(merging_list)))
}

### Visualization through LDAvis
#### Setting up a helper function
LDAvis_from_seededLDA <- function(x, out.dir = tempfile(), open.browser = interactive()) {
  json <- LDAvis::createJSON(phi = x$phi,
                             theta = x$theta,
                             doc.length = rowSums(x$data),
                             vocab = colnames(x$data),
                             term.frequency = colSums(x$data),
                             reorder.topics = FALSE)
  LDAvis::serVis(json, out.dir = out.dir, open.browser = open.browser, encoding = "UTF-8")
}


wordcloudTerms <- function(textmodel_lda, n = 10) {
  words <- t(as.matrix(textmodel_lda$words))
  colnames(words) <- colnames(textmodel_lda$phi)
  rownames(words) <- rownames(textmodel_lda$phi)
  suppressWarnings(quanteda.textplots::textplot_wordcloud( quanteda::as.dfm(words),
                                          max_words = 800,
                                          comparison = TRUE))
}



## Redefine this function instead of importing tidytext::reorder_within. (This
## avoid unnecessary dependencies)
reorder_within <- function (x, by, within, fun = mean, sep = "___", ...) 
{
  if (!is.list(within)) {
    within <- list(within)
  }
  new_x <- do.call(paste, c(list(x, sep = sep), within))
  stats::reorder(new_x, by, FUN = fun)
}
## Same for tidytext::scale_x_reordered
scale_x_reordered <- function (..., sep = "___") 
{
  reg <- paste0(sep, ".+$")
  ggplot2::scale_x_discrete(labels = function(x) gsub(reg, "", x), ...)
}