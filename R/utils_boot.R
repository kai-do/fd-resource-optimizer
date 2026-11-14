# Bootstrap resamples in chunks, each chunk with its own random number stream
# (L'Ecuyer-CMRG, as the parallel package uses). The chunks are fixed by
# n_boot alone, so the draws are the same whether the chunks run one after
# another or spread over any number of worker processes: n_cores changes the
# run time, not the result.
#
#   cl <- boot_cluster(6)
#   on.exit(boot_stop(cl))
#   chunks <- run_boot_chunks(draws_fn, n_boot = 1000, cl)

# Chunks per bootstrap: enough to keep 12 workers busy
boot_n_chunks <- 24L

# Draws in each chunk: n_boot split as evenly as possible
boot_chunks <- function(n_boot, n_chunks = boot_n_chunks) {
  n_chunks <- min(n_chunks, n_boot)
  sizes <- rep(n_boot %/% n_chunks, n_chunks)
  extra <- n_boot %% n_chunks
  sizes[seq_len(extra)] <- sizes[seq_len(extra)] + 1L
  sizes
}

# One random number stream per chunk, seeded by one draw from the caller's
# stream (so set.seed() before still makes the result repeatable). The
# caller's random number generator is left as it was after that one draw.
boot_streams <- function(n) {
  seed <- sample.int(.Machine$integer.max, 1)
  caller <- get(".Random.seed", envir = globalenv())
  on.exit(assign(".Random.seed", caller, envir = globalenv()))

  set.seed(seed, kind = "L'Ecuyer-CMRG")
  stream <- get(".Random.seed", envir = globalenv())
  streams <- vector("list", n)
  for (i in seq_len(n)) {
    streams[[i]] <- stream
    stream <- parallel::nextRNGStream(stream)
  }
  streams
}

# `fn(size)` run on its own stream; the random number generator is put back
# afterwards
boot_chunk <- function(fn, size, stream) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv())
  on.exit(
    if (had) assign(".Random.seed", old, envir = globalenv()) else rm(".Random.seed", envir = globalenv())
  )
  assign(".Random.seed", stream, envir = globalenv())
  fn(size)
}

# Run `fn(size)`, a function making `size` resamples, over every chunk; a list
# of its results in chunk order. With a cluster from boot_cluster(), `fn` is
# sent to the workers once and the chunks are shared out among them. Keep
# `fn`'s environment small (only what a draw needs): it is copied to each
# worker.
run_boot_chunks <- function(fn, n_boot, cl = NULL) {
  sizes <- boot_chunks(n_boot)
  streams <- boot_streams(length(sizes))
  if (is.null(cl)) {
    return(Map(function(size, stream) boot_chunk(fn, size, stream), sizes, streams))
  }
  parallel::clusterCall(cl, boot_set_fn, fn)
  parallel::clusterMap(
    cl,
    function(size, stream) boot_chunk(get(".fdresopt_boot_fn", envir = globalenv()), size, stream),
    sizes, streams,
    SIMPLIFY = FALSE,
    .scheduling = "dynamic"
  )
}

boot_set_fn <- function(fn) {
  assign(".fdresopt_boot_fn", fn, envir = globalenv())
  NULL
}

# Worker processes for the bootstrap, each with this package loaded; NULL for
# n_cores of 1 or less (run in this process). Starting them takes ~10 seconds.
boot_cluster <- function(n_cores) {
  if (is.null(n_cores) || n_cores <= 1) {
    return(NULL)
  }
  check_n_cores(n_cores)
  pkg <- environmentName(topenv())
  path <- getNamespaceInfo(pkg, "path")
  dev <- isNamespaceLoaded("pkgload") && pkgload::is_dev_package(pkg)

  cl <- parallel::makePSOCKcluster(n_cores)
  # runs before the package is loaded on the worker, so it can't be a
  # function of this package
  load_pkg <- function(pkg, path, dev) {
    if (dev) {
      suppressMessages(pkgload::load_all(path, quiet = TRUE, helpers = FALSE, attach_testthat = FALSE))
    } else {
      loadNamespace(pkg)
    }
    NULL
  }
  environment(load_pkg) <- globalenv()
  tryCatch(
    parallel::clusterCall(cl, load_pkg, pkg, path, dev),
    error = function(e) {
      parallel::stopCluster(cl)
      stop(e)
    }
  )
  cl
}

boot_stop <- function(cl) {
  if (!is.null(cl)) {
    parallel::stopCluster(cl)
  }
}

check_n_cores <- function(n_cores) {
  if (!is.null(n_cores) && (!is.numeric(n_cores) || length(n_cores) != 1 || is.na(n_cores) || n_cores < 1)) {
    stop("n_cores must be NULL or one number, 1 or more", call. = FALSE)
  }
}
