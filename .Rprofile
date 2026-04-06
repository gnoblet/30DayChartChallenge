source("renv/activate.R")

options(
  repos = c(
    CRAN = sprintf(
      "https://p3m.dev/cran/latest/bin/linux/manylinux_2_28-%s/%s",
      R.version["arch"],
      substr(getRversion(), 1, 3)
    )
  )
)

options(renv.pak.enabled = TRUE)
