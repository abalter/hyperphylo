library(R6)

CloneEnv <- R6Class("CloneEnv",
                    public = list(
                      a = NULL,
                      b = NULL,
                      v = 1,
                      initialize = function() {
                        self$a <- new.env(parent = emptyenv())
                        self$b <- new.env(parent = emptyenv())
                        self$a$x <- 1
                        self$b$x <- 1
                      }
                    ),
                    private = list(
                      deep_clone = function(name, value) {
                        # With x$clone(deep=TRUE) is called, the deep_clone gets invoked once for
                        # each field, with the name and value.
                        if (name == "a") {
                          # `a` is an environment, so use this quick way of copying
                          list2env(as.list.environment(value, all.names = TRUE),
                                   parent = emptyenv())
                        } else {
                          # For all other fields, just return the value
                          value
                        }
                      }
                    )
)