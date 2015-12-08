##' Plugins for Grokit
##'
##' These functions are used to modify the plugin infrastructure of Grokit.
##'
##' The plugin infrastructure of Grokit refers to the ability to alter the
##' query plan at run-time. The main feature of this system is that allows
##' waypoints to be changed based on other waypoints, as the whole exceution
##' plan has been formulated by the time plugins are run.
##'
##' Each plugin is specified by a label, a waypoint class, and a boolean.
##' This combination must be unique and an error is thrown during an attempted
##' registration that would result in a clash. Therefore, the label should
##' generally be generated at the time of library loading using to avoid name
##' conflict between unassociated plugins, using \code{\link{create.alias}}.
##'
##' The boolean governs when the plugin takes effect during the processing of
##' the query plan. During waypoint processing, the following occurs:
##'
##' 1. Compuation of additional information.
##' 2. Translation of parent waypoints.
##' 3. Transmutation of current waypoint.
##'
##' Note that translation is done recursively, so after these three steps, the
##' children waypoints are then translated. The boolean governs whether a plugin
##' is run during step 1 or step 3, with TRUE and FALSE corresponding to those
##' steps respectively. Step 1 plugins should return a list, which is added to
##' the 'extra' list with the name specified by the label given for that plugin.
##' The entirety of 'extra' is passed to the functions that translate the parent
##' waypoints during step 2. Lastly, step 3 plugins are run consecutively, each
##' of which should return a waypoint of the same class as the label. The output
##' of each plugin is used as the input to the next plugin, with the first input
##' being the original waypoint and the last output being the result of the
##' processing of this waypoint.

register.plugin <- function(label, class, prior, plugin) {
  assert(is.character(label) && length(label) == 1,
         "invalid label: ", label)

  classes <- c("Filter", "Generated", "GF", "GI", "GIST", "GLA", "Load", "Join")
  assert(is.character(class) && length(class) == 1 && class %in% classes,
         "invalid class: ", class)

  assert(is.logical(prior) && length(prior) == 1,
         "prior should be a length one logical.")
  prior <- if (prior) "before" else "after"

  assert(is.null(grokit$plugins[[class]][[prior]][[label]]),
         "Plugin clash, signature: ", paste(class, prior, label))
  grokit$plugins[[class]][[prior]][[label]] <- plugin
}

Process <- function(waypoint, info) {
  class <- class(waypoint)[[1]]
  plugins <-  grokit$plugins[[class]]
  waypoint$extra <- lapply(plugins[["before"]], do.call, list(waypoint, info))
  if (class == "Join") {
    waypoint$x <- Process(waypoint$x, waypoint$extra)
    waypoint$y <- Process(waypoint$y, waypoint$extra)
  } else if (!(class %in% c("Load", "GI", "GIST"))) {
    waypoint$data <- Process(waypoint$data, waypoint$extra)
  }
  if (class %in% c("GF", "GI", "GIST", "GLA"))
    waypoint$states <- lapply(waypoint$states, Process, waypoint$extra)
  for (plugin in plugins[["after"]])
    waypoint <- plugin(waypoint, info)
  waypoint
}

## These are unused function currently. Process is currently a standalone
## function, but it may be changed to an S3 method in the future if necessary.
Process.Load <- function(waypoint, info) {
  plugins <- grokit$plugins[["Load"]]
  waypoint$extra <- lapply(plugins[["before"]], do.call, list(waypoint, info))
  for (plugin in plugins[["after"]])
    waypoint <- plugin(waypoint, info)
}

Process.Join <- function(waypoint, info) {
  plugins <- grokit$plugins[["Join"]]
  waypoint$extra <- lapply(plugins[["before"]], do.call, list(waypoint, info))
  waypoint$x <- Process(waypoint$x, waypoint$extra)
  waypoint$y <- Process(waypoint$y, waypoint$extra)
  for (plugin in plugins[["after"]])
    waypoint <- plugin(waypoint, info)
}

Process.Filter <- function(waypoint, info) {
  plugins <- grokit$plugins[["Filter"]]
  waypoint$extra <- lapply(plugins[["before"]], do.call, list(waypoint, info))
  waypoint$data <- Process(waypoint$data, waypoint$extra)
  for (plugin in plugins[["after"]])
    waypoint <- plugin(waypoint, info)
}
