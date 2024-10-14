.onLoad <- function(libname, pkgname) { # nolint

  op <- options()
  op.getRad <- list(
    getRad.key_prefix = "getRad_",
    getRad.user_agent = "R package getRad"
  )
  toset <- !(names(op.getRad) %in% names(op))
  if (any(toset)) options(op.getRad[toset])
  rlang::run_on_load()
  invisible()
}
rlang::on_load(rlang::local_use_cli(inline = T))


req_user_agent_getrad<-function(req){
  req_user_agent(req,string=getOption("getRad.user_agent"))
}
