#' @importFrom git2r cred_ssh_key
get_cred <- function() {
  if (identical(.Platform$OS.type, "windows")) {
    cred <- cred_ssh_key(
      publickey = "~/../.ssh/id_rsa.pub",
      privatekey = "~/../.ssh/id_rsa"
    )
  } else {
    cred <- NULL
  }
  return(cred)
}

