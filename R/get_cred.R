#' @importFrom git2r cred_ssh_key remote_url
get_cred <- function(repo) {
  if (!startsWith(remote_url(repo), "https") &
      identical(.Platform$OS.type, "windows")) {
    cred <- cred_ssh_key(
      publickey = "~/../.ssh/id_rsa.pub",
      privatekey = "~/../.ssh/id_rsa"
    )
  } else {
    cred <- NULL
  }
  return(cred)
}

