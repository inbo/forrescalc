#' @importFrom git2r cred_ssh_key remote_url
#' @importFrom fs path_home
get_cred <- function(repo) {
  if (!startsWith(remote_url(repo), "https")[1] &&
      identical(.Platform$OS.type, "windows")) {
    cred <- cred_ssh_key(
      publickey = path_home(".ssh", "id_rsa.pub"),
      privatekey = path_home(".ssh", "id_rsa")
    )
  } else {
    cred <- NULL
  }
  return(cred)
}
