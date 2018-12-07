  \(user : Text)
->
  let    homeDirectory  = "/home/${user}"
  in let privateKeyFile = "${homeDirectory}/id_rsa"
  in let publicKeyFile  = "${privateKeyFile}.pub"
  in  { homeDirectory  = homeDirectory
      , privateKeyFile = privateKeyFile
      , publicKeyFile  = publicKeyFile
      } : ./User.dhall
