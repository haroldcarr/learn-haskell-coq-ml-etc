let makeUser = ./makeUser.dhall

in let makeBuildUser = \(index : Natural) -> makeUser "build${Natural/show index}"

-- import
in let User = ./User.dhall

in    [ -- inline config for special cases
        { homeDirectory  = "/home/jenkins"
        , privateKeyFile = "/etc/jenkins/id_rsa"
        , publicKeyFile  = "/etc/jenkins/id_rsa.pub"
        }
      , makeUser "alice"
      , makeUser "bob"
      ]
 -- ↓ # is list concat
    # ( -- add current $USER
        [ makeUser (env:USER as Text) ] -- "as Text" imports raw Text instead of Dhall expr
      )
