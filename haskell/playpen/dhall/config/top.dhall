let makeUser = ./makeUser.dhall

in let makeBuildUser = \(index : Natural) -> makeUser "build${Natural/show index}"

in let generate =
        https://prelude.dhall-lang.org/List/generate
            -- ... and optionally protect them with integrity checks
            sha256:77dbfc09daa00a7c6ba0c834863e8ee42e538af0f0600397a1c34923f11454b5

        -- 1st fallback
      ? https://raw.githubusercontent.com/dhall-lang/Prelude/302881a17491f3c72238975a6c3e7aab603b9a96/List/generate

        -- 2nd fallback
      ? /usr/local/share/dhall/Prelude/List/generate

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
