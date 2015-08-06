-- https://gist.github.com/ahammar/1341505

import Data.Generics
import Language.Haskell.Parser
import Language.Haskell.Pretty
import Language.Haskell.Syntax

main = do
    input <- getContents
    case parseModule input of
        ParseOk mod -> putStrLn $ prettyPrint $ everywhere (mkT desugarExp) mod
        ParseFailed loc msg -> failed loc msg

desugarExp (HsDo stmts) = desugarDo stmts
desugarExp other = other

desugarDo [HsQualifier exp] = exp
desugarDo (HsGenerator loc pat exp : stmts) = HsInfixApp exp bindOp body
  where body | canFail pat = bindPattern loc pat exp stmts
             | otherwise   = HsLambda loc [pat] (desugarDo stmts)
desugarDo (HsQualifier exp : stmts) =
    HsInfixApp exp thenOp $ desugarDo stmts
desugarDo (HsLetStmt decls : stmts) =
    HsLet decls $ desugarDo stmts
desugarDo _ = error "invalid do-block"

canFail (HsPVar _) = False
canFail (HsPParen pat) = canFail pat
canFail (HsPAsPat _ pat) = canFail pat
canFail HsPWildCard = False
canFail other = True

bindPattern loc pat exp stmts =
    HsLambda loc [HsPVar dummy] $
             HsCase (HsVar $ UnQual dummy)
                    [ HsAlt loc pat (HsUnGuardedAlt $ desugarDo stmts) []
                    , HsAlt loc HsPWildCard (HsUnGuardedAlt noMatch) []]
  where noMatch = HsApp (HsVar (UnQual $ HsIdent "fail"))
                        (HsLit (HsString "pattern match failure"))

dummy  = HsIdent "dummy"
bindOp = HsQVarOp $ UnQual $ HsSymbol ">>="
thenOp = HsQVarOp $ UnQual $ HsSymbol ">>"

failed loc msg = do
    print loc
    putStrLn msg

-- End of file.
