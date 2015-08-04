{-# LANGUAGE TemplateHaskell #-}
-- | In this module there is a helper function called 'wsass''.  You need to use
-- it to define another function where the first argument is the path to be used
-- to resolve @include, like this:
--
-- >>> let wsass = wass' "sass_include/"
--
-- Please, check the install instructions of hlibsass at
-- <https://github.com/jakubfijalkowski/hlibsass>
--
module Text.Sass.QQ
    ( sass
    , wsass'
    ) where

import Prelude hiding (exp)
import Text.Sass
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.Default (def)
import Text.Lucius (toCss)
import qualified Data.Text as T
import Yesod.Core.Widget (toWidget, CssBuilder(..))

data Ctx = Pat | Type | Dec

qq :: String -> (String -> Q Exp) -> QuasiQuoter
qq name exp = QuasiQuoter { quoteExp = exp
                          , quotePat = error $ errorString Pat
                          , quoteType = error $ errorString Type
                          , quoteDec = error $ errorString Dec
                          }
    where
        errorString ctx = 
            "You can't use the " ++ name ++ " quasiquoter in " ++ ctxName ctx

        ctxName Pat  = "a pattern"
        ctxName Type = "a type"
        ctxName Dec  = "a declaration"





-- | Quasiquoter that generate a string from Sass code
sass :: QuasiQuoter
sass = qq "sass" exp
    where
        exp :: String -> Q Exp
        exp str = do
            css <- runIO $ compileSass Nothing str
            [| $(stringE css) |]

-- | Quasiquoter that generate a yesod widget from Sass code
--
wsass' :: FilePath    -- ^ path of the sass files to use with @include
       -> QuasiQuoter
wsass' include = qq "wsass" exp
    where
        exp :: String -> Q Exp
        exp str = do
            css <- runIO $ compileSass (Just [include]) str
            [| toWidget $ CssBuilder $(stringE css) |]


compileSass :: Maybe [FilePath] -> String -> IO String
compileSass include str = do
    result <- compileString str (def { sassIncludePaths = include } )
    case result of
        Left err -> do
            err' <- errorMessage err
            error err'
        Right compiled -> return . resultString $ compiled
