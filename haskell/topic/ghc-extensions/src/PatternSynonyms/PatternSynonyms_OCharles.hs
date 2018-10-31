module PatternSynonyms.PatternSynonyms_OCharles where
{-
24 Days of GHC Extensions: Pattern Synonyms

binding extensions

ViewPatterns enable viewing data through the result of function application
- enables keeping some of the data definition abstract
- require new syntax

PatternSynonyms : enable giving names to pattern matches
- code maintainence, new abstractions, pattern match values as if they are data definitions

------------------------------------------------------------------------------
Pattern Synonyms As Constants

pattern synonyms instead of magic constants

in foreign code, enumerations often typed as integer
example : SDL library

int SDL_SetRenderDrawBlendMode(SDL_Renderer* renderer,
                               SDL_BlendMode blendMode)

SDL_BlendMode is an enum - a runtime number - CInt

best practive : make ADT

data BlendMode
  = NoBlending | AlphaBlending | AdditiveBlending | ColourModulatedBlending

toBlendMode :: BlendMode -> CInt
toBlendMode NoBlending    = #{const SDL_BLENDMODE_NONE}
toBlendMode AlphaBlending = #{const SDL_BLENDMODE_BLEND}
toBlendMode _             = undefined

fromBlendMode :: CInt -> Maybe BlendMode
fromBlendMode 0 = Just NoBlending
fromBlendMode _ = ndefined

 #{const ...} from hsc2hs

cost : runtime conversion to/from

remove cost goes via pattern synonyms

pattern NoBlending    = #{const SDL_BLENDMODE_NONE}  :: CInt
pattern AlphaBlending = #{const SDL_BLENDMODE_BLEND} :: CInt
pattern ...

Tells GHC that pattern matching on NoBlending is expecting a number
equal to SDL_BLENDMODE_NONE constant

setUpBlendMode :: CInt -> IO ()
setUpBlendMode AlphaBlending = do
  putStrLn "Enabling Alpha Blending"
  activateAlphaBlendingForAllTextures
  activateRenderAlphaBlending

still pattern matching values that don’t semantically make sense a CInt
so :

newtype BlendMode     = MkBlendMode { unBlendMode :: CInt }

pattern NoBlending    = MkBlendMode #{const SDL_BLENDMODE_NONE}
pattern AlphaBlending = MkBlendMode #{const SDL_BLENDMODE_BLEND}

benefits of an ADT, but no runtime overhead
- do not export MkBlendMode constructor
- only export the pattern synonyms

------------------------------------------------------------------------------
Bidirectional Patterns : enable creating data

send correct integers to the SDL c library:

setRenderAlphaBlending :: Renderer -> IO ()
setRenderAlphaBlending r =
  sdlSetRenderDrawBlendMode r (unBlendMode AlphaBlending)

use AlphaBlending pattern synonym - a bidirectional pattern -
to construct a value of type BlendMode - the newtype.
Then unBlendMode to coerce newtype back to CInt

See Matthew Pickering working with “unfixed” versions of data types *TODO*

See icelandj to build IRC bot *TODO*

See Matt’s blog post) working with generic data *TODO*

See patterns-as-constants usage a really nice trick too (first
demonstrated to me by Edward Kmett in gl and later in sdl2). *TODO*

pattern synonyms compliment view patterns
- pattern synonyms to tidy up bindings
- view patterns to perform computations
-}
