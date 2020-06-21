module Scripts.TrimDocs where

import Prelude
import Data.String (take, length) as String
import Data.Foldable (for_, fold)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Node.FS.Aff (readdir, readTextFile, writeTextFile)
import Node.Encoding (Encoding(UTF8))

foreign import data CheerioModule :: Type

foreign import data Cheerio :: Type

foreign import loadImpl :: EffectFn1 String CheerioModule

foreign import selectImpl :: EffectFn2 String CheerioModule Cheerio

foreign import setHtmlImpl :: EffectFn2 String Cheerio Unit

foreign import htmlImpl :: EffectFn1 CheerioModule String

main :: Effect Unit
main = do
  launchAff_ do
    files <- readdir "./generated-docs/"
    let
      withoutExtension f = String.take (String.length f - 5) f

      modules = map withoutExtension files
    for_ modules \m -> do
      let
        file = "./generated-docs/" <> m <> ".html"
      code <- readTextFile UTF8 file
      newHtml <-
        liftEffect do
          cheerio <- runEffectFn1 loadImpl code
          selection <- runEffectFn2 selectImpl ".col--aside ul" cheerio
          let
            showModule m' = "<li><a href=\"" <> m' <> ".html\">" <> m' <> "</a></li>"

            shownModules :: Array String
            shownModules = map showModule modules

            newList = "<ul>" <> fold shownModules <> "</ul>"
          runEffectFn2 setHtmlImpl newList selection
          runEffectFn1 htmlImpl cheerio
      writeTextFile UTF8 file newHtml
