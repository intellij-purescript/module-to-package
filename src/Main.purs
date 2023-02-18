module Main where

import Prelude

import Data.Array ((:))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (Stats, isDirectory, isFile)
import Node.Path (FilePath)
import Node.Process (argv)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Declaration(..), Export(..), Foreign(..), Ident(..), Labeled(..), Module(..), ModuleHeader(..), ModuleName(..), Name(..))
import Data.Array (catMaybes, drop, fold, fromFoldable, intercalate) as Array
import Node.Path (concat, extname) as Path
import Data.Tuple (snd) as Tuple
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import Data.Tuple (Tuple(..))
import Node.FS.Aff (readTextFile, readdir, stat)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import Control.Parallel (parTraverse)
import Yoga.JSON (writeJSON) as JSON
import Foreign.Object (Object)
import Foreign.Object (singleton) as Object

main :: Effect Unit
main = do
  args <- argv
  launchAff_ $ for_ (args # Array.drop 2) \filename -> do
    filenames <- allFiles filename
    modules <- filenames # parTraverse \pursFilePath -> do
      file <- readTextFile UTF8 pursFilePath
      pure case parseModule file of
        ParseSucceeded m -> Just $ toJson m
        ParseSucceededWithErrors _ _ -> Nothing
        ParseFailed _ -> Nothing
    let package = Array.fold modules
    log $ JSON.writeJSON package

extractExports :: forall e. Module e -> Array String
extractExports m = case getExports m of
  [] -> m
    # foldMapModule
        ( defaultMonoidalVisitor
            { onDecl = case _ of
                (DeclValue { name: (Name { name: (Ident name) }) }) -> [ name ]
                (DeclClass _ (Just (Tuple _ members))) -> (Array.fromFoldable members)
                  <#> unwrap
                  <#> _.label
                  <#> unwrap
                  <#> _.name
                  <#> unwrap
                  <#> \name -> name
                (DeclForeign _ _ (ForeignValue (Labeled { label: (Name { name: (Ident name) }) }))) ->
                  [ name ]
                _ -> mempty
            }
        )

  list -> list
    <#> case _ of
      ExportValue (Name { name: (Ident name) }) -> Just name
      _ -> Nothing
    # Array.catMaybes

toJson :: forall e. Module e -> Object (Array String)
toJson m = Object.singleton (getName m) (extractExports m)

getExports :: forall e. Module e -> Array (Export e)
getExports (Module { header: ModuleHeader { exports } }) = case exports of
  Nothing -> []
  Just list -> list
    # unwrap
    # _.value
    # unwrap
    # \{ head, tail } -> head : (tail <#> Tuple.snd)

getName :: forall e7. Module e7 -> String
getName (Module { header: (ModuleHeader { name: (Name { name: (ModuleName name) }) }) }) = name

allFiles :: FilePath -> Aff (Array FilePath)
allFiles (path :: FilePath) = do
  (stats :: Stats) <- stat (path :: FilePath)
  if (stats # isDirectory) then do
    files <- readdir path
    let
      paths = do
        p <- files
        pure $ Path.concat [ path, p ]
    for paths allFiles <#> join
  else if isFile stats && Path.extname path == ".purs" then pure [ path ]
  else pure []