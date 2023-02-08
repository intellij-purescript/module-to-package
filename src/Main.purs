module Main where

import Prelude

import Data.Array ((:))
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (Stats, isDirectory, isFile)
import Node.FS.Sync (readTextFile, readdir, stat)
import Node.Path (FilePath)
import Node.Process (argv)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Declaration(..), Export(..), Ident(..), Module(..), ModuleHeader(..), ModuleName(..), Name(..))
import Data.Array (catMaybes, drop, fromFoldable, intercalate) as Array
import Node.Path (concat, extname) as Path
import Data.Tuple (snd) as Tuple
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import Data.Tuple (Tuple(..))

main :: Effect Unit
main = do
  args <- argv
  for_ (args # Array.drop 2) \filename -> do
    filenames <- allFiles filename
    modules <- for filenames \pursFilePath -> do
      file <- readTextFile UTF8 pursFilePath
      pure case parseModule file of
        ParseSucceeded m -> Just $ toJson m
        ParseSucceededWithErrors _ _ -> Nothing
        ParseFailed _ -> Nothing
    log $
      "{" <> (Array.catMaybes modules # Array.intercalate ", ") <> "}"

toJson :: forall e. Module e -> String
toJson m = "\"" <> moduleName <> "\": [" <> exports <> "]"
  where
  moduleName = getName m
  exports = case getExports m of
    [] -> m # foldMapModule
      ( defaultMonoidalVisitor
          { onDecl = case _ of
              (DeclValue {name: (Name {name: (Ident name)})}) -> ["\"" <> name <> "\""]
              (DeclClass _ (Just (Tuple _ members))) -> (Array.fromFoldable members)
                <#> unwrap
                <#> _.label
                <#> unwrap
                <#> _.name
                <#> unwrap
                <#> \ name -> "\"" <> name <> "\""
              _ -> mempty
          }
      )
      # Array.intercalate ", "

    list -> list
      <#> case _ of
        ExportValue (Name { name: (Ident name) }) -> Just $ "\"" <> name <> "\""
        _ -> Nothing
      # Array.catMaybes
      # Array.intercalate ", "

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

allFiles :: FilePath -> Effect (Array FilePath)
allFiles (path :: FilePath) = do
  (stats :: Stats) <- stat (path :: FilePath)
  if (stats # isDirectory) then do
    files <- readdir path
    let
      paths = do
        p <- files
        pure $ Path.concat [ path, p ]
    pps <- for paths allFiles
    pure do
      p <- pps
      p
  else if isFile stats && Path.extname path == ".purs" then pure [ path ]
  else pure []