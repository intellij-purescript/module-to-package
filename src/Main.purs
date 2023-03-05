module Main where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array (fold, (:))
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log, warn)
import Foreign.Object (Object)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, readdir, stat)
import Node.FS.Stats (Stats, isDirectory, isFile)
import Node.Path (FilePath)
import Node.Process (argv)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Traversal (defaultMonoidalVisitor, foldMapModule)
import PureScript.CST.Types (DataCtor(..), DataMembers(..), Declaration(..), Export(..), FixityOp(..), Foreign(..), Ident(..), Labeled(..), Module(..), ModuleHeader(..), ModuleName(..), Name(..), Operator(..), Proper(..), Separated(..), Wrapped(..))
import Data.Array (drop, fold, fromFoldable) as Array
import Yoga.JSON (writePrettyJSON) as JSON
import Foreign.Object (singleton) as Object
import Node.Path (concat, extname) as Path
import Data.Tuple (snd) as Tuple

main :: Effect Unit
main = do
  args <- argv
  launchAff_ do
    packages <- for (args # Array.drop 2) \package_name -> do
      warn $ "reading package: " <> package_name
      filenames <- allFiles $ "packages/" <> package_name <> "/src"
      modules <- filenames # parTraverse \file_path -> do
        file <- readTextFile UTF8 file_path
        pure case parseModule file of
          ParseSucceeded m -> Just $ toJson m
          ParseSucceededWithErrors _ _ -> Nothing
          ParseFailed _ -> Nothing
      pure $ Object.singleton package_name $ Array.fold modules
    log $ JSON.writePrettyJSON 2 $ Array.fold packages

extractExports :: forall e. Module e -> Array (Object String)
extractExports m = case getExports m of
  [] -> m
    # foldMapModule
        ( defaultMonoidalVisitor
            { onDecl = case _ of
                (DeclData { name: Name { name: (Proper name) } } (Just (Tuple _ (Separated { head, tail })))) ->
                  head : (tail <#> snd) <#> \(DataCtor { name: Name { name: (Proper constructor) } }) ->
                    dataMemberImport name constructor
                (DeclNewtype _ _ (Name { name: (Proper name) }) _) ->
                  [ dataMemberImport name name ]
                (DeclFixity { operator: (FixityValue _ _ (Name { name: Operator name })) }) ->
                  [ operatorImport name ]
                (DeclValue { name: (Name { name: (Ident name) }) }) ->
                  [ valueImport name ]
                (DeclClass _ (Just (Tuple _ members))) -> (Array.fromFoldable members)
                  <#> unwrap
                  <#> _.label
                  <#> unwrap
                  <#> _.name
                  <#> unwrap
                  <#> valueImport
                (DeclForeign _ _ (ForeignValue (Labeled { label: (Name { name: (Ident name) }) }))) ->
                  [ valueImport name ]
                _ -> mempty
            }
        )

  list -> list
    <#> case _ of
      ExportValue (Name { name: (Ident name) }) -> [ valueImport name ]
      ExportOp (Name { name: (Operator name) }) -> [ operatorImport name ]
      ExportType
        (Name { name: Proper type_ })
        (Just (DataEnumerated (Wrapped { value: (Just (Separated { head, tail })) }))) ->
        head : (tail <#> snd) <#> \(Name { name: Proper constructor }) ->
          dataMemberImport constructor type_

      _ -> []
    # fold

toJson :: forall e. Module e -> Object (Array (Object String))
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

valueImport name = Object.singleton "name" name
  <> Object.singleton "import type" "value"

operatorImport name = Object.singleton "name" name
  <> Object.singleton "import type" "operator"

dataMemberImport type_ constructor = Object.singleton "type" type_
  <> Object.singleton "import type" "data member"
  <> Object.singleton "constructor" constructor