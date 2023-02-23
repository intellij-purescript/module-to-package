module Main where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array (fold, (:))
import Data.Foldable (for_)
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
import Yoga.JSON (writeJSON) as JSON
import Foreign.Object (singleton) as Object
import Node.Path (concat, extname) as Path
import Data.Tuple (snd) as Tuple

main :: Effect Unit
main = do
  args <- argv
  launchAff_ $ for_ (args # Array.drop 2) \filename -> do
    warn $ "reading file: " <> filename
    filenames <- allFiles filename
    modules <- filenames # parTraverse \pursFilePath -> do
      file <- readTextFile UTF8 pursFilePath
      pure case parseModule file of
        ParseSucceeded m -> Just $ toJson m
        ParseSucceededWithErrors _ _ -> Nothing
        ParseFailed _ -> Nothing
    let package = Array.fold modules
    log $ JSON.writeJSON package

extractExports :: forall e. Module e -> Array (Object String)
extractExports m = case getExports m of
  [] -> m
    # foldMapModule
        ( defaultMonoidalVisitor
            { onDecl = case _ of
                (DeclData { name: Name { name: (Proper name) } } (Just (Tuple _ (Separated { head, tail })))) ->
                  head : (tail <#> snd) <#> \(DataCtor { name: Name { name: (Proper constructor) } }) ->
                    Object.singleton "type" name
                      <> Object.singleton "import type" "data member"
                      <> Object.singleton "constructor" constructor

                (DeclNewtype _ _ (Name { name: (Proper name) }) _) ->
                  [ Object.singleton "type" name
                      <> Object.singleton "import type" "data member"
                      <> Object.singleton "constructor" name
                  ]
                (DeclFixity { operator: (FixityValue _ _ (Name { name: Operator name })) }) ->
                  [ Object.singleton "name" name
                      <> Object.singleton "import type" "operator"
                  ]
                (DeclValue { name: (Name { name: (Ident name) }) }) ->
                  [ Object.singleton "name" name
                      <> Object.singleton "import type" "value"
                  ]
                (DeclClass _ (Just (Tuple _ members))) -> (Array.fromFoldable members)
                  <#> unwrap
                  <#> _.label
                  <#> unwrap
                  <#> _.name
                  <#> unwrap
                  <#> \name -> Object.singleton "name" name
                    <> Object.singleton "import type" "value"
                (DeclForeign _ _ (ForeignValue (Labeled { label: (Name { name: (Ident name) }) }))) ->
                  [ Object.singleton "name" name
                      <> Object.singleton "import type" "value"
                  ]
                _ -> mempty
            }
        )

  list -> list
    <#> case _ of
      ExportValue (Name { name: (Ident name) }) ->
        [ Object.singleton "name" name
            <> Object.singleton "import type" "value"
        ]
      ExportOp (Name { name: (Operator name) }) ->
        [ Object.singleton "name" name
            <> Object.singleton "import type" "operator"
        ]
      ExportType
        (Name { name: Proper name })
        (Just (DataEnumerated (Wrapped { value: (Just  (Separated { head, tail })) }))) ->
        head : (tail <#> snd) <#> \(Name {name: Proper constructor}) ->
          Object.singleton "type" name
            <> Object.singleton "import type" "data member"
            <> Object.singleton "constructor" constructor

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