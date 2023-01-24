module Main where

import Prelude

import Data.Foldable (for_)
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Stats (Stats, isDirectory, isFile)
import Node.FS.Sync (readTextFile, readdir, stat)
import Node.Path (FilePath)
import Node.Process (argv)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types (Module(..), ModuleHeader(..), ModuleName(..), Name(..))
import Data.Array (drop) as Array
import Node.Path (concat, extname) as Path

main :: Effect Unit
main = do
  args <- argv
  for_ (args # Array.drop 2) \filename -> do
    filenames <- allFiles filename
    for_ filenames \pursFilePath -> do
        file <- readTextFile UTF8 pursFilePath
        case parseModule file of
          ParseSucceeded m -> log (getName m)
          ParseSucceededWithErrors _ _ -> pure unit
          ParseFailed _ -> pure unit
        pure unit

getName :: forall e7. Module e7 -> String
getName (Module {header: (ModuleHeader {name: (Name {name: (ModuleName name)})})}) = name

allFiles :: FilePath -> Effect (Array FilePath)
allFiles (path :: FilePath) = do
  (stats :: Stats) <- stat (path :: FilePath)
  if (stats # isDirectory)
  then do
    files <- readdir path
    let
        paths = do
            p <- files
            pure $ Path.concat [path, p]
    pps <- for paths allFiles
    pure do
        p <- pps
        p
  else if isFile stats && Path.extname path == ".purs"
  then pure [path]
  else pure []