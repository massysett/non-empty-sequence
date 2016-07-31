module Main (main) where

import qualified Cartel

version :: [Word]
version = [0,2,0,2]

properties :: Cartel.Properties
properties = Cartel.Properties
  { Cartel.name = "non-empty-sequence"
  , Cartel.version = version
  , Cartel.cabalVersion = Just (1,10)
  , Cartel.buildType = Just Cartel.simple
  , Cartel.license = Just Cartel.bsd3
  , Cartel.licenseFile = "LICENSE"
  , Cartel.licenseFiles = []
  , Cartel.copyright = "Copyright (c) 2016 Omari Norman"
  , Cartel.author = "Omari Norman"
  , Cartel.maintainer = "omari@smileystation.com"
  , Cartel.stability = "Experimental"
  , Cartel.homepage = "http://www.github.com/massysett/non-empty-sequence"
  , Cartel.bugReports = "http://www.github.com/massysett/non-empty-sequence/issues"
  , Cartel.packageUrl = ""
  , Cartel.synopsis = "Non-empty sequence"
  , Cartel.description =
    [ "Please see README.md" ]
  , Cartel.category = "Data"
  , Cartel.testedWith = []
  , Cartel.dataFiles = []
  , Cartel.dataDir = ""
  , Cartel.extraSourceFiles = ["README.md"]
  , Cartel.extraDocFiles = []
  , Cartel.extraTmpFiles = []
  }

ghcOptions :: Cartel.HasBuildInfo a => a
ghcOptions = Cartel.ghcOptions
  [ "-Wall"
  ]

commonOptions :: Cartel.HasBuildInfo a => [a]
commonOptions
  = ghcOptions
  : Cartel.haskell2010
  : Cartel.hsSourceDirs ["lib"]
  : []

libraryDepends :: [Cartel.Package]
libraryDepends =
  [ Cartel.closedOpen "base" [4,8] [5]
  , Cartel.atLeast "containers" [0,5,6]
  , Cartel.atLeast "semigroups" [0,18,1]
  ]

library
  :: [Cartel.NonEmptyString]
  -- ^ List of library modules
  -> [Cartel.LibraryField]
library libModules
  = Cartel.buildDepends libraryDepends
  : Cartel.exposedModules libModules
  : commonOptions

github :: Cartel.Section
github = Cartel.githubHead "massysett" "non-empty-sequence"

sections :: [Cartel.Section]
sections =
  [ github
  ]

main :: IO ()
main = Cartel.defaultMain $ do
  libModules <- Cartel.modules "../non-empty-sequence/lib"
  return (properties, library libModules, sections)
