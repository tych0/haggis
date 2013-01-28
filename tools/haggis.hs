module Main where

import Options.Applicative

import Text.Haggis

data BloghOpts = BloghOpts { input :: String
                           , output :: String
                           , config :: String
                           , verbose :: Bool
                           }

options :: Parser BloghOpts
options = BloghOpts
    <$> strOption
        ( long "input"
       <> metavar "INPUT"
       <> help "Input contents (directory) for your web site." )
    <*> strOption
        ( long "output"
       <> metavar "OUTPUT"
       <> help "Output directory for site results." )
    <*> strOption
        ( long "config"
       <> metavar "CONFIG"
       <> help "Location of your .blogh configuration file." )
    <*> switch
        ( long "verbose"
       <> help "Talk a lot about what's going on." )

run :: BloghOpts -> IO ()
run opts = buildSite (input opts) (output opts)

main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> options)
      ( fullDesc
     <> progDesc "Generate a haggis blog."
     <> header "haggis - a static site generator" )
