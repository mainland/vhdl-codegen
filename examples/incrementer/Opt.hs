module Opt where

import Control.Monad ( when )
import System.Console.GetOpt
import System.Environment
import System.IO

data Variant = Serial | Parallel

data Config = Config { help      :: Bool
                     , variant   :: Variant
                     , output    :: Maybe FilePath
                     , tb_output :: Maybe FilePath
                     }

defaultOptions :: Config
defaultOptions = Config { help      = False
                        , variant   = Serial
                        , output    = Nothing
                        , tb_output = Nothing
                        }

options :: [OptDescr (Config -> Config)]
options =
    [ Option ['h'] ["help"]      (NoArg (\conf -> conf { help = True }))                        "show help"
    , Option ['s'] ["serial"]    (NoArg (\conf -> conf { variant = Serial }))                   "generate serial version"
    , Option ['p'] ["parallel"]  (NoArg (\conf -> conf { variant = Parallel }))                 "generate parallel version"
    , Option ['o'] ["output"]    (ReqArg (\path conf -> conf { output = Just path }) "FILE")    "output FILE"
    , Option ['t'] ["testbench"] (ReqArg (\path conf -> conf { tb_output = Just path }) "FILE") "testbench output FILE"
    ]

compilerOpts :: [String] -> IO (Config, [String])
compilerOpts argv = do
  progName <- getProgName
  let header = "Usage: " ++ progName ++ " [OPTION...]"
  case getOpt Permute options argv of
      (o_,n,[])  -> do let o = foldl (flip id) defaultOptions o_
                       when (help o) $
                         hPutStrLn stderr (usageInfo header options)
                       return (o,n)
      (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
