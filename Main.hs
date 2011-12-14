module Main where
import qualified Server as S
import qualified Client as C
import System.Console.GetOpt
import System.IO
import System.Environment
--import System


--From the documentation on System.Console.GetOpt
data Flag = ServerMode | ClientMode | Input String | Stdin | Host String deriving (Show,Eq)

options :: [OptDescr Flag]
options =
  [ Option ['s'] ["server"] (NoArg ServerMode) "Run in server mode"
  , Option ['c'] ["client"] (NoArg ClientMode) "Run in client mode"
  , Option ['i'] ["input"] (ReqArg Input "MESSAGE") "Explicit message to send"
  , Option ['n'] ["stdin"] (NoArg Stdin) "Transmit from standard in"
  , Option ['h'] ["host"] (ReqArg Host "HOSTNAME") "Host to contact"
  ]

serverOpts :: [String] -> IO ([Flag], [String])
serverOpts argv = 
      case getOpt Permute options argv of
          (o,n,[] ) -> return (o,n)
          (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
      where header = "Usage Main [OPTION...]"
----

main = do
        args <- getArgs
        (opts,n) <- serverOpts args
        if elem ClientMode opts
          then clientmode opts
          else if elem ServerMode opts 
                 then S.main
                 else error "You must use client mode or server mode."
        return ()

clientmode flags = do
                     case pluckInput flags of
                            Just (Input s) -> host s 
                            Nothing        -> if elem Stdin flags
                                                then do
                                                       sn <- hGetContents stdin
                                                       host sn 
                                                else error "Client requires a specified input type."
    where
      host msg = case pluckhost flags of
                    Nothing       -> error "You must supply a host address."
                    Just (Host s) -> C.main s msg
      pluckInput [] = Nothing
      pluckInput ((Input s):xs) = Just $ Input s
      pluckInput (_:xs) = pluckInput xs

      pluckhost [] = Nothing
      pluckhost ((Host s):xs) = Just $ Host s
      pluckhost (_:xs) = pluckhost xs
