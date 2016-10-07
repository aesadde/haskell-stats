{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module ModuleParser where

import           CabalParser (isLibrary, collectMostUsed, getPackageName, getPackageDeps, getPackageVersion, getAllFiles)
import           Control.Monad (liftM,forM_)
import           Control.Monad.Loops
import           Control.Monad.Reader (ask)
import           Control.Concurrent (forkIO)
import qualified Control.Monad.State as S
import           Data.Acid
import           Data.Char (isDigit,isLower)
import           Data.Data hiding (DataType)
import           Data.List (foldl', sortBy, partition)
import           Data.Ord (comparing)
import           Data.Map.Strict (Map)
import           GHC.Exts (Down(..))
import qualified Data.Map.Strict as M
import           Data.Maybe (fromMaybe,catMaybes)
import           Data.SafeCopy
import           Data.Serialize
import           Data.Serialize
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Typeable
import           Distribution.PackageDescription.Configuration (flattenPackageDescription)
import           Distribution.PackageDescription.Parse(readPackageDescription)
import           Distribution.Simple.Utils (tryFindPackageDesc)
import qualified Distribution.PackageDescription as Cabal (PackageDescription(..))
import           Distribution.Verbosity(silent)
import           GHC.Generics hiding (moduleName)
import qualified Language.Haskell.Exts as P
import           Language.Haskell.Exts.Extension (Language(..))
import qualified Language.Haskell.Exts.Extension as P
import qualified Language.Haskell.Exts.Parser as P
import qualified Language.Haskell.Exts.Pretty as P
import           Language.Haskell.Exts.Syntax
import qualified Language.Preprocessor.Cpphs as C
import qualified System.FilePath.Find as F
import           System.FilePath.Posix (splitFileName)
import           System.IO
import           Scraper (scrapeDownloads)
import qualified  Numeric.Statistics as N
import           Numeric.Extra (intToFloat)

-------------------------------------------------------------------------------
-- Parse Haskell Source Files -------------------------------------------------
-------------------------------------------------------------------------------

-- | Get the Language Pragmas from a Parsed Module file
extractPragmas :: Module -> [String]
extractPragmas (Module _ _ prgs _ _ _ _) = concatMap extract prgs
  where extract (LanguagePragma _ n) = ident <$> n
        ident (Ident x) = x

-- | Extract all declarations from a Module
extractDeclarations :: Module -> [Decl]
extractDeclarations (Module _ _ _ _ _ _ decl) = decl

-- | Extract all imports from a Module
extractImports :: Module -> [ImportDecl]
extractImports (Module _ _ _ _ _ imports _) = imports

modNames :: Module -> S.Set Text
modNames m = foldr (\i r -> S.insert (extractModName $ importModule i) r) S.empty $ extractImports m

-- | Unpack ModuleName only care about the String
extractModName :: ModuleName -> Text
extractModName (ModuleName x) = T.pack x

-- | Get The name of a Decl
getNames :: Decl -> [Text]
-- getNames (TypeDecl _ n _ _) = [printIdent n]
-- getNames (TypeFamDecl _ n _ _) = [printIdent n]
-- getNames (ClosedTypeFamDecl _ n _ _ _) = [printIdent n]
-- getNames (DataDecl _ _ _ n _ _ _) = [printIdent n]
-- getNames (GDataDecl _ _ _ n _ _ _ _) = [printIdent n]
-- getNames (DataFamDecl _ _ n _ _) = [printIdent n]
-- getNames (ClassDecl _ _ n _ _ _) = [printIdent n]
-- getNames (TypeSig _ n _) = map printIdent n
-- getNames (PatSynSig _ n _ _ _ _) = [printIdent n]
-- getNames (ForImp _ _ _ _ n _ ) = [printIdent n]
-- getNames (ForExp _ _ _ n _) = [printIdent n]
-- getNames (InstDecl _ _ _ _ qn _ _) = [extractQN qn]
-- getNames (DerivDecl _ _ _ _ qn  _) = [extractQN qn]
getNames (FunBind matches) = concatMap extractFunctions matches
getNames (PatBind _ _ rhs mbinds) = extractRHS rhs ++ extractBinds (fromMaybe (BDecls []) mbinds)
getNames _ = [""]

extractQN :: QName -> Text
extractQN (Qual _ n) = printIdent n
extractQN (UnQual n) = printIdent n
extractQN (Special sp) =  T.pack $ P.prettyPrint sp

printIdent (Ident s) = T.pack s
printIdent (Symbol s)= T.pack s

extractFunName :: Match -> String
extractFunName (Match _ n _ _ _ _) = P.prettyPrint n


-- | Extract only the TypeSignatures from all available Declarations
extractTypeSigs :: [Decl] -> [Decl]
extractTypeSigs xs = [x | x@TypeSig {} <- xs]

-- | Extract All the function texts from the declarations
--   Note: This works only if the function has at least one parameter.
--   If the function is eta-reduced it doesn't parse it
--   for functions (expressions) with no arguments use PatBind
extractFunBinds :: [Decl] -> [Decl]
extractFunBinds xs = [x | x@FunBind {} <- xs]

-- extractFun :: Decl -> (String, String, [String])
-- extractFun (FunBind (Match (SrcLoc src) n _ _ rhs _)) = (srcFilename src, P.prettyPrint n, extractRHS rhs)

extractFunctions :: Match -> [Text]
extractFunctions (Match _ n _ _ rhs _) = (T.pack $ P.prettyPrint n) : extractRHS rhs

extractRHS (UnGuardedRhs exp)          = extractExp exp
extractRHS (GuardedRhss xs)            = concatMap (\(GuardedRhs _ _ exp) -> extractExp exp) xs

extractQOP :: QOp -> Text
extractQOP (QVarOp qn)                 = extractQN qn
extractQOP (QConOp qn)                 = extractQN qn

extractStmt (Generator _ _ exp)        = extractExp exp
extractStmt (Qualifier exp)            = extractExp exp
extractStmt (RecStmt stmts)            = concatMap extractStmt stmts
extractStmt (LetStmt bind)             = extractBinds bind

extractBinds (BDecls decls)            = concatMap getNames decls
extractBinds _                         = []

extractAlts (Alt _ _ rhs mbinds)       = extractRHS rhs  ++ extractBinds (fromMaybe (BDecls []) mbinds)

extractExp :: Exp -> [Text]
extractExp (Var qn)                    = [extractQN qn]
extractExp (Con qn)                    = [extractQN qn]
extractExp (InfixApp exp qp ex)        = extractExp exp ++ [extractQOP qp] ++ extractExp ex
extractExp (App exp ex)                = extractExp exp ++ extractExp ex
extractExp (NegApp exp)                = extractExp exp
extractExp (Lambda _ _ exp)            = extractExp exp
extractExp (Let _ exp)                 = extractExp exp
extractExp (If a b c)                  = extractExp a ++ extractExp b ++ extractExp c
extractExp (Case exp  _)               = extractExp exp
extractExp (Tuple _ exps)              = concatMap extractExp exps
extractExp (TupleSection _ mexps)      = concatMap (extractExp . fromMaybe (Var (UnQual (Ident "")))) mexps
extractExp (List exps)                 = concatMap extractExp exps
extractExp (ParArray exps)             = concatMap extractExp exps
extractExp (Paren exp)                 = extractExp exp
extractExp (LeftSection exp qop)       = extractQOP qop : extractExp exp
extractExp (RightSection qop exp)      = extractQOP qop : extractExp exp
extractExp (MultiIf rhss)              = concatMap (\(GuardedRhs _ _ exp) -> extractExp exp) rhss
extractExp (Do stmts)                  = concatMap extractStmt stmts
extractExp (MDo stmts)                 = concatMap extractStmt stmts
extractExp (LCase alts)                = concatMap extractAlts alts
extractExp (ExpTypeSig _ exp _)        = extractExp exp
extractExp _                           = []

extractData :: Decl -> String
extractData (DataDecl _ DataType _ n _ _ _) = show n
extractData _ = " "

-------------------------------------------------------------------------------
-- Run the Parser -------------------------------------------------------------
-------------------------------------------------------------------------------

-- The PkgInfo Data Type ------------------------------------------------------
-- This is simply a record with all the info we store in the db and process
-- accordingly
data PkgInfo = PkgInfo
  { name          :: Text          -- name of package
  , pversion      :: Text          -- version of package
  , dependencies  :: S.Set Text    -- dependencies of package
  , imports       :: Map Text Int  -- all the imports in the package
  , mostImported  :: (Text,Int)    -- the most imported modules in the package
  , moduleCount   :: Int           -- how many modules are in this package (all .hs files)
  , names         :: [Text]        -- all the idents in the packages
  , allFiles      :: [Text]        -- all *.hs files of the package
  , category      :: Text          -- the category of the package (application,library, etc)
  , downloads     :: Int
  , library       :: Bool
  , executable    :: Bool
  } deriving (Eq,Ord,Typeable,SafeCopy,Generic)

instance Serialize PkgInfo

instance Show PkgInfo where
  show = ppInfo

defaultInfo = PkgInfo
  { name         = ""
  , pversion     = ""
  , dependencies = S.empty
  , imports      = M.empty
  , mostImported = (" ", 0)
  , moduleCount  = 0
  , names        = []
  , allFiles     = []
  , category     = ""
  , library      = False
  , executable   = False
  , downloads    = 0
  }

ppInfo :: PkgInfo -> String
ppInfo info = "Name: " ++ (show $ name info) ++ "\n"
              ++ "Version: " ++ (show $ pversion info) ++ "\n"
              ++ "Most Imported Module: " ++ (show $ mostImported info) ++ "\n"
              ++ "Number of Modules : " ++ (show $ moduleCount info) ++ "\n"
              ++ "Category: " ++ (show $ category info) ++ "\n"
              ++ "Number of Downloads: " ++ (show $ downloads info) ++ "\n"
              ++ "Library: " ++ (show $ library info) ++ "\n"
              ++ "Executable: " ++ (show $ executable info) ++ "\n"

parseMode :: String -> P.ParseMode
parseMode fname              = P.defaultParseMode
  {  P.ignoreLanguagePragmas = True
  ,  P.parseFilename         = fname
  ,  P.baseLanguage          = Haskell2010
  ,  P.ignoreLinePragmas     = True
  ,  P.extensions            = allExts
  ,  P.fixities              = Nothing
  }

-- | Enable all extensions by default so we don't have to parse them
allExts :: [P.Extension]
allExts = [P.EnableExtension x | x <- [minBound..maxBound]]

-- need to pass it the include dir of the package in question
-- for this I need to change how we recurse into directories
cppOpts pkg = C.defaultCpphsOptions { C.includes = [pkg ++ "/include/"]}

-- | Try to parse a module. If fail then preprocess the module and try to parse
-- it again. Otherwise just fail
prepareModule :: FilePath -> FilePath -> IO (P.ParseResult Module)
prepareModule pkg fname = do
    -- need to preprocess the files to get rid of the #ifdef directives
    handle <- openFile fname ReadMode
    -- hSetEncoding handle utf8_bom
    contents <- hGetContents handle
    preprocessed <- C.runCpphs (cppOpts pkg) "" contents
    let pMode = parseMode fname
    case P.parseModuleWithMode pMode preprocessed of
      P.ParseOk m -> return $ P.ParseOk m
      P.ParseFailed _ _ -> do
        res <- case P.parseModuleWithMode pMode contents of
                P.ParseOk m' -> return $ P.ParseOk m'
                P.ParseFailed srcLoc message -> do
                  return $ P.ParseFailed srcLoc message
        hClose handle
        return res

-- | Collect all the imports of a Module
importModules :: P.ParseResult Module -> IO (S.Set Text)
importModules m = case m of
      P.ParseOk m -> do
        let names  = modNames m
        return names
      P.ParseFailed srcLoc message -> do
        putStrLn  $ unlines [P.prettyPrint srcLoc , message]
        return    $ S.empty

popularNames :: P.ParseResult Module -> IO [Text]
popularNames m = case m of
  P.ParseOk m -> do
    let decls    = extractDeclarations m
        names    = concatMap getNames decls
        allNames = filter (/= "") names
    return allNames
  P.ParseFailed srcLoc message -> do
    putStrLn $ unlines [P.prettyPrint srcLoc , message]
    return   $ []

findMaximum :: Map Text Int -> (Text, Int)
findMaximum mp = if null res then ("this is empty" , 0) else head res
  where res = collectMostUsed 1 mp

collectCabalInfo :: FilePath -> IO PkgInfo
collectCabalInfo fpath = do
  cabal <- tryFindPackageDesc fpath
  pkg   <- readPackageDescription silent cabal
  let desc    = flattenPackageDescription pkg
      name    = getPackageName desc
      pname   = T.pack $ show name
      version = T.pack . show $ getPackageVersion desc
      deps    = getPackageDeps desc name
      cat     = T.pack $ Cabal.category desc
      files   = map T.pack $ getAllFiles desc
      lib     = isLibrary desc
      exe     = not . null $ Cabal.executables desc
  return $ defaultInfo { name = pname
                       , pversion = version
                       , dependencies = deps
                       , category = cat
                       , library = lib
                       , executable = exe
                       }

-- | Collects the import declarations from a single package and populates the
-- PkgInfo record
collectSinglePkg :: FilePath -> PkgInfo -> IO PkgInfo
collectSinglePkg pkg pkgInfo = do
  -- get all the .hs files in the given directory
  -- if we pass a file it returns [file]
  modPaths <- F.find F.always (liftM (== ".hs") F.extension) pkg
  prepared <- mapM (prepareModule pkg)  modPaths
  imports <- mapM importModules prepared -- get the import declarations
  allNames <- concat <$> mapM popularNames prepared
  putStrLn . show $ name pkgInfo
  downloads <- scrapeDownloads (T.unpack (name pkgInfo))
  let imports'       = concatMap S.toList imports
      popularImports = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty imports'
      m              = findMaximum popularImports
      popNames       = allNames
  return $ pkgInfo  { imports      = popularImports
                    , mostImported = m
                    , moduleCount  = length modPaths
                    , names        = popNames
                    , downloads    = downloads
                    }

-- | Collects all the functions declared in the given module
collectDeclFuns m =
  case m of
    P.ParseOk m -> do
      let decls = extractDeclarations m
          funb  = map (\(FunBind xs) -> xs) $ extractFunBinds decls
          funb' = concatMap (map extractFunName) funb
      return $ S.fromList funb'
    P.ParseFailed srcLoc message -> do
      putStrLn $ unlines [P.prettyPrint srcLoc , message]
      return $ S.fromList [""]
--------------------------------------------------------------------------------
-- A Database For PkgIngo ------------------------------------------------------
--------------------------------------------------------------------------------
type InfoName = Text

data InfoDB = InfoDB !(Map InfoName PkgInfo)
    deriving (Show, Ord, Eq, Typeable)

$(deriveSafeCopy 0 'base ''InfoDB)

insertInfo :: InfoName -> PkgInfo -> Update InfoDB ()
insertInfo pkgN info = do
  InfoDB m <- S.get
  S.put (InfoDB (M.insert pkgN info m))

deleteInfo :: InfoName -> Update InfoDB ()
deleteInfo pkgN = do
  InfoDB m <- S.get
  S.put (InfoDB (M.delete pkgN m))

lookupInfo :: InfoName -> Query InfoDB (Maybe PkgInfo)
lookupInfo pkgN = do
  InfoDB m <- ask
  return (M.lookup pkgN m)

infoToList :: Query InfoDB [(InfoName,PkgInfo)]
infoToList = do
  InfoDB m <- ask
  return $ M.toList m

getMostImported :: Int -> Query InfoDB [(Text,Int)]
getMostImported n = do
  InfoDB m <- ask
  let allImports  = concatMap (M.toList . imports . snd) (M.toList m)
      popularImports =  foldl' (\m (k,v) -> M.insertWith (+) k v m) M.empty allImports
  return $ collectMostUsed n popularImports

getPopNames :: Int -> Query InfoDB ([(Text,Int)], [(Text,Int)])
getPopNames n = do
  InfoDB m <- ask
  let allNames = concatMap (names . snd) (M.toList m)
      (pnames, constructors) = partition (\t -> isLower (T.head t)) allNames
      pnames' = filter (\t -> T.length t > 2) pnames
      popularNames =  foldl' (\m k -> M.insertWith (+) k 1 m) M.empty pnames'
      popularCons  =  foldl' (\m k -> M.insertWith (+) k 1 m) M.empty constructors
  return $ (collectMostUsed n popularNames, collectMostUsed n popularCons)

countModules :: Query InfoDB Int
countModules = do
  InfoDB m <- ask
  return . sum $ map (moduleCount . snd) (M.toList m)

countLibApp :: Query InfoDB (Int,Int)
countLibApp = do
  InfoDB m <- ask
  let infos = M.elems m
      libs = foldr (\x acc -> if x then acc + 1 else acc) 0 (map library infos)
      execs = foldr (\x acc -> if x then acc + 1 else acc) 0 (map executable infos)
  return (libs,execs)

moduleStats :: Query InfoDB (Float,Float,Float,Float,Float,Float)
moduleStats = do
  InfoDB m <- ask
  let mods   = map (intToFloat . moduleCount . snd) (M.toList m)
      total  = sum mods
      mean   = N.mean mods
      median = N.median mods
      stdev  = N.stddev mods
      max    = maximum mods
      min    = minimum  mods
  return (total,mean,median,stdev,max,min)


getPkgNames :: Query InfoDB [Text]
getPkgNames = do
  InfoDB m <- ask
  return $ map fst (M.toList m)

-- | Get all the Packages where a name appears
getName :: Text -> Query InfoDB [(Text, Int)]
getName n = do
  InfoDB m <- ask
  let infos = M.elems m
      wNames = filter (checkName n) infos
  return . take 100 . sortBy (comparing (Down . snd)) $ map (occs n) wNames
    where
      checkName n pkg = if n `elem` (names pkg) then True else False
      occs n pkg = (name pkg, length $ filter (== n) (names pkg))

getMostDownloaded :: Int -> Query InfoDB [(Text,Int)]
getMostDownloaded n = do
  InfoDB m <- ask
  let pkgs = M.elems m
      dloads = map (\pkg -> (name pkg, downloads pkg)) pkgs
  return . take n $ sortBy (comparing (Down . snd))  dloads

getMostModules :: Int -> Query InfoDB [(Text,Int)]
getMostModules n = do
  InfoDB m <- ask
  let pkgs = M.elems m
      mods = map (\pkg -> (name pkg, moduleCount pkg)) pkgs
  return . take n $ sortBy (comparing (Down . snd))  mods

-- sortBy :: Query InfoDB [Text]
-- sortBy xs = do
--   InfoDB m <- ask
--   let pkgs = M.elems m
--   let apps = filter (\pkg -> executable pkg == True) pkgs
--   return . take n $ sortBy (comparing (Down . ))  mods
--



$(makeAcidic ''InfoDB [
   'insertInfo
 , 'lookupInfo
 , 'infoToList
 , 'getMostImported
 , 'moduleStats
 , 'getPopNames
 , 'countModules
 , 'getPkgNames
 , 'deleteInfo
 , 'getName
 , 'getMostDownloaded
 , 'getMostModules
 , 'countLibApp
 ])

--------------------------------------------------------------------------------
-- Running and testing ---------------------------------------------------------
--------------------------------------------------------------------------------

addPkg db dir = do
  pkgPaths <- F.find (liftM (/= 1) F.depth) (F.fileType F.==? F.Directory) dir
  let paths = filter (/= dir) pkgPaths
  mapM_ (addSinglePkg db) paths

addSinglePkg db pkgPath = do
    pkgInfo <- collectCabalInfo pkgPath
    let pkgN = name pkgInfo
    putStrLn $ show pkgN
    ispkg <- query db (LookupInfo pkgN)
    case ispkg of
      Just _  -> return ()
        -- print $ "pkg already in db " ++ (T.unpack pkgN)
      Nothing -> do
        print $ "Adding pkg " ++ (T.unpack pkgN)
        info <- collectSinglePkg pkgPath pkgInfo
        update db (InsertInfo pkgN info)

db_loop :: IO ()
db_loop = do
  hSetBuffering stdin NoBuffering
  db <- openLocalStateFrom "db/testfuns" (InfoDB M.empty)
  printInfo
  iterateUntil (== 'q') $ do
    opt <- getChar
    putStrLn ""
    case opt of
      'c' -> do
        (total,mean,median,stdev,max,min) <- query db ModuleStats
        (pkgD,dloads) <- head <$> query db (GetMostDownloaded 1)
        (pkgM, mod) <- head <$> query db (GetMostModules 1)
        (libs,execs) <- query db (CountLibApp)
        putStrLn $ "There are " ++ (show total) ++ " modules in the db"
        putStrLn $ "Mean = " ++ (show mean)
        putStrLn $ "Median = " ++ (show median)
        putStrLn $ "Stdev = " ++ (show stdev)
        putStrLn $ "Max = " ++ (show max)
        putStrLn $ "Min = " ++ (show min)
        ls <- query db InfoToList
        putStrLn $ "Coming from " ++ (show $ length ls) ++ " packages"
        putStrLn $ "Most downloaded package is " ++ (show pkgD) ++ "(" ++ (show dloads) ++ " times)"
        putStrLn $ "The package with most modules is " ++ (show pkgM) ++ "(" ++ (show mod) ++ " modules)"
        putStrLn $ "There are " ++ (show libs) ++ " libraries"
        putStrLn $ "There are " ++ (show execs) ++ " applications"
        putStr "Command: "

      'g' -> do
        putStrLn "Give me the name "
        n <- T.pack <$> getLine
        ns <- query db (GetName n)
        putStrLn $ show ns
        putStr "Command: "

      'b' -> do -- Bulk Add packages to the db
        let dir = "../../Stats/packages"
        addPkg db dir
        putStrLn "Db updated"
        putStr "Command: "

      'a' -> do -- Add package to the db
        putStrLn "Give me the path to the package"
        fpath <- getLine
        addSinglePkg db fpath
        putStrLn "Db updated"
        putStr "Command: "

      'm' -> do -- Get the MOST used module
        mostImported <- query  db (GetMostImported 1)
        putStrLn $ "Most Imported Module is " ++ (show mostImported)
        putStr "Command: "

      'd' -> do -- Delete Package info
        putStr "Give me the package name: "
        pkN <- T.pack <$> getLine
        info <- query  db (LookupInfo pkN)
        case info of
          Nothing -> putStrLn "Package not in DB"
          Just p -> do
            update db (DeleteInfo pkN)
        putStr "Command: "

      's' -> do -- Show Package info
        putStr "Give me the package name: "
        pkN <- getLine
        info <- query  db (LookupInfo (T.pack pkN))
        case info of
          Nothing -> putStrLn "Package not in DB"
          Just p -> do putStrLn $ show p
        putStr "Command: "

      'n' -> do -- Get n Most used Modules
        n <- getInt "How many modules? "
        mostImported <- query  db (GetMostImported n)
        putStrLn $ "Most Imported Module is " ++ (show mostImported)
        ls <- query db InfoToList
        putStrLn $ "From " ++ (show $ length ls) ++ " packages"
        putStr "Command: "

      'i' -> do -- Get Most Used Ids
        n <- getInt "How many ids? "
        (popNames, popCons) <- query  db (GetPopNames n)
        putStrLn $ "Most Popular Names " ++ (show popNames)
        putStrLn $ "Most Popular Cons " ++ (show popCons)
        ls <- query db InfoToList
        putStrLn $ "From " ++ (show $ length ls) ++ " packages"
        putStr "Command: "

      'l' -> do -- Get most downloaded
        putStrLn "Getting most downloaded pkgs"
        n <- getInt "How many Packages? "
        dloads <- query  db (GetMostDownloaded n)
        putStrLn $ "Most Downloaded Pkgs " ++ (show dloads)
        putStr "Command: "

      'z' -> do -- Zoom in into n packages
        zoomIn db
        putStr "Command :"
      'h' -> do
        printInfo
        putStr "Command: "
      'q' -> do
        putStrLn "Create checkpoint? (y/n)"
        resp <- getChar
        case resp of
          'y' -> do
            forkIO (createCheckpoint db)
            (putStrLn "See ya!") >> return ()
          _  -> (putStrLn "See ya!") >> return ()
      _ -> putStr "Command :"
    return $ opt
  closeAcidState db

zoomIn :: AcidState InfoDB -> IO ()
zoomIn db = do
  putStrLn "Give me a list of packages to zoom in:"
  list <- getLine
  pkgs <- catMaybes <$> mapM (\p -> query db (LookupInfo (T.pack p))) (words list)
  putStrLn "Only found the following: "
  mapM_ (putStrLn . show . name) pkgs
  let mods     = map mostImported pkgs
      ids      = concatMap names pkgs
      popNames = foldl' (\m k -> M.insertWith (+) k 1 m) M.empty ids
  putStrLn "The Most used Module is"
  putStrLn . show . findMaximum $ M.fromList mods
  putStrLn "The 20 most used ids are:"
  putStrLn . show $ collectMostUsed 20 popNames
  putStrLn $ "From " ++ show (sum $ map moduleCount pkgs) ++ " modules"
  putStrLn $ "And " ++ (show $ length pkgs) ++ " packages"



getInt :: String -> IO Int
getInt msg = do
  putStr msg
  opt <- getLine
  let res = and $ map isDigit opt
  case res of
    True ->
      return ((read opt) :: Int)
    False ->  do
     putStrLn "You didn't give me a number, I'm defaulting to 1"
     return 1

printInfo = do
  putStrLn "Options are:"
  putStrLn "a - add a package to the db"
  putStrLn "b - add packages in bulk to the db --> Stats/packages"
  putStrLn "c - Print Overall Stats"
  putStrLn "d - delete a package"
  putStrLn "g - get all the packages that contain the given ID"
  putStrLn "h - Print this info again"
  putStrLn "i - get the n most popular ids"
  putStrLn "l - get the n most downloaded pkgs"
  putStrLn "m - get most imported module"
  putStrLn "n - get the n most imported modules"
  putStrLn "s - show package info"
  putStrLn "q - quit"

main = db_loop
