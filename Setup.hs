import           Control.Monad (when)
import qualified Data.Map as Map
import           Distribution.Simple
import           Distribution.Simple.LocalBuildInfo (LocalBuildInfo, withPrograms)
import           Distribution.Simple.Program (Program(..), simpleProgram, findProgramVersion, lookupProgram, runProgram)
import           Distribution.Simple.Setup (fromFlag, buildVerbosity)
import           Distribution.Simple.Utils (moreRecentFile)

npmProgram :: Program
npmProgram = (simpleProgram "npm")
  { programFindVersion = findProgramVersion "-v" id
  }

main = defaultMainWithHooks simpleUserHooks
  { hookedPrograms = [npmProgram] ++ hookedPrograms simpleUserHooks
  , postBuild = \args flag desc lbi -> do
    let verb = fromFlag $ buildVerbosity flag
        Just npm = lookupProgram npmProgram $ withPrograms lbi
    r <- moreRecentFile "hawk.ts" "hawk.js"
    when r $ runProgram verb npm ["run", "build"]
    postBuild simpleUserHooks args flag desc lbi
  }
