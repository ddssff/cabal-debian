import Control.Monad.State (evalStateT)
import Debian.Debianize.BasicInfo (newFlags)
import Debian.Debianize.CabalInfo (newCabalInfo)
import Debian.Debianize.Details (debianDefaults)
import Debian.Debianize.Finalize (debianize)
import Debian.Debianize.Output (finishDebianization)
import System.Unix.Mount (withProcAndSys)

main :: IO ()
main = newFlags >>= withProcAndSys "/" . newCabalInfo >>= evalStateT cabalDebian
    where
      cabalDebian = do
        -- Read and inspect the cabal info to compute the debianization
        debianize debianDefaults
        -- Write, compare, or validate the resulting debianization,
        -- or print usage message, depending on options.
        finishDebianization
