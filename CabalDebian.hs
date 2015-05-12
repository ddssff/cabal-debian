import Debian.Debianize.Details (debianDefaults)
import Debian.Debianize.Output (performDebianization)

main :: IO ()
main = performDebianization debianDefaults
