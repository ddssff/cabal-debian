import Debian.Debianize.Details (debianDefaults)
import Debian.Debianize.Output (performDebianizationOfWebsite)

main :: IO ()
main = performDebianizationOfWebsite debianDefaults
