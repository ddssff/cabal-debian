import Distribution.Debian

main :: IO ()
main = debianize (Config { flags = defaultFlags {buildDeps = ["haskell-hsx-utils"] ++ buildDeps defaultFlags}
                         , modifyAtoms = id })
