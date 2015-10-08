# cabal-debian
Create a Debianization by examining a .cabal file.

Here is a quick reference between command line options
and the equivalent lens to use in the debian/Debianize.hs script.

| Command Line | Lens |
| ------------ | ---- |
| --debian-name-base | debInfo . overrideDebianNameBase |
| --executable | debInfo . executable |
| --cabal-flags | debinfo . flags . cabalFlagAssignments |
