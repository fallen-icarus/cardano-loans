module CLI.Data.Output where

import Relude 

-- | For when saving to file is optional
data Output = Stdout | File FilePath

data Format = JSON | Pretty | Plain
