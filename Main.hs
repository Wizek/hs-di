import Data.Dynamic

data IOCC = IOCC String Deps Value	

type Deps = Dynamic
type Value = Dynamic