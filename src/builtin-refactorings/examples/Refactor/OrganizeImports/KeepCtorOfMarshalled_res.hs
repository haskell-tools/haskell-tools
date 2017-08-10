module Refactor.OrganizeImports.KeepCtorOfMarshalled where

import Foreign.C.String (CString(..))

foreign import ccall unsafe "abc" abc :: CString -> IO ()
