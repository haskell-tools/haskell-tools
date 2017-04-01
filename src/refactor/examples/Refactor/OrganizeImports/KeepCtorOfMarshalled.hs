module Refactor.OrganizeImports.KeepCtorOfMarshalled where

import Foreign.C.String

foreign import ccall unsafe "abc" abc :: CString -> IO ()
