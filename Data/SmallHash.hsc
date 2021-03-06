{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Data.SmallHash (Table, new, insert, find, delete) where

import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

data C_Table                    -- int_to_a__table
data C_IntToANode

#include "int_to_a.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable C_Table where
  sizeOf _    = #size int_to_a__table
  alignment _ = #alignment int_to_a__table
  peek = error "Cannot peek inside table"
  poke = error "Cannot poke inside table"

newtype Table a = Table (ForeignPtr C_Table)

foreign import ccall unsafe "int_to_a__table__init_dynamic"
  c_int_to_a__table__init_dynamic :: Ptr C_Table -> Word -> IO ()

-- void int_to_a__table__add(int_to_a__table *, int key, int val);
foreign import ccall unsafe "int_to_a__table__add"
  c_int_to_a__table__add :: Ptr C_Table -> CInt -> CInt -> IO ()

-- void int_to_a__table__del(int_to_a__table *, struct int_to_a_node *);
foreign import ccall unsafe "int_to_a__table__del"
  c_int_to_a__table__del :: Ptr C_Table -> Ptr C_IntToANode -> IO ()

-- struct int_to_a_node *int_to_a__table__find(int_to_a__table *, int key);
foreign import ccall unsafe "int_to_a__table__find"
  c_int_to_a__table__find :: Ptr C_Table -> CInt -> IO (Ptr C_IntToANode)

-- int int_to_a__get_val(struct int_to_a_node *);
foreign import ccall unsafe "int_to_a__get_val"
  c_int_to_a__get_val :: Ptr C_IntToANode -> CInt

foreign import ccall unsafe "&int_to_a__table__free"
  c_int_to_a__table__free :: FunPtr (Ptr C_Table -> IO ())

anchorsCount :: Word
anchorsCount = 16384

{-# INLINE new #-}
new :: IO (Table a)
new = do
  -- TODO: Missing assignment of finalizer to table that calls int_to_a__table__free
  tablePtr <- malloc
  c_int_to_a__table__init_dynamic tablePtr anchorsCount
  tableFPtr <- newForeignPtr c_int_to_a__table__free tablePtr
  return $ Table tableFPtr

{-# INLINE insert #-}
insert :: Table a -> Int -> Int -> IO ()
insert (Table table) key val =
  withForeignPtr table $ \tablePtr ->
    c_int_to_a__table__add tablePtr (fromIntegral key) (fromIntegral val)

{-# INLINE findInternal #-}
findInternal :: IO b -> (Ptr C_Table -> Ptr C_IntToANode -> IO b) -> Table a -> Int -> IO b
findInternal notFound found (Table table) key =
  withForeignPtr table $ \tablePtr -> do
    intToAPtr <- c_int_to_a__table__find tablePtr (fromIntegral key)
    if intToAPtr == nullPtr
      then notFound
      else found tablePtr intToAPtr

{-# INLINE find #-}
find :: Table a -> Int -> IO (Maybe Int)
find = findInternal (return Nothing) $ \_ intToAPtr ->
  return $ Just $! fromIntegral $ c_int_to_a__get_val intToAPtr

{-# INLINE delete #-}
delete :: Table a -> Int -> IO ()
delete = findInternal (return ()) $ \tablePtr intToAPtr ->
  c_int_to_a__table__del tablePtr intToAPtr
