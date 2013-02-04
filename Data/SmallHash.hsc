{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}
module Data.SmallHash (Table, new, insert, find, delete) where

import Data.Word
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.StablePtr
import Foreign.Storable
import Foreign.Marshal.Alloc

data C_Table
data C_IntToANode

#include "int_to_a.h"

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

instance Storable C_Table where
  sizeOf _    = #size small_hash__table
  alignment _ = #alignment small_hash__table
  peek = error "Cannot peek inside table"
  poke = error "Cannot poke inside table"

instance Storable C_IntToANode where
  sizeOf _    = #size struct int_to_a_node
  alignment _ = #alignment struct int_to_a_node
  peek = error "Cannot peek inside int_to_a_node"
  poke = error "Cannot poke inside int_to_a_node"

newtype Table a = Table (ForeignPtr C_Table)

foreign import ccall unsafe "int_to_a__table__init_dynamic"
  c_int_to_a__table__init_dynamic :: Ptr C_Table -> Word -> IO ()

-- void int_to_a__table__add(small_hash__table *, int key, void *stable_ptr);
foreign import ccall unsafe "int_to_a__table__add"
  c_int_to_a__table__add :: Ptr C_Table -> CInt -> Ptr () -> IO ()

-- void int_to_a__table__del(small_hash__table *, struct int_to_a_node *);
foreign import ccall unsafe "int_to_a__table__del"
  c_int_to_a__table__del :: Ptr C_Table -> Ptr C_IntToANode -> IO ()

-- struct int_to_a_node *int_to_a__table__find(small_hash__table *, int key);
foreign import ccall unsafe "int_to_a__table__find"
  c_int_to_a__table__find :: Ptr C_Table -> CInt -> IO (Ptr C_IntToANode)

-- void int_to_a__get_val(struct int_to_a_node *);
foreign import ccall unsafe "int_to_a__get_val"
  c_int_to_a__get_val :: Ptr C_IntToANode -> Ptr ()

foreign import ccall unsafe "&int_to_a__table__free"
  c_int_to_a__table__free :: FunPtr (Ptr C_Table -> IO ())

anchorsCount :: Word
anchorsCount = 16384

new :: IO (Table a)
new = do
  -- TODO: Missing assignment of finalizer to table that calls int_to_a__table__free
  tablePtr <- malloc
  c_int_to_a__table__init_dynamic tablePtr anchorsCount
  tableFPtr <- newForeignPtr c_int_to_a__table__free tablePtr
  return $ Table tableFPtr

insert :: Table a -> Int -> a -> IO ()
insert (Table table) key val =
  withForeignPtr table $ \tablePtr -> do
    stablePtr <- newStablePtr val
    c_int_to_a__table__add tablePtr (fromIntegral key) (castStablePtrToPtr stablePtr)

findInternal :: IO b -> (Ptr C_Table -> Ptr C_IntToANode -> IO b) -> Table a -> Int -> IO b
findInternal notFound found (Table table) key =
  withForeignPtr table $ \tablePtr -> do
    intToAPtr <- c_int_to_a__table__find tablePtr (fromIntegral key)
    if intToAPtr == nullPtr
      then notFound
      else found tablePtr intToAPtr

find :: Table a -> Int -> IO (Maybe a)
find = findInternal (return Nothing) $ \_ intToAPtr ->
  fmap (Just $!) . deRefStablePtr . castPtrToStablePtr $
  c_int_to_a__get_val intToAPtr

delete :: Table a -> Int -> IO ()
delete = findInternal (return ()) $ \tablePtr intToAPtr -> do
  freeStablePtr . castPtrToStablePtr $ c_int_to_a__get_val intToAPtr
  c_int_to_a__table__del tablePtr intToAPtr
