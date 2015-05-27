{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Messages.Type (Type(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Type = GET
          | PUT
          | REMOVE
          deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Type
 
instance Prelude'.Bounded Type where
  minBound = GET
  maxBound = REMOVE
 
instance P'.Default Type where
  defaultValue = GET
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Type
toMaybe'Enum 1 = Prelude'.Just GET
toMaybe'Enum 2 = Prelude'.Just PUT
toMaybe'Enum 3 = Prelude'.Just REMOVE
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Type where
  fromEnum GET = 1
  fromEnum PUT = 2
  fromEnum REMOVE = 3
  toEnum = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Messages.Type") . toMaybe'Enum
  succ GET = PUT
  succ PUT = REMOVE
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Messages.Type"
  pred PUT = GET
  pred REMOVE = PUT
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Messages.Type"
 
instance P'.Wire Type where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Type
 
instance P'.MessageAPI msg' (msg' -> Type) Type where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Type where
  reflectEnum = [(1, "GET", GET), (2, "PUT", PUT), (3, "REMOVE", REMOVE)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".messages.Type") [] ["Messages"] "Type") ["Messages", "Type.hs"]
      [(1, "GET"), (2, "PUT"), (3, "REMOVE")]
 
instance P'.TextType Type where
  tellT = P'.tellShow
  getT = P'.getRead