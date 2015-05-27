{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
module Messages.ClientRequest (ClientRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Messages.GetRequest as Messages (GetRequest)
import qualified Messages.PutRequest as Messages (PutRequest)
import qualified Messages.RemoveRequest as Messages (RemoveRequest)
import qualified Messages.Type as Messages (Type)
 
data ClientRequest = ClientRequest{type' :: !(Messages.Type), get :: !(P'.Maybe Messages.GetRequest),
                                   put :: !(P'.Maybe Messages.PutRequest), remove :: !(P'.Maybe Messages.RemoveRequest)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ClientRequest where
  mergeAppend (ClientRequest x'1 x'2 x'3 x'4) (ClientRequest y'1 y'2 y'3 y'4)
   = ClientRequest (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default ClientRequest where
  defaultValue = ClientRequest P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire ClientRequest where
  wireSize ft' self'@(ClientRequest x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 14 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 11 x'3 + P'.wireSizeOpt 1 11 x'4)
  wirePut ft' self'@(ClientRequest x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 8 14 x'1
             P'.wirePutOpt 18 11 x'2
             P'.wirePutOpt 26 11 x'3
             P'.wirePutOpt 34 11 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{type' = new'Field}) (P'.wireGet 14)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{get = P'.mergeAppend (get old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{put = P'.mergeAppend (put old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{remove = P'.mergeAppend (remove old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ClientRequest) ClientRequest where
  getVal m' f' = f' m'
 
instance P'.GPB ClientRequest
 
instance P'.ReflectDescriptor ClientRequest where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [8]) (P'.fromDistinctAscList [8, 18, 26, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".messages.ClientRequest\", haskellPrefix = [], parentModule = [MName \"Messages\"], baseName = MName \"ClientRequest\"}, descFilePath = [\"Messages\",\"ClientRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".messages.ClientRequest.type\", haskellPrefix' = [], parentModule' = [MName \"Messages\",MName \"ClientRequest\"], baseName' = FName \"type'\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".messages.Type\", haskellPrefix = [], parentModule = [MName \"Messages\"], baseName = MName \"Type\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".messages.ClientRequest.get\", haskellPrefix' = [], parentModule' = [MName \"Messages\",MName \"ClientRequest\"], baseName' = FName \"get\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".messages.GetRequest\", haskellPrefix = [], parentModule = [MName \"Messages\"], baseName = MName \"GetRequest\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".messages.ClientRequest.put\", haskellPrefix' = [], parentModule' = [MName \"Messages\",MName \"ClientRequest\"], baseName' = FName \"put\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".messages.PutRequest\", haskellPrefix = [], parentModule = [MName \"Messages\"], baseName = MName \"PutRequest\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".messages.ClientRequest.remove\", haskellPrefix' = [], parentModule' = [MName \"Messages\",MName \"ClientRequest\"], baseName' = FName \"remove\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".messages.RemoveRequest\", haskellPrefix = [], parentModule = [MName \"Messages\"], baseName = MName \"RemoveRequest\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"
 
instance P'.TextType ClientRequest where
  tellT = P'.tellSubMessage
  getT = P'.getSubMessage
 
instance P'.TextMsg ClientRequest where
  textPut msg
   = do
       P'.tellT "type" (type' msg)
       P'.tellT "get" (get msg)
       P'.tellT "put" (put msg)
       P'.tellT "remove" (remove msg)
  textGet
   = do
       mods <- P'.sepEndBy (P'.choice [parse'type', parse'get, parse'put, parse'remove]) P'.spaces
       Prelude'.return (Prelude'.foldl (\ v f -> f v) P'.defaultValue mods)
    where
        parse'type'
         = P'.try
            (do
               v <- P'.getT "type"
               Prelude'.return (\ o -> o{type' = v}))
        parse'get
         = P'.try
            (do
               v <- P'.getT "get"
               Prelude'.return (\ o -> o{get = v}))
        parse'put
         = P'.try
            (do
               v <- P'.getT "put"
               Prelude'.return (\ o -> o{put = v}))
        parse'remove
         = P'.try
            (do
               v <- P'.getT "remove"
               Prelude'.return (\ o -> o{remove = v}))