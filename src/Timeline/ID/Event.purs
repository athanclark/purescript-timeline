module Timeline.ID.Event where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Generic.Rep (class Generic)
import Data.UInt (fromInt, toInt) as UInt
import Data.UUID (UUID)
import Data.UUID (toString, parseUUID, toBytes, parseBytesUUID) as UUID
import Data.Argonaut
  ( class EncodeJson
  , class DecodeJson
  , encodeJson
  , decodeJson
  , JsonDecodeError(TypeMismatch)
  )
import Data.ArrayBuffer.Class
  ( class EncodeArrayBuffer
  , class DecodeArrayBuffer
  , class DynamicByteLength
  , putArrayBuffer
  , readArrayBuffer
  , byteLength
  )
import Data.ArrayBuffer.Class.Types (Uint8(..))
import Data.Vec (Vec)
import Data.Typelevel.Num (D16)
import Control.Monad.Error.Class (throwError)
import Effect.Exception (throw)
import Test.QuickCheck (class Arbitrary, arbitrary)
import Partial.Unsafe (unsafePartial)

newtype EventID
  = EventID UUID

derive instance genericEventID :: Generic EventID _

derive newtype instance eqEventID :: Eq EventID

instance showEventID :: Show EventID where
  show (EventID x) = UUID.toString x

instance encodeJsonEventID :: EncodeJson EventID where
  encodeJson x = encodeJson (show x)

instance decodeJsonEventID :: DecodeJson EventID where
  decodeJson json = do
    s <- decodeJson json
    case UUID.parseUUID s of
      Nothing -> throwError $ TypeMismatch $ "Can't parse EventID: " <> show s
      Just x -> pure (EventID x)

instance encodeArrayBufferEventID :: EncodeArrayBuffer EventID where
  putArrayBuffer b o (EventID x) = putArrayBuffer b o (map (Uint8 <<< UInt.fromInt) (UUID.toBytes x))

instance decodeArrayBufferEventID :: DecodeArrayBuffer EventID where
  readArrayBuffer b o = do
    (mXs :: Maybe (Vec D16 Uint8)) <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs' -> do
        let
          xs :: Vec D16 Int
          xs = map (\(Uint8 x) -> UInt.toInt x) xs'
        case UUID.parseBytesUUID xs of
          Nothing -> throw $ "Can't parse EventID: " <> show xs
          Just y -> pure (Just (EventID y))

instance dynamicByteLengthEventID :: DynamicByteLength EventID where
  byteLength (EventID x) = byteLength (map (Uint8 <<< UInt.fromInt) (UUID.toBytes x))

instance arbitraryEventID :: Arbitrary EventID where
  arbitrary = do
    (xs :: Vec D16 Int) <- map (_ `mod` 256) <$> arbitrary
    unsafePartial
      $ case UUID.parseBytesUUID xs of
          Just x -> pure (EventID x)
