module Timeline.ID.TimeSpan where

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

newtype TimeSpanID
  = TimeSpanID UUID

derive instance genericTimeSpanID :: Generic TimeSpanID _

derive newtype instance eqTimeSpanID :: Eq TimeSpanID

instance showTimeSpanID :: Show TimeSpanID where
  show (TimeSpanID x) = UUID.toString x

instance encodeJsonTimeSpanID :: EncodeJson TimeSpanID where
  encodeJson x = encodeJson (show x)

instance decodeJsonTimeSpanID :: DecodeJson TimeSpanID where
  decodeJson json = do
    s <- decodeJson json
    case UUID.parseUUID s of
      Nothing -> throwError $ TypeMismatch $ "Can't parse TimeSpanID: " <> show s
      Just x -> pure (TimeSpanID x)

instance encodeArrayBufferTimeSpanID :: EncodeArrayBuffer TimeSpanID where
  putArrayBuffer b o (TimeSpanID x) = putArrayBuffer b o (map (Uint8 <<< UInt.fromInt) (UUID.toBytes x))

instance decodeArrayBufferTimeSpanID :: DecodeArrayBuffer TimeSpanID where
  readArrayBuffer b o = do
    (mXs :: Maybe (Vec D16 Uint8)) <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs' -> do
        let
          xs :: Vec D16 Int
          xs = map (\(Uint8 x) -> UInt.toInt x) xs'
        case UUID.parseBytesUUID xs of
          Nothing -> throw $ "Can't parse TimeSpanID: " <> show xs
          Just y -> pure (Just (TimeSpanID y))

instance dynamicByteLengthTimeSpanID :: DynamicByteLength TimeSpanID where
  byteLength (TimeSpanID x) = byteLength (map (Uint8 <<< UInt.fromInt) (UUID.toBytes x))

instance arbitraryTimeSpanID :: Arbitrary TimeSpanID where
  arbitrary = do
    (xs :: Vec D16 Int) <- map (_ `mod` 256) <$> arbitrary
    unsafePartial
      $ case UUID.parseBytesUUID xs of
          Just x -> pure (TimeSpanID x)
