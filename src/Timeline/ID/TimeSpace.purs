module Timeline.ID.TimeSpace where

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

newtype TimeSpaceID
  = TimeSpaceID UUID

derive instance genericTimeSpaceID :: Generic TimeSpaceID _

derive newtype instance eqTimeSpaceID :: Eq TimeSpaceID

instance showTimeSpaceID :: Show TimeSpaceID where
  show (TimeSpaceID x) = UUID.toString x

instance encodeJsonTimeSpaceID :: EncodeJson TimeSpaceID where
  encodeJson x = encodeJson (show x)

instance decodeJsonTimeSpaceID :: DecodeJson TimeSpaceID where
  decodeJson json = do
    s <- decodeJson json
    case UUID.parseUUID s of
      Nothing -> throwError $ TypeMismatch $ "Can't parse TimeSpaceID: " <> show s
      Just x -> pure (TimeSpaceID x)

instance encodeArrayBufferTimeSpaceID :: EncodeArrayBuffer TimeSpaceID where
  putArrayBuffer b o (TimeSpaceID x) = putArrayBuffer b o (map (Uint8 <<< UInt.fromInt) (UUID.toBytes x))

instance decodeArrayBufferTimeSpaceID :: DecodeArrayBuffer TimeSpaceID where
  readArrayBuffer b o = do
    (mXs :: Maybe (Vec D16 Uint8)) <- readArrayBuffer b o
    case mXs of
      Nothing -> pure Nothing
      Just xs' -> do
        let
          xs :: Vec D16 Int
          xs = map (\(Uint8 x) -> UInt.toInt x) xs'
        case UUID.parseBytesUUID xs of
          Nothing -> throw $ "Can't parse TimeSpaceID: " <> show xs
          Just y -> pure (Just (TimeSpaceID y))

instance dynamicByteLengthTimeSpaceID :: DynamicByteLength TimeSpaceID where
  byteLength (TimeSpaceID x) = byteLength (map (Uint8 <<< UInt.fromInt) (UUID.toBytes x))

instance arbitraryTimeSpaceID :: Arbitrary TimeSpaceID where
  arbitrary = do
    (xs :: Vec D16 Int) <- map (_ `mod` 256) <$> arbitrary
    unsafePartial
      $ case UUID.parseBytesUUID xs of
          Just x -> pure (TimeSpaceID x)
