{-# LANGUAGE DataKinds                                                     #-}
{-# LANGUAGE DeriveGeneric                                                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving                                    #-}
{-# LANGUAGE OverloadedStrings                                             #-}
{-# LANGUAGE RecordWildCards                                               #-}
{-# LANGUAGE TypeFamilies                                                  #-}
{-# LANGUAGE TypeOperators                                                 #-}

module Postmates.Client where

import           Control.Applicative
import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Monoid
import           Data.Time.Clock
import           Data.ByteString
import qualified Data.ByteString.Base64                    as B64
import           Data.Text
import qualified Data.Text.Encoding                        as T
import           GHC.Generics

import           Servant
import           Servant.Client


newtype BasicAuthHeader = BasicAuthHeader { encoded :: ByteString } deriving (Show, Read, Eq)

mkBasicAuthHeader :: ByteString -> ByteString -> BasicAuthHeader
mkBasicAuthHeader user pw = BasicAuthHeader . B64.encode $ user <> ":" <> pw

instance ToText BasicAuthHeader where
    toText BasicAuthHeader{..} = T.decodeUtf8 $ "Basic " <> encoded


data Quote = Quote { quoteId         :: Text
                   , quoteCreated    :: UTCTime
                   , quoteExpires    :: UTCTime
                   , fee             :: Integer
                   , currency        :: Text    -- TODO: keep as sum type once
                                                -- we know the possible values
                                                -- Currently only USD?
                   , dropoffEta      :: UTCTime
                   , duration        :: Integer
                   } deriving (Show, Read, Eq)

instance FromJSON Quote where
    parseJSON (Object o ) = Quote <$> o .: "id" -- TODO: manually check "kind"
                                                --       field, or let Aeson
                                                --       parser failure
                                                --       handle it?
                                  <*> o .: "created"
                                  <*> o .: "expires"
                                  <*> o .: "fee"
                                  <*> o .: "currency"
                                  <*> o .: "dropoff_eta"
                                  <*> o .: "duration"
    parseJSON _           = error "Expected and object"

data QuoteReq = QuoteReq { pickupAddy  :: Text
                          , dropoffAddy :: Text
                          } deriving (Show, Read, Eq, Generic)

instance ToFormUrlEncoded QuoteReq where
    toFormUrlEncoded QuoteReq{..} = [ ("pickup_address",  pickupAddy)
                                     , ("dropoff_address", dropoffAddy)
                                     ]



type API = "v1" :> "customers" :> Capture "customerId" Text :> "delivery_quotes" :> Header "Authorization" BasicAuthHeader :> ReqBody '[FormUrlEncoded] QuoteReq:> Post '[JSON] Quote

api :: Proxy API
api = Proxy

baseUrl :: BaseUrl
baseUrl = BaseUrl Https "api.postmates.com" 443

getQuote :: Text -> Maybe BasicAuthHeader -> QuoteReq -> EitherT ServantError IO Quote
getQuote = client api baseUrl
