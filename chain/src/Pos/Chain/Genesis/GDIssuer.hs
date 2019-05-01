{-# LANGUAGE TypeFamilies #-}

module Pos.Chain.Genesis.GDIssuer
       ( GDIssuer (..)
       ) where

import           Universum

import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import qualified Text.JSON.Canonical as Canonical (FromJSON (..),
                     ReportSchemaErrors, ToJSON (..))
import qualified Formatting.Buildable as Buildable
import           Formatting (bprint, build, (%))

import           Pos.Core.Common (Address)

-- | Owner of the address have the permission to issue/destroy GDs
newtype GDIssuer = GDIssuer
    { getIssuer :: Address
    } deriving (Show, Eq)

instance Buildable GDIssuer where
    build (GDIssuer m) =
        bprint ("Issuer: " %build) m

instance Monad m => Canonical.ToJSON m GDIssuer where
    toJSON = Canonical.toJSON . getIssuer

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m GDIssuer where
    fromJSON = fmap GDIssuer . Canonical.fromJSON

deriving instance Aeson.ToJSON GDIssuer
deriving instance Aeson.FromJSON GDIssuer