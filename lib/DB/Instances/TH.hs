{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module DB.Instances.TH where

import Data.Model
import Data.Model.TH
import Database.SQLite.Simple
import Database.SQLite.Simple.FromField
import Database.SQLite.Simple.ToField
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

foldQ ∷ [Q [a]] → Q [a]
foldQ = fmap concat . sequenceQ

concatMapQ ∷ (a → Q [b]) → [a] → Q [b]
concatMapQ x = foldQ . fmap x

deriveDBInstances ∷ Model → Q [Dec]
deriveDBInstances Model { modelName, fields } = do

    let fieldInstances ∷ Q [Dec]
        fieldInstances = foldQ $ fmap (\Field { upperField = upperField, typeName = typeName } -> do
            -- let modelName' = mkName modelName
            let fieldName = mkName $ modelName <> upperField
            -- InstanceD Nothing [] ConT typeName []
            [d|
                deriving via $(conT typeName) instance FromField $(conT fieldName)
                deriving via $(conT typeName) instance ToField $(conT fieldName)
                |]
            ) (addDefaultFields fields)

    let modelInstances ∷ Q [Dec]
        modelInstances = [d|
            instance FromRow $(conT (mkName modelName))
            instance ToRow $(conT (mkName modelName))
            |]

    foldQ [fieldInstances, modelInstances]
