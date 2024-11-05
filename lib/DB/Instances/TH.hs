{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}

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
deriveDBInstances Model { modelName, fields, extraViewFields } = do

    let fieldInstances ∷ Q [Dec]
        fieldInstances = foldQ $
            fmap (\Field { upperField = upperField, typeName = typeName } -> do
                -- let modelName' = mkName modelName
                let fieldName = mkName $ "Create" <> modelName <> upperField
                -- InstanceD Nothing [] ConT typeName []
                [d|
                    deriving via $(conT typeName) instance ToField $(conT fieldName)
                    |]
                ) fields <>
            fmap (\Field { upperField = upperField, typeName = typeName } -> do
                -- let modelName' = mkName modelName
                let fieldName = mkName $ modelName <> upperField
                -- InstanceD Nothing [] ConT typeName []
                [d|
                    deriving via $(conT typeName) instance FromField $(conT fieldName)
                    deriving via $(conT typeName) instance ToField $(conT fieldName)
                    |]
                ) (addDefaultFieldsForRetrieve (fields <> extraViewFields)) <>
            fmap (\Field { upperField = upperField, typeName = typeName } -> do
                -- let modelName' = mkName modelName
                let fieldName = mkName $ "Update" <> modelName <> upperField
                -- InstanceD Nothing [] ConT typeName []
                [d|
                    deriving via $(conT typeName) instance FromField $(conT fieldName)
                    deriving via $(conT typeName) instance ToField $(conT fieldName)
                    |]
                ) (addDefaultFieldsForUpdate fields) <>
            fmap (\Field { upperField = upperField, typeName = typeName } -> do
                -- let modelName' = mkName modelName
                let fieldName = mkName $ "Delete" <> modelName <> upperField
                -- InstanceD Nothing [] ConT typeName []
                [d|
                    deriving via $(conT typeName) instance ToField $(conT fieldName)
                    |]
                ) (addDefaultFieldsForDelete fields)

    let modelInstances ∷ Q [Dec]
        modelInstances = [d|
            instance ToRow $(conT (mkName ("Create" <> modelName)))
            instance FromRow $(conT (mkName modelName))
            instance ToRow $(conT (mkName ("Update" <> modelName)))
            instance ToRow $(conT (mkName ("Delete" <> modelName)))
            |]

    foldQ [fieldInstances, modelInstances]


deriveRetrieveDBInstances ∷ Model → Q [Dec]
deriveRetrieveDBInstances Model { modelName, fields, extraViewFields } = do

    let fieldInstances ∷ Q [Dec]
        fieldInstances = foldQ $
            fmap (\Field { upperField = upperField, typeName = typeName } -> do
                -- let modelName' = mkName modelName
                let fieldName = mkName $ modelName <> upperField
                -- InstanceD Nothing [] ConT typeName []
                [d|
                    deriving via $(conT typeName) instance ToField $(conT fieldName)
                    deriving via $(conT typeName) instance FromField $(conT fieldName)
                    |]
                ) (addDefaultFieldsForRetrieve (fields <> extraViewFields))

    let modelInstances ∷ Q [Dec]
        modelInstances = [d|
            instance FromRow $(conT (mkName modelName))
            |]

    foldQ [fieldInstances, modelInstances]
