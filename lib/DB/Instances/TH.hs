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
deriveDBInstances Model { singularType, createFields, retrieveFields, updateFields, deleteFields } = do

    let fieldInstances ∷ Q [Dec]
        fieldInstances = foldQ $
            fmap (\Field { upperField = upperField, typeName = typeName } -> do
                -- let singularType' = mkName singularType
                let fieldName = mkName $ "Create" <> singularType <> upperField
                -- InstanceD Nothing [] ConT typeName []
                [d|
                    deriving via $(conT typeName) instance ToField $(conT fieldName)
                    |]
                ) (addDefaultFieldsForCreate createFields) <>
            fmap (\Field { upperField = upperField, typeName = typeName } -> do
                -- let singularType' = mkName singularType
                let fieldName = mkName $ singularType <> upperField
                -- InstanceD Nothing [] ConT typeName []
                [d|
                    deriving via $(conT typeName) instance FromField $(conT fieldName)
                    deriving via $(conT typeName) instance ToField $(conT fieldName)
                    |]
                ) (addDefaultFieldsForRetrieve retrieveFields) <>
            fmap (\Field { upperField = upperField, typeName = typeName } -> do
                -- let singularType' = mkName singularType
                let fieldName = mkName $ "Update" <> singularType <> upperField
                -- InstanceD Nothing [] ConT typeName []
                [d|
                    deriving via $(conT typeName) instance FromField $(conT fieldName)
                    deriving via $(conT typeName) instance ToField $(conT fieldName)
                    |]
                ) (addDefaultFieldsForUpdate updateFields) <>
            fmap (\Field { upperField = upperField, typeName = typeName } -> do
                -- let singularType' = mkName singularType
                let fieldName = mkName $ "Delete" <> singularType <> upperField
                -- InstanceD Nothing [] ConT typeName []
                [d|
                    deriving via $(conT typeName) instance ToField $(conT fieldName)
                    |]
                ) (addDefaultFieldsForDelete deleteFields)

    let modelInstances ∷ Q [Dec]
        modelInstances = [d|
            instance ToRow $(conT (mkName ("Create" <> singularType)))
            instance FromRow $(conT (mkName singularType))
            instance ToRow $(conT (mkName ("Update" <> singularType)))
            instance ToRow $(conT (mkName ("Delete" <> singularType)))
            |]

    foldQ [fieldInstances, modelInstances]


deriveRetrieveDBInstances ∷ Model → Q [Dec]
deriveRetrieveDBInstances Model { singularType, retrieveFields } = do

    let fieldInstances ∷ Q [Dec]
        fieldInstances = foldQ $
            fmap (\Field { upperField = upperField, typeName = typeName } -> do
                -- let singularType' = mkName singularType
                let fieldName = mkName $ singularType <> upperField
                -- InstanceD Nothing [] ConT typeName []
                [d|
                    deriving via $(conT typeName) instance ToField $(conT fieldName)
                    deriving via $(conT typeName) instance FromField $(conT fieldName)
                    |]
                ) (addDefaultFieldsForRetrieve retrieveFields)

    let modelInstances ∷ Q [Dec]
        modelInstances = [d|
            instance FromRow $(conT (mkName singularType))
            |]

    foldQ [fieldInstances, modelInstances]
