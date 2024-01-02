{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DB.SQLite where

import Control.Monad.IO.Class         (MonadIO (liftIO))
import Data.Data
import Data.Map                       qualified as M
import Data.Map.Strict                (Map)
import Data.Maybe                     (listToMaybe)
import Data.Text                      as T (Text, intercalate, pack)
import Database.SQLite.Simple         as SQLite
import Database.SQLite.Simple.ToField

type TableName = Text

{-}
-- @TODO MonadDB - how is that even going to be possible though?
-- pure etc

-- todo intermediate db data model to be a map itself? hkd?

-- todo take the C classes out
data QueryType = Insert | Select | Update | Delete
data QueryFields field value = NoQuery | QueryFields [(field, value)]
data UpdateFields field value = NoUpdate | UpdateFields [(field, value)]
data ReturnFields field value = ReturnAll | ReturnFields [(field, value)]
data InsertFields field value = NoInsert | InsertFields [(field, value)]
data SoftDeleteOption field = SoftDeleteExclusive | SoftDeleteInclusive field

-- Consolidate InsertFields with other
-- get what a is
data Query = Query {
    queryType :: QueryType,
    tableName :: Text,
    queryFields :: QueryFields,
    returnFields :: ReturnFields,
    insertFields :: InsertFields,
    softDeleteOption :: SoftDeleteOption
}

executeQuery :: MonadIO m => DB.SQLite.Query a → m a
executeQuery = undefined
-}

-- @TODO ids ToField

getFields ∷ Data a ⇒ a → [Text]
getFields = fmap T.pack . constrFields . toConstr
{-# INLINABLE getFields #-}

getOneByIdSoftDeletedExclusive ∷ (FromRow row, ToField id, MonadIO m) ⇒ Connection → TableName → Text → id → m (Maybe row)
getOneByIdSoftDeletedExclusive conn' table deletedAtField = getOneByFieldSoftDeletedExclusive conn' table deletedAtField "id"
{-# INLINABLE getOneByIdSoftDeletedExclusive #-}

getOneByIdSoftDeletedInclusive ∷ (FromRow row, ToField id, MonadIO m) ⇒ Connection → TableName → id → m (Maybe row)
getOneByIdSoftDeletedInclusive conn' table = getOneByFieldSoftDeletedInclusive conn' table "id"
{-# INLINABLE getOneByIdSoftDeletedInclusive #-}

getOneByFieldSoftDeletedExclusive ∷ (FromRow row, ToField value, MonadIO m) ⇒ Connection → TableName → Text → Text → value → m (Maybe row)
getOneByFieldSoftDeletedExclusive conn' table deletedAtField field' value' = listToMaybe <$> liftIO (
    query conn' (SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> field' <> " = ? AND " <> deletedAtField <> " IS NULL LIMIT 1") (Only value'))
{-# INLINABLE getOneByFieldSoftDeletedExclusive #-}

getOneByFieldSoftDeletedInclusive ∷ (FromRow row, ToField value, MonadIO m) ⇒ Connection → TableName → Text → value → m (Maybe row)
getOneByFieldSoftDeletedInclusive conn' table field' value' = listToMaybe <$> liftIO (
    query conn' (SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> field' <> " = ? LIMIT 1") (Only value'))
{-# INLINABLE getOneByFieldSoftDeletedInclusive #-}

getOneByFieldsSoftDeletedExclusive ∷ (FromRow row, ToField value, MonadIO m) ⇒ Connection → TableName → Text → Map Text value → m (Maybe row)
getOneByFieldsSoftDeletedExclusive conn' table deletedAtField  fields' = listToMaybe <$> (liftIO . query conn' (
        SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        ) <> " AND " <> deletedAtField <> " IS NULL LIMIT 1"
    ) $ M.elems fields')
{-# INLINABLE getOneByFieldsSoftDeletedExclusive #-}

getOneByFieldsSoftDeletedInclusive ∷ (ToField value, FromRow row, MonadIO m) ⇒ Connection → TableName → Map Text value → m (Maybe row)
getOneByFieldsSoftDeletedInclusive conn' table fields' = listToMaybe <$> (liftIO . query conn' (
        SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        ) <> " LIMIT 1"
    ) $ M.elems fields')
{-# INLINABLE getOneByFieldsSoftDeletedInclusive #-}

getAllSoftDeletedExclusive ∷ (FromRow row, MonadIO m) ⇒ Connection → TableName → Text → m [row]
getAllSoftDeletedExclusive conn' table deletedAtField = liftIO $ query_ conn' (SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> deletedAtField <> " IS NULL")
{-# INLINABLE getAllSoftDeletedExclusive #-}

getAllSoftDeletedInclusive ∷ (FromRow row, MonadIO m) ⇒ Connection → TableName → m [row]
getAllSoftDeletedInclusive conn' table = liftIO $ query_ conn' (SQLite.Query $ "SELECT * from " <> table)
{-# INLINABLE getAllSoftDeletedInclusive #-}

getAllByFieldSoftDeletedExclusive ∷ (FromRow row, ToField value, MonadIO m) ⇒ Connection → TableName → Text → Text → value → m [row]
getAllByFieldSoftDeletedExclusive conn' table deletedAtField field' value' =
    liftIO $ query conn' (SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> field' <> " = ? AND " <> deletedAtField <> " IS NULL") (Only value')
{-# INLINABLE getAllByFieldSoftDeletedExclusive #-}

getAllByFieldSoftDeletedInclusive ∷ (FromRow row, ToField value, MonadIO m) ⇒ Connection → TableName → Text → value → m [row]
getAllByFieldSoftDeletedInclusive conn' table field' value' =
    liftIO $ query conn' (SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> field' <> " = ?") (Only value')
{-# INLINABLE getAllByFieldSoftDeletedInclusive #-}

getAllByFieldsSoftDeletedExclusive ∷ (FromRow row, ToField value, MonadIO m) ⇒ Connection → TableName → Text → Map Text value → m [row]
getAllByFieldsSoftDeletedExclusive conn' table deletedAtField fields' = liftIO . query conn' (
    SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        ) <> " AND " <> deletedAtField <> " IS NULL"
    ) $ M.elems fields'
{-# INLINABLE getAllByFieldsSoftDeletedExclusive #-}

getAllByFieldsSoftDeletedInclusive ∷ (FromRow row, ToField value, MonadIO m) ⇒ Connection → TableName → Map Text value → m [row]
getAllByFieldsSoftDeletedInclusive conn' table fields' = liftIO . query conn' (
    SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        )
    ) $ M.elems fields'
{-# INLINABLE getAllByFieldsSoftDeletedInclusive #-}

-- maybe abstract out the joins so we don't have to have all of these separate?:

-- getAllWithLeftJoin
-- getAllWithInnerJoin
-- getAllWithRightJoin
-- getAllByFieldWithLeftJoin
-- getAllByFieldWithInnerJoin
-- get

-- todo upsert?

insertOne ∷ forall m row returnedRow. (Data row, ToRow row, FromRow returnedRow, MonadIO m) ⇒ Connection → TableName → TableName -> row → m returnedRow
insertOne conn' table toTable row = fmap head <$> liftIO $
    (query conn' (
        SQLite.Query $
            "INSERT INTO " <>
            table <>
            " (" <>
            T.intercalate "," (
                ("`" <>) . (<> "`") <$> getFields row
            )
            <> ") values (" <>
            T.intercalate "," (
                -- Currently, the table columns have to be in the same order as the rows in the datatype.
                "?" <$ getFields row
            ) <>
            "); SELECT * FROM `" <> toTable <> "` WHERE id = LAST_INSERT_ROWID() LIMIT 1;"
        ) $ toRow row :: IO [returnedRow])
{-# INLINABLE insertOne #-}

insertMany ∷ (ToRow row, Data row, FromRow returnedRow, MonadIO m) ⇒ Connection → TableName → TableName -> [row] → m [returnedRow]
insertMany conn' table toTable = mapM (insertOne conn' table toTable)
{-# INLINABLE insertMany #-}

-- @TODO If soft deleted - when updating appendthe unique keys with "_SOFT_DELETED_TIMESTAMP_" ?
-- @TODO If soft deleted - version?
-- @TODO If soft deleted - insert into other table?

-- TODO ignore ID?
-- update all by field, update all by fields

updateOneByIdSoftDeleteExclusive ∷ forall m row returnedRow. (FromRow returnedRow, ToRow row, Data row, MonadIO m) ⇒ Connection → TableName → TableName -> Text → row → m (Maybe returnedRow)
updateOneByIdSoftDeleteExclusive conn' table toTable deletedAtField row = listToMaybe <$> liftIO (
    query conn' (
        SQLite.Query $
            "UPDATE " <>
            table <>
            " SET " <>
            T.intercalate "," (
                (<> " = ?") <$> tail (getFields row) -- everything but id
            ) <> " WHERE id = ? AND " <> deletedAtField <> " IS NULL; SELECT * FROM " <> toTable <> " WHERE id = ? LIMIT 1;"
        )
        ((tail . toRow $ row) <> [head (toRow row), head (toRow row)]) :: IO [returnedRow])
{-# INLINABLE updateOneByIdSoftDeleteExclusive #-}

updateOneByIdSoftDeleteInclusive ∷ forall m row returnedRow. (FromRow returnedRow, ToRow row, Data row, MonadIO m) ⇒ Connection → TableName → TableName -> row → m (Maybe returnedRow)
updateOneByIdSoftDeleteInclusive conn' table toTable row = listToMaybe <$> liftIO (
    query conn' (
        SQLite.Query $
            "UPDATE " <>
            table <>
            " SET " <>
            T.intercalate "," (
                (<> " = ?") <$> tail (getFields row) -- everything but id
            ) <> " WHERE id = ?; SELECT * FROM " <> toTable <> " WHERE id = ? LIMIT 1;"
        )
        ((tail . toRow $ row) <> [head (toRow row), head (toRow row)]) :: IO [returnedRow])
{-# INLINABLE updateOneByIdSoftDeleteInclusive #-}

hardDeleteById ∷ (ToField id, MonadIO m) ⇒ Connection → TableName → id → m ()
hardDeleteById conn' table id' = liftIO $ execute conn' (SQLite.Query $ "DELETE FROM " <> table <> " WHERE id = ?") (Only id')
{-# INLINABLE hardDeleteById #-}

hardDeleteAllByField ∷ (ToField value, MonadIO m) ⇒ Connection → TableName → Text → value → m ()
hardDeleteAllByField conn' table field' value' = liftIO $ execute conn' (SQLite.Query $ "DELETE FROM " <> table <> " WHERE " <> field' <> " = ?") (Only value')
{-# INLINABLE hardDeleteAllByField #-}

hardDeleteAllByFields ∷ (ToField value, MonadIO m) ⇒ Connection → TableName → Map Text value → m ()
hardDeleteAllByFields conn' table fields' = liftIO .
    execute conn' (SQLite.Query $ "DELETE FROM " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        )
    ) $ M.elems fields'
{-# INLINABLE hardDeleteAllByFields #-}

softDeleteById ∷ (ToField id, MonadIO m) ⇒ Connection → TableName → Text → id → m ()
softDeleteById conn' table deletedAtField id' = liftIO $
    execute conn' (SQLite.Query $ "UPDATE " <> table <> " SET " <> deletedAtField  <> " = DATETIME('NOW') WHERE id = ?") (Only id')
{-# INLINABLE softDeleteById #-}

softDeleteAllByField ∷ (ToField value, MonadIO m) ⇒ Connection → TableName → Text → Text → value → m ()
softDeleteAllByField conn' table deletedAtField field' value' = liftIO $
    execute conn' (SQLite.Query $ "UPDATE " <> table <> " SET " <> deletedAtField <> " = DATETIME('NOW') WHERE " <> field' <> " = ?") (Only value')
{-# INLINABLE softDeleteAllByField #-}

softDeleteAllByFields ∷ (ToField value, MonadIO m) ⇒ Connection → TableName → Text → Map Text value → m ()
softDeleteAllByFields conn' table deletedAtField fields' =  liftIO $
    execute conn' (SQLite.Query $ "UPDATE " <> table <> " SET " <> deletedAtField <> " = DATETIME('NOW') WHERE " <> T.intercalate " AND " (
                (<> " = ?") <$> M.keys fields'
            )
        ) (M.elems fields')
{-# INLINABLE softDeleteAllByFields #-}
