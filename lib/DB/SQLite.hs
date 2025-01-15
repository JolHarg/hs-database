{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module DB.SQLite where

import Control.Monad.IO.Class         (MonadIO (liftIO))
import Data.Data
import Data.Map                       qualified as M
import Data.Map.Strict                (Map)
import Data.Maybe                     (listToMaybe)
import Data.Text                      as T (Text, intercalate, pack)
import Database.SQLite.Simple         qualified as SQLite
import Database.SQLite.Simple.ToField qualified as SQLite

type TableName = Text

-- define ToRow etc
-- then indicate ToRow for this
-- go through data stuff and indexedly mappedly do things

-- reimplement SQLData

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
    SQLite.queryType :: QueryType,
    tableName :: Text,
    SQLite.queryFields :: QueryFields,
    returnFields :: ReturnFields,
    insertFields :: InsertFields,
    softDeleteOption :: SoftDeleteOption
}

SQLite.executeQuery :: MonadIO m => DB.SQLite.Query a → m a
SQLite.executeQuery = undefined
-}

-- @TODO ids ToField

getFields ∷ Data a ⇒ a → [Text]
getFields = fmap T.pack . constrFields . toConstr
{-# INLINABLE getFields #-}

getOneByIdSoftDeletedExclusive ∷ (SQLite.FromRow row, SQLite.ToField identifier, MonadIO m) ⇒ SQLite.Connection → TableName → Text → identifier → m (Maybe row)
getOneByIdSoftDeletedExclusive conn' table deletedAtField = getOneByFieldSoftDeletedExclusive conn' table deletedAtField "id"
{-# INLINABLE getOneByIdSoftDeletedExclusive #-}

getOneByIdSoftDeletedInclusive ∷ (SQLite.FromRow row, SQLite.ToField identifier, MonadIO m) ⇒ SQLite.Connection → TableName → identifier → m (Maybe row)
getOneByIdSoftDeletedInclusive conn' table = getOneByFieldSoftDeletedInclusive conn' table "id"
{-# INLINABLE getOneByIdSoftDeletedInclusive #-}

getOneByFieldSoftDeletedExclusive ∷ (SQLite.FromRow row, SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Text → Text → value → m (Maybe row)
getOneByFieldSoftDeletedExclusive conn' table deletedAtField field' value' = listToMaybe <$> liftIO (
    SQLite.query conn' (SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> field' <> " = ? AND " <> deletedAtField <> " IS NULL LIMIT 1") (SQLite.Only value'))
{-# INLINABLE getOneByFieldSoftDeletedExclusive #-}

getOneByFieldSoftDeletedInclusive ∷ (SQLite.FromRow row, SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Text → value → m (Maybe row)
getOneByFieldSoftDeletedInclusive conn' table field' value' = listToMaybe <$> liftIO (
    SQLite.query conn' (SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> field' <> " = ? LIMIT 1") (SQLite.Only value'))
{-# INLINABLE getOneByFieldSoftDeletedInclusive #-}

getOneByFieldsSoftDeletedExclusive ∷ (SQLite.FromRow row, SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Text → Map Text value → m (Maybe row)
getOneByFieldsSoftDeletedExclusive conn' table deletedAtField  fields' = listToMaybe <$> (liftIO . SQLite.query conn' (
        SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        ) <> " AND " <> deletedAtField <> " IS NULL LIMIT 1"
    ) $ M.elems fields')
{-# INLINABLE getOneByFieldsSoftDeletedExclusive #-}

getOneByFieldsSoftDeletedInclusive ∷ (SQLite.ToField value, SQLite.FromRow row, MonadIO m) ⇒ SQLite.Connection → TableName → Map Text value → m (Maybe row)
getOneByFieldsSoftDeletedInclusive conn' table fields' = listToMaybe <$> (liftIO . SQLite.query conn' (
        SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        ) <> " LIMIT 1"
    ) $ M.elems fields')
{-# INLINABLE getOneByFieldsSoftDeletedInclusive #-}

getAllSoftDeletedExclusive ∷ (SQLite.FromRow row, MonadIO m) ⇒ SQLite.Connection → TableName → Text → m [row]
getAllSoftDeletedExclusive conn' table deletedAtField = liftIO $ SQLite.query_ conn' (SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> deletedAtField <> " IS NULL")
{-# INLINABLE getAllSoftDeletedExclusive #-}

getAllSoftDeletedInclusive ∷ (SQLite.FromRow row, MonadIO m) ⇒ SQLite.Connection → TableName → m [row]
getAllSoftDeletedInclusive conn' table = liftIO $ SQLite.query_ conn' (SQLite.Query $ "SELECT * from " <> table)
{-# INLINABLE getAllSoftDeletedInclusive #-}

getAllByFieldSoftDeletedExclusive ∷ (SQLite.FromRow row, SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Text → Text → value → m [row]
getAllByFieldSoftDeletedExclusive conn' table deletedAtField field' value' =
    liftIO $ SQLite.query conn' (SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> field' <> " = ? AND " <> deletedAtField <> " IS NULL") (SQLite.Only value')
{-# INLINABLE getAllByFieldSoftDeletedExclusive #-}

getAllByFieldSoftDeletedInclusive ∷ (SQLite.FromRow row, SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Text → value → m [row]
getAllByFieldSoftDeletedInclusive conn' table field' value' =
    liftIO $ SQLite.query conn' (SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> field' <> " = ?") (SQLite.Only value')
{-# INLINABLE getAllByFieldSoftDeletedInclusive #-}

getAllByFieldsSoftDeletedExclusive ∷ (SQLite.FromRow row, SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Text → Map Text value → m [row]
getAllByFieldsSoftDeletedExclusive conn' table deletedAtField fields' = liftIO . SQLite.query conn' (
    SQLite.Query $ "SELECT * from " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        ) <> " AND " <> deletedAtField <> " IS NULL"
    ) $ M.elems fields'
{-# INLINABLE getAllByFieldsSoftDeletedExclusive #-}

getAllByFieldsSoftDeletedInclusive ∷ (SQLite.FromRow row, SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Map Text value → m [row]
getAllByFieldsSoftDeletedInclusive conn' table fields' = liftIO . SQLite.query conn' (
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

insertOne ∷ forall m row returnedRow. (Data row, SQLite.ToRow row, SQLite.FromRow returnedRow, MonadIO m) ⇒ SQLite.Connection → TableName → TableName → row → m returnedRow
insertOne conn' table toTable row = fmap head <$> liftIO $
    (SQLite.query conn' (
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
        ) $ SQLite.toRow row :: IO [returnedRow])
{-# INLINABLE insertOne #-}

insertMany ∷ (SQLite.ToRow row, Data row, SQLite.FromRow returnedRow, MonadIO m) ⇒ SQLite.Connection → TableName → TableName → [row] → m [returnedRow]
insertMany conn' table toTable = traverse (insertOne conn' table toTable)
{-# INLINABLE insertMany #-}

-- @TODO If soft deleted - when updating appendthe unique keys with "_SOFT_DELETED_TIMESTAMP_" ?
-- @TODO If soft deleted - version?
-- @TODO If soft deleted - insert into other table?

-- TODO ignore ID?
-- update all by field, update all by fields

updateOneByIdSoftDeleteExclusive ∷ forall m row returnedRow. (SQLite.FromRow returnedRow, SQLite.ToRow row, Data row, MonadIO m) ⇒ SQLite.Connection → TableName → TableName → Text → row → m (Maybe returnedRow)
updateOneByIdSoftDeleteExclusive conn' table toTable deletedAtField row = listToMaybe <$> liftIO (
    SQLite.query conn' (
        SQLite.Query $
            "UPDATE " <>
            table <>
            " SET " <>
            T.intercalate "," (
                (<> " = ?") <$> tail (getFields row) -- everything but id
            ) <> " WHERE id = ? AND " <> deletedAtField <> " IS NULL; SELECT * FROM " <> toTable <> " WHERE id = ? LIMIT 1;"
        )
        ((tail . SQLite.toRow $ row) <> [head (SQLite.toRow row), head (SQLite.toRow row)]) :: IO [returnedRow])
{-# INLINABLE updateOneByIdSoftDeleteExclusive #-}

updateOneByIdSoftDeleteInclusive ∷ forall m row returnedRow. (SQLite.FromRow returnedRow, SQLite.ToRow row, Data row, MonadIO m) ⇒ SQLite.Connection → TableName → TableName → row → m (Maybe returnedRow)
updateOneByIdSoftDeleteInclusive conn' table toTable row = listToMaybe <$> liftIO (
    SQLite.query conn' (
        SQLite.Query $
            "UPDATE " <>
            table <>
            " SET " <>
            T.intercalate "," (
                (<> " = ?") <$> tail (getFields row) -- everything but id
            ) <> " WHERE id = ?; SELECT * FROM " <> toTable <> " WHERE id = ? LIMIT 1;"
        )
        ((tail . SQLite.toRow $ row) <> [head (SQLite.toRow row), head (SQLite.toRow row)]) :: IO [returnedRow])
{-# INLINABLE updateOneByIdSoftDeleteInclusive #-}

hardDeleteById ∷ (SQLite.ToField identifier, MonadIO m) ⇒ SQLite.Connection → TableName → identifier → m ()
hardDeleteById conn' table id' = liftIO $ SQLite.execute conn' (SQLite.Query $ "DELETE FROM " <> table <> " WHERE id = ?") (SQLite.Only id')
{-# INLINABLE hardDeleteById #-}

hardDeleteAllByField ∷ (SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Text → value → m ()
hardDeleteAllByField conn' table field' value' = liftIO $ SQLite.execute conn' (SQLite.Query $ "DELETE FROM " <> table <> " WHERE " <> field' <> " = ?") (SQLite.Only value')
{-# INLINABLE hardDeleteAllByField #-}

hardDeleteAllByFields ∷ (SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Map Text value → m ()
hardDeleteAllByFields conn' table fields' = liftIO .
    SQLite.execute conn' (SQLite.Query $ "DELETE FROM " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        )
    ) $ M.elems fields'
{-# INLINABLE hardDeleteAllByFields #-}

softDeleteById ∷ (SQLite.ToField identifier, MonadIO m) ⇒ SQLite.Connection → TableName → Text → identifier → m ()
softDeleteById conn' table deletedAtField id' = liftIO $
    SQLite.execute conn' (SQLite.Query $ "UPDATE " <> table <> " SET " <> deletedAtField  <> " = DATETIME('NOW') WHERE id = ?") (SQLite.Only id')
{-# INLINABLE softDeleteById #-}

softDeleteAllByField ∷ (SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Text → Text → value → m ()
softDeleteAllByField conn' table deletedAtField field' value' = liftIO $
    SQLite.execute conn' (SQLite.Query $ "UPDATE " <> table <> " SET " <> deletedAtField <> " = DATETIME('NOW') WHERE " <> field' <> " = ?") (SQLite.Only value')
{-# INLINABLE softDeleteAllByField #-}

softDeleteAllByFields ∷ (SQLite.ToField value, MonadIO m) ⇒ SQLite.Connection → TableName → Text → Map Text value → m ()
softDeleteAllByFields conn' table deletedAtField fields' =  liftIO $
    SQLite.execute conn' (SQLite.Query $ "UPDATE " <> table <> " SET " <> deletedAtField <> " = DATETIME('NOW') WHERE " <> T.intercalate " AND " (
                (<> " = ?") <$> M.keys fields'
            )
        ) (M.elems fields')
{-# INLINABLE softDeleteAllByFields #-}
