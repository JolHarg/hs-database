{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module DB.SQLite where

import           Control.Monad                  (void)
import           Control.Monad.IO.Class         (MonadIO (liftIO))
import           Data.Data
import qualified Data.Map                       as M
import           Data.Map.Strict                (Map)
import           Data.Maybe
import           Data.Text                      as T (Text, intercalate, pack)
import           Database.SQLite.Simple
import           Database.SQLite.Simple.ToField

type TableName = Text

-- @TODO MonadDB - how is that even going to be possible though?
-- pure etc

-- todo intermediate db data model to be a map itself? hkd?

getFields ∷ (Data a) ⇒ a → [Text]
getFields = fmap T.pack . constrFields . toConstr

getOneById ∷ (ToField id, FromRow row, MonadIO m) ⇒ Connection → TableName → id → m (Maybe row)
getOneById conn' table = getOneByField conn' table "id"

getOneByField ∷ (ToField field, FromRow row, MonadIO m) ⇒ Connection → TableName → Text → field → m (Maybe row)
getOneByField conn' table field' value' = listToMaybe <$> liftIO (
    query conn' (Query $ "SELECT * from " <> table <> " WHERE " <> field' <> " = ? LIMIT 1") (Only value'))

getOneByFields ∷ (ToField field, FromRow row, MonadIO m) ⇒ Connection → TableName → Map Text field → m (Maybe row)
getOneByFields conn' table fields' = listToMaybe <$> (liftIO . query conn' (
        Query $ "SELECT * from " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        ) <> " LIMIT 1"
    ) $ M.elems fields')

getAll ∷ (FromRow row, MonadIO m) ⇒ Connection → TableName → m [row]
getAll conn' table = liftIO $ query_ conn' (Query $ "SELECT * from " <> table)

getAllByField ∷ (FromRow row, ToField field, MonadIO m) ⇒ Connection → TableName → Text → field → m [row]
getAllByField conn' table field' value' =
    liftIO $ query conn' (Query $ "SELECT * from " <> table <> " WHERE " <> field' <> " = ?") (Only value')

getAllByFields ∷ (FromRow row, ToField field, MonadIO m) ⇒ Connection → TableName → Map Text field → m [row]
getAllByFields conn' table fields' = liftIO . query conn' (
    Query $ "SELECT * from " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        )
    ) $ M.elems fields'

-- todo upsert?
insertOne ∷ forall row m. (FromRow row, ToRow row, Data row, MonadIO m) ⇒ Connection → TableName → row → m ()
insertOne conn' table row = void . liftIO $
    (query conn' (
        Query $
            "INSERT INTO " <>
            table <>
            " (" <>
            T.intercalate "," (
                getFields row
            )
            <> ") values (" <>
            T.intercalate "," (
                -- Currently, the table columns have to be in the same order as the rows in the datatype.
                "?" <$ getFields row
            ) <>
            ")"
        ) $ toRow row :: IO [row])

insertMany ∷ forall row m. (FromRow row, ToRow row, Data row, MonadIO m) ⇒ Connection → TableName → [row] → m ()
insertMany conn' table = mapM_ (insertOne conn' table)

-- TODO ignore ID?
-- update all by field, update all by fields
updateOneById ∷ forall row m. (FromRow row, ToRow row, Data row, MonadIO m) ⇒ Connection → TableName → row → m ()
updateOneById conn' table row = void $ liftIO (
    query conn' (
        Query $
            "UPDATE " <>
            table <>
            " SET " <>
            T.intercalate "," (
                (<> " = ?") <$> tail (getFields row)
            ) <> " WHERE id = ?"
        )
        ((tail . toRow $ row) <> [head . toRow $ row]) :: IO [row])

deleteById ∷ (ToField id, MonadIO m) ⇒ Connection → TableName → id → m ()
deleteById conn' table id' = liftIO $ execute conn' (Query $ "DELETE FROM " <> table <> " WHERE id = ?") (Only id')

deleteAllByField ∷ (ToField value, MonadIO m) ⇒ Connection → TableName → Text → value → m ()
deleteAllByField conn' table field' value' = liftIO $ execute conn' (Query $ "DELETE FROM " <> table <> " WHERE " <> field' <> " = ?") (Only value')

deleteAllByFields ∷ (ToField value, MonadIO m) ⇒ Connection → TableName → Map Text value → m ()
deleteAllByFields conn' table fields' = liftIO .
    execute conn' (Query $ "DELETE FROM " <> table <> " WHERE " <> T.intercalate " AND " (
            (<> " = ?") <$> M.keys fields'
        )
    ) $ M.elems fields'
