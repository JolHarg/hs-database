{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Monad.IO.Class
import Data.Map                       qualified as M
import Data.Map.Strict                (Map)

type TableName = Text

class MonadDB m where

class DB conn identifier fromRow toRow fromField toField where
    getOneByIdSoftDeletedExclusive ∷ (MonadIO m, MonadReader conn m) → TableName → Text → identifier → m (Maybe fromRow)
    getOneByIdSoftDeletedInclusive ∷ (MonadIO m, MonadReader conn m) → TableName → identifier → m (Maybe fromRow)
    getOneByFieldSoftDeletedExclusive ∷ (MonadIO m, MonadReader conn m) → TableName → Text → Text → value → m (Maybe fromRow)
    getOneByFieldSoftDeletedInclusive ∷ (MonadIO m, MonadReader conn m) → TableName → Text → value → m (Maybe fromRow)
    getOneByFieldsSoftDeletedExclusive ∷ (MonadIO m, MonadReader conn m) → TableName → Text → Map Text value → m (Maybe fromRow)
    getOneByFieldsSoftDeletedInclusive ∷ (MonadIO m, MonadReader conn m) → TableName → Map Text value → m (Maybe fromRow)
    getAllSoftDeletedExclusive ∷ (MonadIO m, Traversable t, MonadReader conn m) → TableName → Text → m [fromRow]
    getAllSoftDeletedInclusive ∷ (MonadIO m, Traversable t, MonadReader conn m) → TableName → m [fromRow]
    getAllByFieldSoftDeletedExclusive ∷ (MonadIO m, MonadReader conn m) → TableName → Text → Text → value → m [fromRow]
    getAllByFieldSoftDeletedInclusive ∷ (MonadIO m, MonadReader conn m) → TableName → Text → value → m [fromRow]
    getAllByFieldsSoftDeletedExclusive ∷ (MonadIO m, MonadReader conn m) → TableName → Text → Map Text value → m [fromRow]
    getAllByFieldsSoftDeletedInclusive ∷ (MonadIO m, MonadReader conn m) → TableName → Map Text value → m [fromRow]
    insertOne ∷ forall m row returnedRow. (Data row, MonadIO m, MonadReader conn m) → TableName → TableName → toRow → m fromRow
    insertMany ∷ (Data row, MonadIO m, MonadReader conn m) → TableName → TableName → [toRow] → m [fromRow]
    updateOneByIdSoftDeleteExclusive ∷ forall m row returnedRow. (Data row, MonadIO m, MonadReader conn m) → TableName → TableName → Text → row → m (Maybe fromRow)
    updateOneByIdSoftDeleteInclusive ∷ forall m row returnedRow. (Data row, MonadIO m, MonadReader conn m) → TableName → TableName → row → m (Maybe fromRow)
    hardDeleteById ∷ (MonadIO m, MonadReader conn m) → TableName → identifier → m ()
    hardDeleteAllByField ∷ (MonadIO m, MonadReader conn m) → TableName → Text → value → m ()
    hardDeleteAllByFields ∷ (MonadIO m, MonadReader conn m) → TableName → Map Text value → m ()
    softDeleteById ∷ (MonadIO m, MonadReader conn m) → TableName → Text → identifier → m ()
    softDeleteAllByField ∷ (MonadIO m, MonadReader conn m) → TableName → Text → Text → value → m ()
    softDeleteAllByFields ∷ (MonadIO m, MonadReader conn m) → TableName → Text → Map Text value → m ()
