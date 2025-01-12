{-# OPTIONS_GHC -Wno-x-partial #-}

import Control.Monad.IO.Class
import Data.Map               qualified as M
import Data.Map.Strict        (Map)

type TableName = Text

class MonadDB m where

class DB conn identifier fromRow toRow fromField toField where
    getOneByIdSoftDeletedExclusive ∷ MonadReader conn m → TableName → Text → identifier → m (Maybe fromRow)
    getOneByIdSoftDeletedInclusive ∷ MonadReader conn m → TableName → identifier → m (Maybe fromRow)
    getOneByFieldSoftDeletedExclusive ∷ MonadReader conn m → TableName → Text → Text → value → m (Maybe fromRow)
    getOneByFieldSoftDeletedInclusive ∷ MonadReader conn m → TableName → Text → value → m (Maybe fromRow)
    getOneByFieldsSoftDeletedExclusive ∷ MonadReader conn m → TableName → Text → Map Text value → m (Maybe fromRow)
    getOneByFieldsSoftDeletedInclusive ∷ MonadReader conn m → TableName → Map Text value → m (Maybe fromRow)
    getAllSoftDeletedExclusive ∷ (Traversable t, MonadReader conn m) → TableName → Text → m [fromRow]
    getAllSoftDeletedInclusive ∷ (Traversable t, MonadReader conn m) → TableName → m [fromRow]
    getAllByFieldSoftDeletedExclusive ∷ MonadReader conn m → TableName → Text → Text → value → m [fromRow]
    getAllByFieldSoftDeletedInclusive ∷ MonadReader conn m → TableName → Text → value → m [fromRow]
    getAllByFieldsSoftDeletedExclusive ∷ MonadReader conn m → TableName → Text → Map Text value → m [fromRow]
    getAllByFieldsSoftDeletedInclusive ∷ MonadReader conn m → TableName → Map Text value → m [fromRow]
    insertOne ∷ forall m row returnedRow. (Data row, MonadReader conn m) → TableName → TableName → toRow → m fromRow
    insertMany ∷ (Data row, MonadReader conn m) → TableName → TableName → [toRow] → m [fromRow]
    updateOneByIdSoftDeleteExclusive ∷ forall m row returnedRow. (Data row, MonadReader conn m) → TableName → TableName → Text → row → m (Maybe fromRow)
    updateOneByIdSoftDeleteInclusive ∷ forall m row returnedRow. (Data row, MonadReader conn m) → TableName → TableName → row → m (Maybe fromRow)
    hardDeleteById ∷ MonadReader conn m → TableName → identifier → m ()
    hardDeleteAllByField ∷ MonadReader conn m → TableName → Text → value → m ()
    hardDeleteAllByFields ∷ MonadReader conn m → TableName → Map Text value → m ()
    softDeleteById ∷ MonadReader conn m → TableName → Text → identifier → m ()
    softDeleteAllByField ∷ MonadReader conn m → TableName → Text → Text → value → m ()
    softDeleteAllByFields ∷ MonadReader conn m → TableName → Text → Map Text value → m ()
