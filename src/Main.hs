{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Main where

import Data.Time
import Database.Beam
import Database.Beam.Backend.SQL.Types
import Database.Beam.Migrate
import Database.Beam.Migrate.SQL.BeamExtensions
import Database.Beam.Migrate.Simple
import Database.Beam.Postgres
import Database.Beam.Postgres.Migrate

data ModelT f = Model
  { _model_id :: C f (SqlSerial Int)
  , _model_created :: C f LocalTime
  } deriving Generic
instance Beamable ModelT

type Model = ModelT Identity

instance Table ModelT where
  data PrimaryKey ModelT f = ModelId (C f (SqlSerial Int)) deriving Generic
  primaryKey = ModelId . _model_id
instance Beamable (PrimaryKey ModelT)

data Db f = Db
  { _db_model :: f (TableEntity ModelT)
  } deriving Generic
instance Database be Db

db :: DatabaseSettings be Db
db = defaultDbSettings

migration :: _ => Migration syntax (Db (CheckedDatabaseEntity be Db))
migration = do
  model <- createTable "model" $ Model
    { _model_id = genericSerial "model_id" notNull
    , _model_created = field "model_created" timestamp notNull (defaultTo_ currentTimestamp_)
    }
  return $ Db
    { _db_model = model
    }

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  conn <- connect defaultConnectInfo
  runBeamPostgresDebug putStrLn conn $ do
    let checkedDb = runMigrationSilenced @PgCommandSyntax migration
    autoMigrate migrationBackend checkedDb
  pure ()

