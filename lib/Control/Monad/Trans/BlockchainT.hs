module Control.Monad.Trans.BlockchainT
    ( BlockchainT
    , BlockchainT'
    , runBlockchainT
    , blockchainConfig
    , blockchainDB
    ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Except

import Data.Blockchain.Types      hiding (blockchainConfig)

type BlockchainT e m = BlockchainT' (Config, DB m) (ExceptT e m)

newtype BlockchainT' r m a = BlockchainT' { unBlockchainT :: ReaderT r m a }
    deriving
        ( Functor
        , Applicative
        , Alternative
        , Monad
        , MonadTrans
        )

instance Monad m => MonadReader r (BlockchainT' r m) where
    ask     = BlockchainT' ask
    local f = BlockchainT' . local f . unBlockchainT
    reader  = BlockchainT' . reader

runBlockchainT :: Monad m => Config -> DB m -> BlockchainT e m a -> ExceptT e m a
runBlockchainT config interface act = runReaderT (unBlockchainT act) (config, interface)

blockchainConfig :: Monad m => BlockchainT e m Config
blockchainConfig = fst <$> ask

blockchainDB :: Monad m => BlockchainT e m (DB m)
blockchainDB = snd <$> ask
