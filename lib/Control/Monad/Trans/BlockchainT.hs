module Control.Monad.Trans.BlockchainT
    ( BlockchainT
    , BlockchainT'
    , runBlockchainT
    , blockchainConfig
    , blockchainInterface
    , BlockchainInterface(..)
    ) where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.Trans.Except

import Data.Blockchain.Types      hiding (blockchainConfig)

type BlockchainT e m = BlockchainT' (Config, BlockchainInterface m) (ExceptT e m)

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

runBlockchainT :: Monad m => Config -> BlockchainInterface m -> BlockchainT e m a -> ExceptT e m a
runBlockchainT config interface act = runReaderT (unBlockchainT act) (config, interface)

blockchainConfig :: Monad m => BlockchainT e m Config
blockchainConfig = fst <$> ask

blockchainInterface :: Monad m => BlockchainT e m (BlockchainInterface m)
blockchainInterface = snd <$> ask
