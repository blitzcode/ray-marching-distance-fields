
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Main (main) where

import Control.Monad
import Control.Monad.Trans (MonadTrans(..))
import Control.Applicative
import System.Random
import Data.Char

someFunc :: Int -> Int -> Int -> Int
someFunc a b c = a + b + c

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
    a >>= b = b $ runIdentity a
    return  = Identity
    fail    = error

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance Monad m => Monad (ReaderT r m) where
    a >>= b = ReaderT $ \r -> do am <- runReaderT a r
                                 runReaderT (b am) r
    return  = lift . return
    fail    = error

instance MonadTrans (ReaderT r) where
    lift a = ReaderT (\_ -> a)

ask :: Monad m => ReaderT r m r
ask = ReaderT return

main :: IO ()
main = do
    let r1 = someFunc 1 2 3

    a <- randomIO
    b <- randomIO
    c <- randomIO
    let r2 = someFunc a b c

    r3 <- someFunc <$> randomIO <*> randomIO <*> randomIO

    r4 <- liftM3 someFunc randomIO randomIO randomIO

    r5 <- someFunc <$> randomIO <*> (return 5) <*> randomIO

    void . liftM2 when ((> 5) <$> (randomIO :: IO Int)) . return $
        putStrLn "Larger than 5"

    flip when (putStrLn "Larger than 5") =<< (> 5) <$> (randomIO :: IO Int)

    let tmp = convert (10 :: Int)

    let tmpM = runIdentity $ do x <- return 2
                                y <- return 3
                                z <- return 3
                                return $ x + y
    print tmpM

    let tmpM2 = runIdentity . flip runReaderT (7 :: Int) $ do x <- return 2
                                                              y <- return 3
                                                              z <- ask
                                                              return $ x + y + z
    print tmpM2

    return ()


class Convertible a b | a -> b where
  convert :: a -> b

--instance Convertible Int Integer where
  --convert = toInteger

instance Convertible Int Char where
  convert = chr

instance Convertible Char Int where
  convert = ord

