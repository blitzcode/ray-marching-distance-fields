
module ContTTest where

import Control.Monad.Cont

-- http://blog.sigfpe.com/2011/10/quick-and-dirty-reinversion-of-control.html
-- http://blog.sigfpe.com/2014/02/reinversion-revisited.html
-- http://www.thev.net/PaulLiu/invert-inversion.html
-- https://www.fpcomplete.com/user/jwiegley/understanding-continuations

{-
newtype ContT r m a = ContT { runContT :: (a -> m r) -> m r }

runContT :: ContT r m a -> (a -> m r) -> m r

unitAttack :: Target -> ContT () IO Target
unitAttack target = ContT $ \todo -> do
    swingAxeBack 60
    valid <- isTargetValid target
    if valid
    then todo target
    else sayUhOh

runContT (unitAttack target) damageTarget :: IO ()

callCC :: ((a -> ContT r m b) -> ContT r m a) -> ContT r m a

flip runContT return . callCC $ \exit -> forever (do liftIO $ putStrLn "not forever"; exit ())

-}

{-
runContTTest :: IO ()
runContTTest = flip runContT return $ do
    lift $ putStrLn "alpha"
    (num, num2) <- callCC $ \k -> do
      k (43, 0)
      return (44, 0)
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
    lift $ print num                -- l
-}

runContTTest :: IO ()
runContTTest = flip runContT return $ do
    lift $ putStrLn "alpha"
    (k, num) <- callCC $ \k -> let f x = k (f, x)
                               in return (f, 0)
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
    if num < 5
        then k (num + 1) >> return ()
        else lift $ print num       -- l

