
{-# LANGUAGE LambdaCase #-}

module ContTTest where

import Control.Monad.Cont
import Control.Monad.State
import Control.Applicative

-- http://blog.sigfpe.com/2011/10/quick-and-dirty-reinversion-of-control.html
-- http://blog.sigfpe.com/2014/02/reinversion-revisited.html
-- http://www.thev.net/PaulLiu/invert-inversion.html
-- https://www.fpcomplete.com/user/jwiegley/understanding-continuations

{-
newtype Label r m = Label { runLabel :: ContT r m () }

label :: ContT r m (Label r m)
label = callCC $ \k -> return $ let x = Label (k x) in x

goto :: Label r m -> ContT r m b
goto lbl = runLabel lbl >> goto lbl

usesGoto :: (Monad m) => ContT r m r -> m r
usesGoto = flip runContT return

main :: IO ()
main = usesGoto $ do
abc <- label
liftIO $ putStrLn "goto"
goto abc
-}

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
flip runContT return $ forever (do liftIO $ putStrLn "not forever"; ContT $ \_ -> return ())

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

{-
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
-}

runContTTest :: IO ()
runContTTest = flip runContT return $ do
    x <- runContTTest3
    liftIO $ putStrLn "after"
    case x of
        Just f -> f ()
        Nothing -> return ()
    runContTTest2
    lift $ putStrLn "alpha"
    -- 'Tying the Knot', the continuation needs itself as a parameter because that's what
    -- we're returning from callCC, the entry point of the continuation. Use the little
    -- workaround with 'f' so we don't run into problems with infinite types
    k <- callCC $ \k -> let f = k f in return f
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
    k

runContTTest3 :: ContT () IO (Maybe (a -> ContT () IO b))
runContTTest3 = callCC $ \leave -> do
    lift $ putStrLn "1"
    callCC $ \resume -> leave $ Just resume
    lift $ putStrLn "2"
    lift $ putStrLn "3"
    return Nothing

runContTTest2 :: ContT () IO ()
runContTTest2 = do
    lift $ putStrLn "1"
    ContT $ \_ -> return ()
    lift $ putStrLn "2"
    lift $ putStrLn "3"

runContTTest4 :: IO ()
runContTTest4 = flip runContT return $ do
    x <- runContTTest3
    liftIO $ putStrLn "after"
    case x of
        Just f -> f ()
        Nothing -> return ()
    runContTTest2
    lift $ putStrLn "alpha"
    -- 'Tying the Knot', the continuation needs itself as a parameter because that's what
    -- we're returning from callCC, the entry point of the continuation. Use the little
    -- workaround with 'f' so we don't run into problems with infinite types
    k <- callCC $ \k -> let f = k f in return f
    lift $ putStrLn "beta"          -- k
    lift $ putStrLn "gamma"         -- j
    k

-- liftIO . flip runContT return $ return runContTTest3
-- :: MonadIO m => m (ContT () IO (Maybe (a -> ContT () IO b)))

{-
a
b
1
2
3
c
-}
runContTTest5 :: IO ()
runContTTest5 = flip runContT return $ do
    liftIO $ putStrLn "a"
    x <- liftIO . flip runContT return $ return runContTTest3
    liftIO $ putStrLn "b"
    _ <- liftIO . flip runContT return $ do
             x >>= \case
                 Just f  -> f ()
                 Nothing -> return ()
    liftIO $ putStrLn "c"
    return ()

contStateTest :: IO ()
contStateTest = do
    r1 <- flip execStateT (0 :: Int) . flip runContT return $ do
        k <- callCC $ \k -> let f = k f in return f
        modify' succ
        flip when k =<< (<5) <$> get
    r2 <- flip runContT return . flip execStateT (0 :: Int) $ do
        k <- callCC $ \k -> let f = k f in return f
        modify' succ
        flip when k =<< (<5) <$> get
    putStrLn $ show r1 ++ " " ++ show r2

callbackMain :: IO ()
callbackMain = do
    forever $ do
        flip runContT return . forever $ do
            val <- yieldForInput
            yield
        pollForEvents
        display

yield :: ContT () IO ()
yield = ContT $ \k -> do return ()

yieldForInput :: ContT () IO Int
yieldForInput = ContT $ \k -> do
    setCallBack $ \val ->
        k val

setCallBack :: (Int -> IO ()) -> IO ()
setCallBack f = do
    return ()

pollForEvents :: IO ()
pollForEvents = do
    return ()

display :: IO ()
display = do
    return ()

