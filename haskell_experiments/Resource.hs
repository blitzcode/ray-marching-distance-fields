
module Experiments.Resource ( glResourceTest
                            ) where

import Control.Applicative
import Control.Monad.Trans
import Control.Monad.Trans.Resource
import Control.Exception
import qualified Graphics.Rendering.OpenGL as GL

genObjectNameResource :: (GL.GeneratableObjectName a, MonadResource m) => m a
genObjectNameResource = snd <$> allocate GL.genObjectName GL.deleteObjectName

withSomeTextures :: (GL.TextureObject -> IO a) -> IO a
withSomeTextures f =
    runResourceT $ do
        tex <- genObjectNameResource
        liftIO $ f tex

glResourceTest :: IO ()
glResourceTest =
    withSomeTextures $ \tex -> do
        throwIO . userError $ "discard OpenGL resources"

