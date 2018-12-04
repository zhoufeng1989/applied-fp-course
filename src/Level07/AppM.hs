{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Level07.AppM where

import           Control.Monad.Except   (MonadError (..))
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (MonadReader (..))

import           Data.Text              (Text)

import           Level07.Types          (Conf, FirstAppDB)
import           Level07.Types.Error    (Error)

-- First, let's clean up our (Conf,FirstAppDB) with an application Env type. We
-- will add a general purpose logging function as well. Remember that functions
-- are values, we're able to pass them around and place them on records like any
-- other type.
data Env = Env

  -- We will add a function to take some 'Text' input and print it to the
  -- console as a crude form of logging. Construct a function that matches this
  -- type so you can include it when you create the 'Env'.
  { envLoggingFn :: Text -> AppM ()
  -- We're able to nest records to keep things neat and tidy.
  , envConfig    :: Conf
  , envDB        :: FirstAppDB
  }

-- It would be nice to remove the need to pass around our Env to every function
-- that needs it. Wouldn't it be great to have our functions run where we could
-- simply ask for the current Env?
--
-- We can create this by wrapping a function in a newtype like so:
--
-- This gives us a type that declares this function has access to our Env, and
-- will do something involving IO. It's another form of documentation and type
-- safety. AppM only has one definition and so we can easily understand what it
-- implies when used in our application.
newtype AppM a = AppM ( Env -> IO (Either Error a) )
  -- Quite often, GHC is able to write the code for us. In this case we just
  -- tell GHC that we want a Functor instance for our newtype, and it is able to
  -- correctly derive what is needed.
  deriving Functor
  -- We could do this for the rest of these instances, but that would turn into
  -- "magic" what is otherwise straight-forward implementations. You are here to
  -- learn after all.

runAppM
  :: AppM a
  -> Env
  -> IO (Either Error a)
runAppM (AppM ra) env = ra env

instance Applicative AppM where
  pure :: a -> AppM a
  pure = AppM . const . pure . pure

  (<*>) :: AppM (a -> b) -> AppM a -> AppM b
  (AppM rf) <*> (AppM ra)= AppM $ \env -> do f <- rf env
                                             a <- ra env
                                             return $ f <*> a

instance Monad AppM where
  return :: a -> AppM a
  return = AppM . const . return . return

  -- When it comes to running functions in AppM as a Monad, this will take care
  -- of passing the Env from one function to the next.
  (>>=) :: AppM a -> (a -> AppM b) -> AppM b
  (AppM ra) >>= f = AppM $ \env -> do a <- ra env
                                      case a of
                                        Left err -> return . Left $ err
                                        Right x -> runAppM (f x) env

instance MonadError Error AppM where
  throwError :: Error -> AppM a
  throwError = AppM . const . return . Left

  catchError :: AppM a -> (Error -> AppM a) -> AppM a
  catchError (AppM ra) f = AppM $ \env -> do a <- ra env
                                             case a of
                                               Left err -> runAppM (f err) env
                                               Right x -> return . return $ x

instance MonadReader Env AppM where
  -- Return the current Env from the AppM.
  ask :: AppM Env
  ask = AppM $ (\env -> return . Right $ env)

  -- Run a AppM inside of the current one using a modified Env value.
  local :: (Env -> Env) -> AppM a -> AppM a
  local f (AppM ra)= AppM (ra . f)


  -- This will run a function on the current Env and return the result.
  reader :: (Env -> a) -> AppM a
  reader f = AppM (return . Right . f)

instance MonadIO AppM where
  -- Take a type of 'IO a' and lift it into our AppM.
  liftIO :: IO a -> AppM a
  liftIO i = AppM $ \_ -> do a <- i
                             return . Right $ a

liftEither
  :: Either Error a
  -> AppM a
liftEither (Right x) = AppM $ \_ -> return . return $ x
liftEither (Left err) = AppM $ \_ -> return . Left $ err

-- Move on to ``src/Level07/DB.hs`` after this
