module Test.Mutagen.Exception where

import qualified Control.Exception as Exc

type AnException = Exc.SomeException

tryEvaluate :: a -> IO (Either AnException a)
tryEvaluate x = tryEvaluateIO (return x)

tryEvaluateIO :: IO a -> IO (Either AnException a)
tryEvaluateIO m = Exc.tryJust notAsync (m >>= Exc.evaluate)
  where
    notAsync :: AnException -> Maybe AnException
    notAsync e = case Exc.fromException e of
        Just (Exc.SomeAsyncException _) -> Nothing
        Nothing                         -> Just e

evaluate :: a -> IO a
evaluate = Exc.evaluate

-- | Test if an exception was a @^C@.
-- QuickCheck won't try to shrink an interrupted test case.
isInterrupt :: AnException -> Bool
isInterrupt e = Exc.fromException e == Just Exc.UserInterrupt

-- | A special error value. If a property evaluates 'discard', it
-- causes QuickCheck to discard the current test case.
-- This can be useful if you want to discard the current test case,
-- but are somewhere you can't use 'Test.QuickCheck.==>', such as inside a
-- generator.
discard :: a
isDiscard :: AnException -> Bool
(discard, isDiscard) =
  (error msg, isDiscard')
 where
  msg = "DISCARD. You should not see this exception, it is internal to Mutagen."
  isDiscard' e =
    case Exc.fromException e of
      Just (Exc.ErrorCall msg') -> msg' == msg
      _                         -> False

finally :: IO a -> IO b -> IO a
finally = Exc.finally
