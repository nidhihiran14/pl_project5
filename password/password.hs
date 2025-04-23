module Password where

newtype PwdOp a = PwdOp { runPwdOp' :: String -> (a, String) }

instance Functor PwdOp where
    fmap f (PwdOp g) = PwdOp $ \s -> let (a, s') = g s in (f a, s')

instance Applicative PwdOp where
    pure a = PwdOp $ \s -> (a, s)
    (PwdOp f) <*> (PwdOp g) = PwdOp $ \s -> 
        let (fab, s1) = f s
            (a, s2) = g s1
        in (fab a, s2)

instance Monad PwdOp where
    return = pure
    (PwdOp f) >>= k = PwdOp $ \s ->
        let (a, s1) = f s
            PwdOp g = k a
        in g s1

setPassword :: String -> PwdOp ()
setPassword newPwd = PwdOp $ \_ -> ((), newPwd)

checkPassword :: String -> PwdOp Bool
checkPassword input = PwdOp $ \pwd -> (input == pwd, pwd)

runPwdOp :: PwdOp a -> a
runPwdOp op = fst (runPwdOp' op "")