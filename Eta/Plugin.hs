{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
module Eta.Plugin(plugin, EtaAnnotation(..)) where

import Control.Applicative
import Control.Monad
import Data.Data
import GhcPlugins

#if __GLASGOW_HASKELL__ >= 706
#define SHOW_S_DOC(dynFlags) showSDoc dynFlags
#else
#define SHOW_S_DOC(dynFlags) const showSDoc dynFlags
#endif

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todos = do
    reinitializeGlobals
    return $ CoreDoPluginPass "Eta" eta : todos

data EtaAnnotation = EtaArity !Int
    deriving (Show, Eq, Ord, Data, Typeable)

type EtaEnv = UniqFM EtaAnnotation

eta :: PluginPass
eta guts = do
    env <- getFirstAnnotations deserializeWithData guts
    bindsOnlyPass (mapM (etaBind env)) guts

etaBind :: EtaEnv -> CoreBind -> CoreM CoreBind
etaBind env (NonRec v body) = NonRec v <$> etaBind1 env v body
etaBind env (Rec binds) = Rec <$> mapM f binds
    where
        f (v, body) = (,) v <$> etaBind1 env v body

etaBind1 :: EtaEnv -> Id -> CoreExpr -> CoreM CoreExpr
etaBind1 env v (Lam arg body) =
    Lam arg <$> etaBindLam env (varType v) body
etaBind1 _ _ e = return e

etaBindLam :: EtaEnv -> Type -> CoreExpr -> CoreM CoreExpr
etaBindLam env funTy body
    | Just (_, bodyTy) <- splitFunTy_maybe funTy
    = case getArity env bodyTy of
        Just arity -> etaWithArity env arity bodyTy body
        Nothing -> case body of
            Lam arg1 body1 -> Lam arg1 <$> etaBindLam env bodyTy body1
            _ -> return body
    | otherwise = return body

etaWithArity :: EtaEnv -> Int -> Type -> CoreExpr -> CoreM CoreExpr
etaWithArity _ arity bodyTy body = do
    dynFlags <- getDynFlags
    liftIO $ putStrLn $ "arity=" ++ show arity
    liftIO $ putStrLn $ SHOW_S_DOC(dynFlags) $ ppr body
    case unFunction arity bodyTy of
        Just (argsTy, co) -> do
            liftIO $ putStrLn $ SHOW_S_DOC(dynFlags) $ ppr argsTy
            liftIO $ putStrLn $ SHOW_S_DOC(dynFlags) $ ppr co
            body' <- flip Cast (mkSymCo co) <$>
                etaFun argsTy (Cast body co)
            liftIO $ putStrLn $ SHOW_S_DOC(dynFlags) $ ppr body'
            return body'
        Nothing -> do
            liftIO $ putStrLn "Eta: not enough arity"
            return body

etaFun :: [Type] -> CoreExpr -> CoreM CoreExpr
etaFun argsTy body0 = foldM f body0 $ reverse argsTy
    where
        f body argTy = do
            argId <- mkSysLocalM (mkFastString "etaplug") argTy
            return $ Lam argId (App body (Var argId))

getArity :: EtaEnv -> Type -> Maybe Int
getArity env ty = do
    tycon <- tyConAppTyCon_maybe ty
    EtaArity n <- lookupUFM env tycon
    return n

unFunction :: Int -> Type -> Maybe ([Type], Coercion)
unFunction 0 ty = Just ([], mkReflCo ty)
unFunction n ty = do
    (argTy, bodyTy, funCo) <- unFunction1 (mkReflCo ty) ty
    (bodyArgTys, bodyCo) <- unFunction (n-1) bodyTy
    let !co = mkTransCo funCo (mkFunCo (mkReflCo argTy) bodyCo)
    return (argTy:bodyArgTys, co)

unFunction1 :: Coercion{-::orig_ty~ty-} -> Type -> Maybe (Type, Type, Coercion)
unFunction1 !co ty
    | Just (argTy, bodyTy) <- splitFunTy_maybe ty
        = Just (argTy, bodyTy, co)
    | Just (tyCon, tyArgs) <- splitTyConApp_maybe ty
    , Just (ty', tyToFun) <- instNewTyCon_maybe tyCon tyArgs
        = unFunction1 (mkTransCo co tyToFun) ty'
    | otherwise = Nothing
