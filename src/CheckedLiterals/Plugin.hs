{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module CheckedLiterals.Plugin (plugin) where

import GHC.Hs
import Prelude

import Control.Monad (unless)
import Data.Generics (Data, extQ, gmapQ)
import Data.Maybe (maybeToList)
import Data.Ratio qualified as Ratio
import Data.Ratio.Extra qualified as RatioExtra
import GHC.Core.Class (Class, className)
import GHC.Core.Predicate (mkClassPred)
import GHC.Data.Bag (listToBag)
import GHC.Iface.Env (lookupOrig)
import GHC.Plugins hiding (rational, (<>))
import GHC.Tc.Errors (reportAllUnsolved)
import GHC.Tc.Solver (simplifyWantedsTcM, tcCheckGivens, tcCheckWanteds)
#if MIN_VERSION_ghc(9,14,0)
import GHC.Tc.Solver.InertSet (InertSet, emptyInertSet)
#else
import GHC.Tc.Solver.InertSet (InertSet, emptyInert)
#endif
import GHC.Tc.Types (TcGblEnv (tcg_binds), TcM)
import GHC.Tc.Types.Evidence (HsWrapper (..))
import GHC.Tc.Types.Origin (CtOrigin (OccurrenceOf))
import GHC.Tc.Utils.Env (tcLookupClass)
#if MIN_VERSION_ghc(9,14,0)
import GHC.Tc.Utils.Monad (getTcLevel, getTopEnv, setSrcSpan)
#else
import GHC.Tc.Utils.Monad (getTopEnv, setSrcSpan)
#endif
import GHC.Tc.Utils.TcMType (newWanted)
import GHC.Types.SourceText (
  SourceText (NoSourceText, SourceText),
  il_value,
 )

import CheckedLiterals.Class.Integer (
  CheckedNegativeIntegerLiteral,
  CheckedPositiveIntegerLiteral,
 )
import CheckedLiterals.Class.Rational (
  CheckedNegativeRationalLiteral,
  CheckedPositiveRationalLiteral,
 )
import CheckedLiterals.Unchecked (uncheckedLiteral)
import GHC.Types.SourceText qualified as SourceText
import Language.Haskell.TH qualified as TH

data PluginThings = PluginThings
  { checkedPositiveIntegerLiteralClass :: Class
  , checkedNegativeIntegerLiteralClass :: Class
  , checkedPositiveRationalLiteralClass :: Class
  , checkedNegativeRationalLiteralClass :: Class
  , uncheckedLiteralVarName :: Name
  }

data PendingCheck = PendingCheck
  { pendingCheckLocation :: SrcSpan
  , pendingCheckGivens :: [EvVar]
  , pendingCheckOrigin :: Name
  , pendingCheckPredicate :: PredType
  }

data FunBindGivens = FunBindGivens
  { funBindMatchGivens :: [EvVar]
  , funBindRhsOnlyGivens :: [EvVar]
  }

data LiteralPolarity = PositiveLiteral | NegativeLiteral

-- | The GHC plugin entry point
plugin :: Plugin
plugin =
  defaultPlugin
    { typeCheckResultAction = typeCheckPlugin
    , pluginRecompile = purePlugin
    }

{- | Validate literals after typechecking, when required type arguments are
already disambiguated from term arguments.
-}
typeCheckPlugin :: [CommandLineOption] -> ModSummary -> TcGblEnv -> TcM TcGblEnv
typeCheckPlugin _opts _modSummary tcGblEnv = do
  pluginThings <- loadPluginThings
  mapM_ runPendingCheck (collectPendingChecks pluginThings (tcg_binds tcGblEnv))
  pure tcGblEnv

runPendingCheck :: PendingCheck -> TcM ()
runPendingCheck
  PendingCheck
    { pendingCheckLocation = location
    , pendingCheckGivens = givens
    , pendingCheckOrigin = origin
    , pendingCheckPredicate = predicate
    } =
    setSrcSpan location $ do
      alreadySatisfied <-
        case givens of
          [] ->
            pure False
          _ -> do
            initialInertSet <- emptyInertCompat
            maybeInertSet <- tcCheckGivens initialInertSet (listToBag givens)
            case maybeInertSet of
              Nothing -> pure True
              Just solvedInertSet -> tcCheckWanteds solvedInertSet [predicate]
      unless alreadySatisfied $ do
        wantedConstraints <-
          simplifyWantedsTcM
            =<< sequence
              [ newWanted
                  (OccurrenceOf origin)
                  Nothing
                  predicate
              ]
        reportAllUnsolved wantedConstraints

collectPendingChecks :: PluginThings -> LHsBinds GhcTc -> [PendingCheck]
collectPendingChecks pluginThings = collectChecks pluginThings []

collectChecks :: (Data a) => PluginThings -> [EvVar] -> a -> [PendingCheck]
collectChecks pluginThings givens =
  (concat . gmapQ (collectChecks pluginThings givens))
    `extQ` collectExpressionChecks pluginThings givens
    `extQ` collectPatternChecks pluginThings givens
    `extQ` collectBindChecks pluginThings givens

collectBindChecks :: PluginThings -> [EvVar] -> LHsBind GhcTc -> [PendingCheck]
collectBindChecks pluginThings givens (L _ bind) =
  case bind of
    FunBind{fun_ext = funExt, fun_matches = matches} ->
      collectFunBindChecks pluginThings givens (funBindGivens funExt) matches
    PatBind{pat_lhs = lhs, pat_rhs = rhs} ->
      collectChecks pluginThings givens lhs <> collectChecks pluginThings givens rhs
    VarBind{var_rhs = rhs} ->
      collectChecks pluginThings givens rhs
    PatSynBind _ patSynBind ->
      collectChecks pluginThings givens patSynBind
    XHsBindsLR absBinds ->
      case absBinds of
        AbsBinds{abs_ev_vars = extraGivens, abs_binds = binds} ->
          collectChecks pluginThings (givens <> extraGivens) binds

collectFunBindChecks ::
  PluginThings ->
  [EvVar] ->
  FunBindGivens ->
  MatchGroup GhcTc (LHsExpr GhcTc) ->
  [PendingCheck]
collectFunBindChecks pluginThings givens funGivens (MG _ (L _ matches)) =
  concatMap (collectFunBindMatchChecks pluginThings givens funGivens) matches

collectFunBindMatchChecks ::
  PluginThings ->
  [EvVar] ->
  FunBindGivens ->
  LMatch GhcTc (LHsExpr GhcTc) ->
  [PendingCheck]
collectFunBindMatchChecks pluginThings givens funGivens (L _ match) =
  case (funGivens, match) of
    ( FunBindGivens
        { funBindMatchGivens = matchExtraGivens
        , funBindRhsOnlyGivens = rhsOnlyGivens
        }
      , Match{m_pats = pats, m_grhss = grhss}
      ) ->
        let matchGivens = givens <> matchExtraGivens
            rhsGivens = matchGivens <> rhsOnlyGivens
         in concatMap (collectChecks pluginThings matchGivens) pats
              <> collectChecks pluginThings rhsGivens grhss

funBindGivens :: XFunBind GhcTc GhcTc -> FunBindGivens
funBindGivens (wrapper, _) = classifyFunBindWrapper wrapper

classifyFunBindWrapper :: HsWrapper -> FunBindGivens
classifyFunBindWrapper wrapper =
  case wrapper of
    WpHole -> memptyFunBindGivens
    WpCompose wrapper1 wrapper2 ->
      combineFunBindGivens
        (classifyFunBindWrapper wrapper1)
        (classifyFunBindWrapper wrapper2)
    WpFun _ resultWrapper _ ->
      FunBindGivens
        { funBindMatchGivens = []
        , funBindRhsOnlyGivens = rhsWrapperGivenEvidence resultWrapper
        }
    WpCast _ -> memptyFunBindGivens
    WpEvLam evVar ->
      FunBindGivens
        { funBindMatchGivens = [evVar]
        , funBindRhsOnlyGivens = []
        }
    WpEvApp _ -> memptyFunBindGivens
    WpTyLam _ -> memptyFunBindGivens
    WpTyApp _ -> memptyFunBindGivens
    WpLet _ -> memptyFunBindGivens
#if !MIN_VERSION_ghc(9,14,0)
    WpMultCoercion _ -> memptyFunBindGivens
#endif

memptyFunBindGivens :: FunBindGivens
memptyFunBindGivens =
  FunBindGivens
    { funBindMatchGivens = []
    , funBindRhsOnlyGivens = []
    }

combineFunBindGivens :: FunBindGivens -> FunBindGivens -> FunBindGivens
combineFunBindGivens givens1 givens2 =
  case (givens1, givens2) of
    ( FunBindGivens
        { funBindMatchGivens = matchGivens1
        , funBindRhsOnlyGivens = rhsOnlyGivens1
        }
      , FunBindGivens
          { funBindMatchGivens = matchGivens2
          , funBindRhsOnlyGivens = rhsOnlyGivens2
          }
      ) ->
        FunBindGivens
          { funBindMatchGivens = matchGivens1 <> matchGivens2
          , funBindRhsOnlyGivens = rhsOnlyGivens1 <> rhsOnlyGivens2
          }

rhsWrapperGivenEvidence :: HsWrapper -> [EvVar]
rhsWrapperGivenEvidence wrapper =
  case wrapper of
    WpHole -> []
    WpCompose wrapper1 wrapper2 ->
      rhsWrapperGivenEvidence wrapper1 <> rhsWrapperGivenEvidence wrapper2
    WpFun _ resultWrapper _ ->
      rhsWrapperGivenEvidence resultWrapper
    WpCast _ -> []
    WpEvLam evVar -> [evVar]
    WpEvApp _ -> []
    WpTyLam _ -> []
    WpTyApp _ -> []
    WpLet _ -> []
#if !MIN_VERSION_ghc(9,14,0)
    WpMultCoercion _ -> []
#endif

collectExpressionChecks :: PluginThings -> [EvVar] -> LHsExpr GhcTc -> [PendingCheck]
collectExpressionChecks pluginThings givens (L loc expr)
  | isUncheckedLiteralApplication pluginThings expr = []
  | otherwise =
      case stripExpression expr of
        NegApp _ literalExpr _ ->
          case stripLocatedExpression literalExpr of
            HsOverLit _ overLit ->
              maybeToList $
                buildLiteralCheck pluginThings givens (locA loc) NegativeLiteral overLit
            _ ->
              collectExpressionChildren pluginThings givens expr
        HsOverLit _ overLit ->
          maybeToList $
            buildLiteralCheck pluginThings givens (locA loc) PositiveLiteral overLit
        _ ->
          collectExpressionChildren pluginThings givens expr

collectExpressionChildren :: PluginThings -> [EvVar] -> HsExpr GhcTc -> [PendingCheck]
collectExpressionChildren pluginThings givens =
  concat . gmapQ (collectChecks pluginThings givens)

collectPatternChecks :: PluginThings -> [EvVar] -> LPat GhcTc -> [PendingCheck]
collectPatternChecks pluginThings givens (L loc pat)
  | isUncheckedLiteralViewPattern pluginThings pat = []
  | otherwise =
      case pat of
        NPat _ overLit negation _ ->
          let polarity =
                case negation of
                  Nothing -> PositiveLiteral
                  Just _ -> NegativeLiteral
           in maybeToList $
                buildLiteralCheck pluginThings givens (locA loc) polarity (unLoc overLit)
        _ ->
          collectPatternChildren pluginThings givens pat

collectPatternChildren :: PluginThings -> [EvVar] -> Pat GhcTc -> [PendingCheck]
collectPatternChildren pluginThings givens =
  concat . gmapQ (collectChecks pluginThings givens)

buildLiteralCheck ::
  PluginThings ->
  [EvVar] ->
  SrcSpan ->
  LiteralPolarity ->
  HsOverLit GhcTc ->
  Maybe PendingCheck
buildLiteralCheck pluginThings givens location polarity overLit =
  case ol_val overLit of
    HsIntegral intLit ->
      Just $
        buildIntegerLiteralCheck
          pluginThings
          givens
          location
          polarity
          (ol_type (ol_ext overLit))
          (il_value intLit)
    HsFractional fracLit ->
      Just $
        buildRationalLiteralCheck
          pluginThings
          givens
          location
          polarity
          (ol_type (ol_ext overLit))
          (SourceText.rationalFromFractionalLit fracLit)
          fracLit
    HsIsString _ _ -> Nothing

buildIntegerLiteralCheck ::
  PluginThings ->
  [EvVar] ->
  SrcSpan ->
  LiteralPolarity ->
  Type ->
  Integer ->
  PendingCheck
buildIntegerLiteralCheck pluginThings givens location polarity targetType literalValue =
  PendingCheck
    { pendingCheckLocation = location
    , pendingCheckGivens = givens
    , pendingCheckOrigin = className literalClass
    , pendingCheckPredicate = mkClassPred literalClass [mkNumLitTy literalValue, targetType]
    }
 where
  PluginThings
    { checkedPositiveIntegerLiteralClass = positiveLiteralClass
    , checkedNegativeIntegerLiteralClass = negativeLiteralClass
    } = pluginThings
  literalClass =
    case polarity of
      PositiveLiteral -> positiveLiteralClass
      NegativeLiteral -> negativeLiteralClass

buildRationalLiteralCheck ::
  PluginThings ->
  [EvVar] ->
  SrcSpan ->
  LiteralPolarity ->
  Type ->
  Rational ->
  SourceText.FractionalLit ->
  PendingCheck
buildRationalLiteralCheck pluginThings givens location polarity targetType rational fracLit =
  PendingCheck
    { pendingCheckLocation = location
    , pendingCheckGivens = givens
    , pendingCheckOrigin = className literalClass
    , pendingCheckPredicate =
        mkClassPred
          literalClass
          [ mkStrLitTy (mkFastString stringRepresentation)
          , mkNumLitTy (abs (Ratio.numerator rational))
          , mkNumLitTy (abs (Ratio.denominator rational))
          , targetType
          ]
    }
 where
  PluginThings
    { checkedPositiveRationalLiteralClass = positiveLiteralClass
    , checkedNegativeRationalLiteralClass = negativeLiteralClass
    } = pluginThings
  literalClass =
    case polarity of
      PositiveLiteral -> positiveLiteralClass
      NegativeLiteral -> negativeLiteralClass
  stringRepresentation =
    case polarity of
      PositiveLiteral -> fractionalLiteralDisplayText rational fracLit
      NegativeLiteral -> fractionalLiteralDisplayText (negate rational) fracLit

loadPluginThings :: TcM PluginThings
loadPluginThings =
  PluginThings
    <$> lookupClass ''CheckedPositiveIntegerLiteral
    <*> lookupClass ''CheckedNegativeIntegerLiteral
    <*> lookupClass ''CheckedPositiveRationalLiteral
    <*> lookupClass ''CheckedNegativeRationalLiteral
    <*> lookupValue 'uncheckedLiteral
 where
  lookupClass quotedName = do
    classModule <- lookupHelperModule (quotedNameModuleName quotedName)
    className' <- lookupOrig classModule (mkTcOcc (TH.nameBase quotedName))
    tcLookupClass className'
  lookupValue quotedName = do
    valueModule <- lookupHelperModule (quotedNameModuleName quotedName)
    lookupOrig valueModule (mkVarOcc (TH.nameBase quotedName))

lookupHelperModule :: ModuleName -> TcM Module
lookupHelperModule moduleName = do
  hscEnv <- getTopEnv
  case lookupModuleWithSuggestions (hsc_units hscEnv) moduleName NoPkgQual of
    LookupFound foundModule _ -> pure foundModule
    _ -> panic "CheckedLiterals.Plugin: failed to resolve helper module"

quotedNameModuleName :: TH.Name -> ModuleName
quotedNameModuleName name =
  case TH.nameModule name of
    Just moduleName -> mkModuleName moduleName
    Nothing ->
      panic $
        "CheckedLiterals.Plugin: quoted helper name is missing a module: "
          ++ TH.pprint name

isUncheckedLiteralApplication :: PluginThings -> HsExpr GhcTc -> Bool
isUncheckedLiteralApplication pluginThings expr =
  case stripExpression expr of
    HsApp _ funExpr _ -> isUncheckedLiteralFunction pluginThings (unLoc funExpr)
    _ -> False

isUncheckedLiteralViewPattern :: PluginThings -> Pat GhcTc -> Bool
isUncheckedLiteralViewPattern pluginThings pat =
  case pat of
    ViewPat _ viewExpr _ -> isUncheckedLiteralFunction pluginThings (unLoc viewExpr)
    _ -> False

isUncheckedLiteralFunction :: PluginThings -> HsExpr GhcTc -> Bool
isUncheckedLiteralFunction pluginThings expr =
  case stripExpression expr of
    HsVar _ name ->
      case pluginThings of
        PluginThings{uncheckedLiteralVarName = uncheckedName} ->
          getNameFromLocatedOcc name == uncheckedName
    _ -> False

stripLocatedExpression :: LHsExpr GhcTc -> HsExpr GhcTc
stripLocatedExpression = stripExpression . unLoc

stripExpression :: HsExpr GhcTc -> HsExpr GhcTc
#if MIN_VERSION_ghc(9,10,0)
stripExpression (HsPar _ innerExpr) =
      stripLocatedExpression innerExpr
#else
stripExpression (HsPar _ _ innerExpr _) =
      stripLocatedExpression innerExpr
#endif
#if MIN_VERSION_ghc(9,10,0)
stripExpression (HsAppType _ funExpr _) =
      stripLocatedExpression funExpr
#else
stripExpression (HsAppType _ funExpr _ _) =
      stripLocatedExpression funExpr
#endif
#if MIN_VERSION_ghc(9,12,0)
stripExpression (XExpr (WrapExpr _ wrappedExpr)) =
  stripExpression wrappedExpr
#else
stripExpression (XExpr (WrapExpr (HsWrap _ wrappedExpr))) =
  stripExpression wrappedExpr
#endif
#if MIN_VERSION_ghc(9,10,0)
stripExpression (XExpr (ExpandedThingTc _ expandedExpr)) =
  stripExpression expandedExpr
#else
stripExpression (XExpr (ExpansionExpr (HsExpanded _ expandedExpr))) =
  stripExpression expandedExpr
#endif
stripExpression (XExpr (HsTick _ innerExpr)) =
  stripLocatedExpression innerExpr
stripExpression (XExpr (HsBinTick _ _ innerExpr)) =
  stripLocatedExpression innerExpr
stripExpression expr =
  expr

#if MIN_VERSION_ghc(9,14,0)
getNameFromLocatedOcc :: LIdOccP GhcTc -> Name
getNameFromLocatedOcc = getName . unLoc
#else
getNameFromLocatedOcc :: LIdP GhcTc -> Name
getNameFromLocatedOcc = getName . unLoc
#endif

emptyInertCompat :: TcM InertSet
#if MIN_VERSION_ghc(9,14,0)
emptyInertCompat = emptyInertSet <$> getTcLevel
#else
emptyInertCompat = pure emptyInert
#endif

#if MIN_VERSION_ghc(9,8,0)
unpackFSCompat :: FastString -> String
unpackFSCompat = unpackFS
#else
unpackFSCompat :: String -> String
unpackFSCompat = id
#endif

fractionalLiteralDisplayText :: Rational -> SourceText.FractionalLit -> String
fractionalLiteralDisplayText rational fracLit =
  case SourceText.fl_text fracLit of
    SourceText sourceText ->
      let sourceTextStr = unpackFSCompat sourceText
       in case sourceTextStr of
            '-' : _ -> sourceTextStr
            _ | rational < 0 -> '-' : sourceTextStr
            _ -> sourceTextStr
    NoSourceText ->
      RatioExtra.showFixedPoint rational
