-- | The core handling for decision files
module Decision
       (
         Decision(..)
       , DecisionType(..)
       , decisionClass
       ) where

import Scoped(Label)
import Condition(Condition,condition)
import Maker(Maker,(@@),(@@@),(@?),(/@@),(<?>),
             checkKey,fetchBool,key,label,firstChild,mapSubForest,number)
import Command(Command,command)

import Control.Applicative((<|>))

-- | What type of decision is this. Controls where it appears in the interface.
data DecisionType = GenericDecision
                  | DeJureLaw
                  | GenderLaw
                  | Law
                  | PlotDecision
                  | SettlementDecision
                  | SuccessionLaw
                  | TargettedDecision
                  | TitleDecision
                  | VassalDecision
                  deriving (Eq,Ord,Show)

decisionTypeM :: Maker DecisionType
decisionTypeM = GenericDecision <$ checkKey "decisions"
                <|> DeJureLaw <$ checkKey "de_jure_laws"
                <|> GenderLaw <$ checkKey "gender_laws"
                <|> Law <$ checkKey "laws"
                <|> PlotDecision <$ checkKey "plot_decisions"
                <|> SettlementDecision <$ checkKey "settlement_decisions"
                <|> SuccessionLaw <$ checkKey "succession_laws"
                <|> TargettedDecision <$ checkKey "targetted_decisions"
                <|> TitleDecision <$ checkKey "title_decisions"
                <|> VassalDecision <$ checkKey "vassal_decisions"
                <?> "decision type"

-- | Any decision in the game
data Decision = Decision {
  decisionType :: DecisionType,
  name :: Label,
  isHighPrio :: Maybe Bool, -- ^ Only relevant for GenericDecision
  potential :: [Condition],
  allow :: [Condition],
  effect :: [Command],
  aiWillDo :: Maybe (Maybe Double,[(Double,[Condition])])
  } deriving (Eq,Ord,Show)

decision :: DecisionType → Maker Decision
decision dt = Decision dt
              <$> label key
              <*> fetchBool @? "is_high_prio"
              <*> mapSubForest condition @@ "potential"
              <*> (concat <$> mapSubForest condition @? "allow")
              <*> mapSubForest command @@ "effect"
              <*> ((,) <$> firstChild number @? "factor"
                   <*> modifier @@@ "modifier") @? "ai_will_do" <?> "ai_will_do"
  where modifier = (,) <$> firstChild number @@ "factor"
                   <*> condition /@@ "factor"

-- | Make an entire block of decisions contained within a @decision_type = { .. }@ block
decisionClass :: Maker [Decision]
decisionClass = do
  dt ← decisionTypeM
  mapSubForest (decision dt)
