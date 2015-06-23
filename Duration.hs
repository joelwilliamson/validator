-- | A @Duration@ is used to represent a period of time. This can be used in
-- MTTH, for example.
module Duration
       (
         Duration(..),
         duration
       ) where

import Maker((~@),number,firstChild)
import Control.Applicative((<|>))

-- | A @Duration@ is a period of time.
data Duration = Days Double | Months Double | Years Double deriving (Eq,Ord,Show)
-- | Make a @`Duration`@
duration = Days <$> number ~@ "days"
           <|> Months <$> number ~@ "months"
           <|> Years <$> number ~@ "years"
           <|> Days <$> number ~@ "duration"
