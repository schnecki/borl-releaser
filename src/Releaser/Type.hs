{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}


module Releaser.Type where


import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State

import           Control.DeepSeq
import           Control.Lens
import qualified Data.Map.Strict            as M
import           Data.Serialize
import           GHC.Generics

import           ML.BORL                    as B
import           SimSim                     as S


mkConfig :: Reader r a -> r -> a
mkConfig = runReader

type ListOfActions = [[Time]]


type StRep = [Double]

data RewardFunction
  = RewardShippedSimple         -- ^ The costs are accumulated from the shipped orders only
  | RewardPeriodEndSimple       -- ^ The costs are accumulated at the end of the period for all orders in the system
  -- TODO: need future reward in BORL! | RewardByReleasedPeriod (M.Map Period [OrderId]) -- ^ The costs are caluclated by all orders released at each period.
  deriving (Generic, Serialize, NFData, Show, Eq, Ord)

type PLTs = M.Map ProductType Time


data St = St
  { _simulation           :: SimSim         -- ^ The simulation itself.
  , _nextIncomingOrders   :: [Order]        -- ^ The incoming orders for next period
  , _rewardFunctionOrders :: RewardFunction -- ^ Defines how to calculate rewards
  , _plannedLeadTimes     :: PLTs          -- ^ Planned lead times currently set
  } deriving (Generic, NFData)
makeLenses ''St

instance Eq St where
  (St sim1 inc1 _ plt1) == (St sim2 inc2 _ plt2) = (sim1,inc1,plt1) == (sim2,inc2,plt2)

instance Ord St where
  compare (St sim1 inc1 _ plt1) (St sim2 inc2 _ plt2) = compare (sim1,inc1,plt1) (sim2,inc2,plt2)

data StSerialisable = StSerialisable
  { _serSimulation           :: SimSimSerialisable
  , _serNextIncomingOrders   :: [Order]
  , _serRewardFunctionOrders :: RewardFunction
  , _serPlannedLeadTimes     :: PLTs
  } deriving (Generic, Serialize, Eq, Ord)
makeLenses ''StSerialisable

type Releaser m a = StateT St m a


serializeSt :: St -> StSerialisable
serializeSt (St ql incOrders rewOrders plts) = StSerialisable (S.toSerialisable ql) incOrders rewOrders plts

deserializeSt :: Release -> Dispatch -> Shipment -> ProcessingTimes -> StSerialisable -> St
deserializeSt rel disp ship procTimes (StSerialisable sim incOrders rewOrders plts) = St (S.fromSerialisable rel disp ship procTimes sim) incOrders rewOrders plts

-- deserializeSt ::
--      [Action SimSim]
--   -> ActionFilter SimSim
--   -> Decay
--   -> ProxyTableStateGeneraliser SimSim
--   -> ProxyNetInput SimSim
--   -> TensorflowModelBuilder
--   -> Release
--   -> Dispatch
--   -> Shipment
--   -> ProcessingTimes
--   -> StSerialisable
--   -> St
-- deserializeSt as aF decay gen inp builder rel disp ship procTimes (StSerialisable ql orders) = St (B.fromSerialisableWith (S.fromSerialisable rel disp ship procTimes) as aF decay gen inp builder ql) orders

