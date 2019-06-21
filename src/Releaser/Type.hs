{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Releaser.Type where


import           Control.Monad.Trans.State

import           Control.Lens
import           Data.Serialize
import           GHC.Generics

import           ML.BORL                   as B
import           SimSim                    as S


type StRep = [Double]

data St = St { _simulation :: BORL SimSim
             -- , _todo
             }

data StSerialisable = StSerialisable { _serSimulation :: BORLSerialisable SimSimSerialisable
                                     } deriving (Generic, Serialize)


serializeSt :: St -> StSerialisable
serializeSt (St ql) = StSerialisable (B.toSerialisableWith S.toSerialisable ql)

deserializeSt ::
     [Action SimSim]
  -> ActionFilter SimSim
  -> Decay
  -> ProxyTableStateGeneraliser SimSim
  -> ProxyNetInput SimSim
  -> TensorflowModelBuilder
  -> Release
  -> Dispatch
  -> Shipment
  -> ProcessingTimes
  -> StSerialisable
  -> St
deserializeSt as aF decay gen inp builder rel disp ship procTimes (StSerialisable ql) = St (B.fromSerialisableWith (S.fromSerialisable rel disp ship procTimes) as aF decay gen inp builder ql)


type Releaser = StateT
