{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}


module Releaser.Type where


import           Control.Monad.Trans.State

import           Control.Lens
import           Data.Serialize
import           GHC.Generics

import           ML.BORL
import           SimSim


data St = St { _simulation :: BORL SimSim
             -- , _todo
             }

data StSerialisable = StSerialisable { _serSimulation :: BORLSerialisable SimSim

                                     } deriving (Generic, Serialize)


serializeSt :: St -> BORLSerialisable SimSim
serializeSt (St sim) = (toSerialisable sim)

deserializeSt ::
     [Action SimSim] -> ActionFilter SimSim -> Decay -> ProxyTableStateGeneraliser SimSim -> ProxyNetInput SimSim -> TensorflowModelBuilder -> StSerialisable -> St
deserializeSt as aF decay gen inp builder (StSerialisable borl) = St $ (fromSerialisable as aF decay gen inp builder borl)


type Releaser = StateT
