
# Parameterisation

- Set gamma_1=1.0     (to be tested)
- Set min alpha to something higher? (does not seem to be necessary)
- Learn rho only from last or from all calcs? (currently: only last experience, seems to work)


# Basic Experimental Setup

|                      |                                                                       |
|----------------------+-----------------------------------------------------------------------|
| ANN                  | lenIn -> lenIn -> lenIn/2 -> 4*lenOut -> lenOut                       |
| State Representation | Full                                                                  |
| N-Step               | 5                                                                     |
| BatchSize            | 4                                                                     |
| LearnRandAbove       | 0.5                                                                   |
| # Workers            | 2 (BS = 4*3 = 12)                                                     |
| Scaling              | {(-400, 2000)}                                                        |
| Decay Steps          | 30k, 50k, 60k  ???                                                    |
| Epsilon              | {(0.5, 0.3)}                                                          |
| ReplayMemoryStategy  | ReplayMemoryPerAction                                                 |
| ReplayMemorySize     | 10800                                                                 |
| Overestimate Rho     | False                                                                 |
| Share Rho            | True                                                                  |
| Clip Gradients       | NoClipping                                                            |
|                      |                                                                       |
| Demand-ProcTimes     | 70%: Unif-Exp, Exp-Unif, Exp-Exp; 80%: Unif-Exp, Exp-Unif, Exp-Exp    |
|                      |                                                                       |
| Release              | AvgRewAdjDRL(0.8, 1.0), AvgRewAdjDRL(0.8, 0.99), DQN(0.99), BIL1-BIL4 |
|                      |                                                                       |
|----------------------+-----------------------------------------------------------------------|


# Ideas for Possible Sensitivity Analysis for Base Scenario: 80-Exp-Exp

|                      |                                                                                                     |
|----------------------+-----------------------------------------------------------------------------------------------------|
| State Representation | NBN/BN                                                                                              |
| ANN                  | 2*lenIn -> lenIn -> lenIn/2 -> 4*lenOut -> lenOut                                                   |
| Release              | AvgRewAdjDRL(0.5, 1.0), AvgRewAdjDRL(0.5, 0.99), AvgRewAdjDRL(0.8, 0.995), AvgRewAdjDRL(0.5, 0.995) |
| ShopFloor            | JobShop                                                                                             |
| Costs                | 1:... (siehe paper 1)                                                                               |
| LearningParamsDecay  | NoDecay                                                                                             |
| # Workers            | 0, 4                                                                                                |
| Scaling              | {(-400, 1000), (-400, 1500), (-400, 3000)}                                                          |
| Epsilon              | {(0.5, 0.0), (2.0, 0.3)}                                                                         |
| N-Step               | 3, 10                                                                                               |
| ReplayMemoryStategy  | ReplayMemorySingle                                                                                  |
| ReplayMemorySize     | 21600, 5400, 5                                                                                      |
| Overestimate Rho     | True                                                                                                |
| Share Rho            | False                                                                                               |
| Clip Gradients       | ClipByGlobalNorm 0.01                                                                               |
|----------------------+-----------------------------------------------------------------------------------------------------|
