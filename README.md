# borl-releaser

Blackwell Optimal Reinforcement Learning

(c) Manuel Schneckenreither. All rights reserved.

## Cloning & Building

Ensure to clone all submodules:


    git clone --recursive git@git.uibk.ac.at:c4371143/borl-releaser.git
    sudo pacman -S openmp
    yay -S cuda-10.2
    sudo mv /opt/cuda /opt/cuda-old
    sudo ln /opt/cuda-10.2 /opt/cuda/
    cd borl-releaser/borl/tensorflow-haskell
    git checkout 3cd2e15
    cd ../../
    stack build --install-ghc
