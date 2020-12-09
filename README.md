# borl-releaser

Blackwell Optimal Reinforcement Learning

(c) Manuel Schneckenreither. All rights reserved.

## Cloning & Building

Ensure to clone all submodules:

    git clone --recursive git@github.com:schnecki/borl-releaser.git
    sudo pacman -S openmp python
    yay -S cuda-10.2
    sudo mv /opt/cuda /opt/cuda-old
    sudo ln -s /opt/cuda-10.2 /opt/cuda/
    stack build --install-ghc
