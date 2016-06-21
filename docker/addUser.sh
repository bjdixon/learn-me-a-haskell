#!/bin/bash
USERNAME="haskellUser"
HOMEDIR="/home/$USERNAME"
useradd $USERNAME
adduser --disabled-password --gecos '' $USERNAME
adduser $USERNAME sudo
echo '%sudo ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers
su - $USERNAME
sudo mkdir $HOMEDIR
sudo chown $USERNAME $HOMEDIR
