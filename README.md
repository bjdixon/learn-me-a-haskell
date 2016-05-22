#Startup
we use stack in a docker image

##Start docker daemon
```sh
sudo docker daemon -g /home/bjdixon/dev/docker/
```

##Use stack-build image
```sh
sudo docker run -t -i fpco/stack-build
```

##Use another user other than root
```sh
groupadd wheel
useradd USERNAME
passwd USERNAME
usermod -aG wheel USERNAME
echo '%wheel ALL=(ALL) ALL' >> /etc/sudoers
su USERNAME
cd /home
sudo mkdir USERNAME
sudo chown USERNAME USERNAME
cd USERNAME
```

##Use the repl
```sh
stack ghci
```

