#Startup
we use stack in a docker image

##Start docker daemon
```sh
sudo docker daemon -g /home/bjdixon/dev/docker/
```

##Use stack-build image
```sh
sudo docker run fpco/stack-build -t -i
```

##Use the repl
```sh
stack ghci
```

