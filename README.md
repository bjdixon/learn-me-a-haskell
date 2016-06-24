#Startup
we use stack in a docker image

##Use stack-build image
```sh
sudo docker build -t="hsk" ./docker/
```

##Login to container
```sh
sudo docker run -t -i -u="haskellUser" -w="/home/haskellUser" hsk
```

##Use the repl
```sh
stack ghci
```

