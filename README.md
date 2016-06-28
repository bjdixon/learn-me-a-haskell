#Startup
we use stack in a docker image

##Use fcpo/stack-build image

The following commands will build and run the image/container

```sh
sudo docker build -t="hsk" ./docker/
sudo docker run -t -i -u="haskellUser" -w="/home/haskellUser" hsk
```

Or use the shortcut script that will execute the two commands

```sh
sudo bash hsk.sh
```

##Use the repl

Once logged into the container you can invoke ghci using the following command
```sh
stack ghci
```

