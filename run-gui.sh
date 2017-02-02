docker run -it -v $(pwd)/src:/scala/akkacrawl:rw -v /tmp/.X11-unix:/tmp/.X11-unix -v $HOME/.Xauthority:/home/dev/.Xauthority -e DISPLAY=$(ifconfig en0 | grep inet | awk '$1=="inet" {print $2}'):0 --env="QT_X11_NO_MITSHM=1" --net=host --privileged akkaidea