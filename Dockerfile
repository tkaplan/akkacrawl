#FROM openjdk:8-jdk-alpine
FROM ubuntu

# Get the python script required for "add-apt-repository"
# Configure the openjdk repo
RUN apt-get update \ 
	&& apt-get install -y software-properties-common \
	&& add-apt-repository ppa:openjdk-r/ppa

# Install OpenJDK 8, X11 libraries, and wget
RUN add-apt-repository ppa:webupd8team/java && apt-get update \
	&& apt-get install -y \ 
		libxext-dev libxrender-dev libxtst-dev \
		openjdk-8-jdk \
		wget \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/* \
	&& rm -rf /tmp/*

# wget IntelliJ IDEA 
COPY ideaIC-2016.3.2.tar.gz /tmp
RUN mv /tmp/ideaIC-2016.3.2.tar.gz /tmp/intellij.tar.gz
RUN mkdir /opt/intellij \
	&& tar -xzf /tmp/intellij.tar.gz -C /opt/intellij --strip-components=1 \
	&& rm -rf /tmp/*

ENV SCALA_HOME /usr/local/share/scala
ENV PATH $PATH:$SCALA_HOME/bin

ENV SCALA_VERSION 2.11.8

RUN apt-get install wget tar bash && \ 
    wget --quiet http://downloads.lightbend.com/scala/$SCALA_VERSION/scala-$SCALA_VERSION.tgz && \
    tar -xf scala-$SCALA_VERSION.tgz && \
    rm scala-$SCALA_VERSION.tgz && \
    mv scala-$SCALA_VERSION $SCALA_HOME

RUN mkdir -p /scala/akkacrawl
WORKDIR /tmp

RUN wget https://dl.bintray.com/sbt/native-packages/sbt/0.13.12/sbt-0.13.12.tgz --no-check-certificate

RUN tar -xvzf sbt-0.13.12.tgz
RUN cp sbt/bin/* /usr/bin

RUN rm -rf sbt-0.13.12.tgz sbt
WORKDIR /scala/akkacrawl

CMD ["/opt/intellij/bin/idea.sh"]