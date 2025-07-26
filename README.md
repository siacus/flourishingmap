# FlourshingMap - The Flourishing Statistics Explorer
This github repo contains the Shiny App code for Flourishing Statistics Explorer related to [The Geography of Human Flourishing](https://github.com/siacus/flourishing-i-challenge) project.

![FlourshingMap Dashboard](FlourishingMap.png)


The application is currently deployed [here](http://140.247.120.209:3838/Flourishing/).

# Deployment on dockerized Shiny Server
We deployed it on a dockerized Shiny server for ARM on a M4 Mac-mini. This is a custom build because
Shiny server is not officially built for ARM hardware.

Please follow to the instructions below at your own risk but consider also reading [these pages](https://github.com/hvalev/shiny-server-arm-docker) from which we took most of it.
As an alternative you can use the official build for `x86_64` by adjusting the `Dockefile` changing the `FROM` statement.

## Build the docker image
Build the image with `--no-cahe` to make sure all libraries are installed

`docker build --no-cache -t shiny-server-arm-full .`

## Run the Shiny server container
Run the container with:

<pre>
bash docker run -d -p 3838:3838 \
  -v ~/shiny-server/apps:/srv/shiny-server/ \
  -v ~/shiny-server/logs:/var/log/shiny-server/ \
  -v ~/shiny-server/conf:/etc/shiny-server/ \
  --name shiny-server shiny-server-arm-full
</pre>

## Move the app
Move `app.R` into the `~/shiny-server/apps/dvhubexplorer` (create it eventually)
Copy the file `.shiny_app.conf` in the same folderas well as the `data` directory.

## Use the dashboard
Point your browser to: `http://your_host_ip:3838/Flourishing/`


