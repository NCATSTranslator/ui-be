# NCATS Translator UI Backend

## Backend Dependencies
### Racket
Install Racket using your system's package manager or get it [here](https://download.racket-lang.org/).

### YAML Library for Racket
`raco pkg install yaml`

## Frontend Dependencies
### Frontend Source
Access to the [frontend repository](https://github.com/dnsmith124/ui-prototype-one).

### Node
Install Node using your system's package manager or get it [here](https://nodejs.org/en/download/).

## Building
### Local Build
Run `./frontend-build.sh` to clone the frontend source and build it.

You can also specify the branch to build the frontend from by adding it as an argument to the script. The default branch is `develop` if no branch is specified.

Example: `./frontend-build.sh my-awesome-branch`

### Deploying to the Server
Run `./build-and-deploy.sh` to clone the frontend source, build it, and deploy to a server. The script assumes two environment variables exist that tell it where to deploy: `TRANSLTR_USER` and `TRANSLTR_HOST`.

You can specifiy the backend and frontend branchs to use in the deployment as arguments to the `build-and-deploy.sh` script.

Example: `./build-and-deploy.sh -b my-awesome-backend-branch -f my-awesome-frontend-branch`

## Running Locally
Run `racket server.rkt` to run the server.

### Configuration
You can configure the the server by creating a YAML configuration file, or by using one of the existing templates in the `configuration` directory. Once you have a configuration you can provide it as a command line argument while running the server.

Example: `racket server.rkt /my/awesome/config/file`

## Running Locally with Docker

- Install the docker enginer for your environment
- `./build-docker-container.sh -b main -f main`
- `docker run --rm -d -p8386:8386 translator-app`
- Point your browser to `http://localhost:8386/`
