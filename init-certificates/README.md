# `init-certificates`

Docker image for producing TLS certificates for use by a Docker server/client.

## Example Usage

Generate certificates:

```console
docker run --rm --volume $PWD:/out \
  restyled/init-certificates -H {hostname} -i {ip} -o /out
```

Start the daemon:

```console
sudo dockerd \
  --tlsverify \
  --tlscacert=ca.pem \
  --tlscert=server_cert.pem \
  --tlskey=server_key.pem \
  -H=0.0.0.0:2376
```

Connect with a client

```console
$ mkdir -pv ~/.docker
$ cp -v {ca,cert,key}.pem ~/.docker
$ sudo chown $USER:$USER ~/.docker/*.pem
$ export DOCKER_TLS_VERIFY=1
$ export DOCKER_HOST=tcp://{hostname or ip}:2376
docker ps
```
