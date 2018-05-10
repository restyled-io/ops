# Restyled - Ops

Support for the deployment and operation of Restyled infrastructure.

*This uses AWS Cloud Formation; familiarity is assumed.*

## Installation

```console
make
make install
```

Requires [stack](https://docs.haskellstack.org/en/stable/README/). Creates
`~/.local/bin/restyled-ops`, which must be on `$PATH`.

## Usage

See `restyled-ops --help` and `restyled-ops COMMAND --help`.

## Docker

To pull and run the latest version from Docker Hub:

```console
docker run --rm restyled/ops --help
```

To build the image locally:

```console
make image.build
```

To release the built image to Docker Hub:

```console
make image.release
```

Assumes you've already `docker login`-ed to the registry.

## Parameters

Most parameters are self-explanatory, except for:

- **CertificateARN**:

  It's assumed you've already set up a hosted zone for the domain you intended
  to deploy to and created an Amazon-managed certificate. When creating your
  Stack include this `Domain` and the ARN to the certificate in the
  `CertificateARN` parameter.

- **GitHubAppKeyBase64**:

  Since Cloud Formation parameters can't handle newlines, we accept this
  parameter as base64-encoded. For example:

  ```console
  base64 /path/to/private-key.pem | tr -d '\n' | xclip -selection clipboard
  ```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
