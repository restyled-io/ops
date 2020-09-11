# promote

Tooling for publishing, testing, and promoting Restylers manifests.

## Context

*TODO*

## Usage

```console
% promote --help
Usage: promote [--file PATH] [--test TESTNAME] [--image IMAGE] [--debug] [--yes]
               FROM [TO]
  Test and promote a Restylers set

Available options:
  --file PATH              Upload PATH as FROM first
  --test TESTNAME          Test to run
  --image IMAGE            Restyler image to test with
  --debug                  Run Restyler with DEBUG=1
  --yes                    Confirm overwriting of TO
  FROM                     Channel to promote from
  TO                       Channel to promote to
  -h,--help                Show this help text
```

## Examples

Upload a locally-written `restylers.yaml` to a Channel named `dev`:

```console
promote --file /tmp/restylers.yaml dev
```

Run integration tests on a Channel named `dev`, specifying a different image:

```console
promote --image restyled/restyler:beta dev
```

Promote the `dev` Channel manifest to `stable`:

```console
promote --yes dev stable
```

Show the `diff` (to `stable`) for the promotion above, but don't do it:

```console
promote dev stable
```

---

[LICENSE](../LICENSE)
