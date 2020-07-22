#!/usr/bin/env bash
# https://docs.docker.com/engine/security/https/#create-a-ca-server-and-client-keys-with-openssl
set -eu

usage() {
  cat <<'EOM'
Usage: init-certificates [Hio]

Options:

  -H    Server Hostname
  -i    Server IP
  -o    Directory to place created files

EOM
}

die_usage() {
  usage >&2
  exit 64
}

host=
out=

# getopts
while getopts "hH:o:" opt; do
  case ${opt} in
    h)
      usage
      exit 0
      ;;
    H)
      host=$OPTARG
      ;;
    o)
      out=$OPTARG
      ;;
    \?)
      die_usage "Invalid option: $OPTARG"
      ;;
    :)
      die_usage "Invalid option: $OPTARG requires an argument"
      ;;
  esac
done
shift $((OPTIND - 1))

if [[ -z "$host" ]]; then
  die_usage "-H is required"
fi

if [[ ! -d "$out" ]]; then
  die_usage "-o must be a directory"
fi

ca_key=ca_key.pem
server_ext=extfile_server.cnf
client_ext=extfile_client.cnf
server_csr=server.csr
client_csr=client.csr

ca=$out/ca.pem
server_cert=$out/server_cert.pem
server_key=$out/server_key.pem
client_key=$out/client_key.pem
client_cert=$out/client_cert.pem

echo "=> Writing extensions configurations"
cat >"$server_ext" <<EOM
subjectAltName = DNS:$host,IP:127.0.0.1"
extendedKeyUsage = serverAuth
EOM

cat >"$client_ext" <<EOM
extendedKeyUsage = clientAuth
EOM

echo "=> Creating Certificate Authority"
openssl genrsa -aes256 -out "$ca_key" -passout pass:abcde 4096
openssl req -new -x509 -days 365 -key "$ca_key" -sha256 -passin pass:abcde \
  -out "$ca" -subj '/C=US'

echo "=> Creating Server Certificates"
openssl genrsa -out "$server_key" 4096
openssl req -subj "/CN=$host" -sha256 -new -key "$server_key" -out "$server_csr"
openssl x509 -req -days 365 -sha256 -in "$server_csr" -CA "$ca" -CAkey "$ca_key" \
  -CAcreateserial -passin pass:abcde -out "$server_cert" -extfile "$server_ext"

echo "=> Creating Client Certificates"
openssl genrsa -out "$client_key" 4096
openssl req -subj '/CN=client' -new -key "$client_key" -out "$client_csr"
openssl x509 -req -days 365 -sha256 -in "$client_csr" -CA "$ca" -CAkey "$ca_key" \
  -CAcreateserial -passin pass:abcde -out "$client_cert" -extfile "$client_ext"

echo "=> Files created in $out"
ls -l "$out"
