### Keys & certificates

- `ca.crt` (CA Certificate): Certificate Authority's (CA) certificate. It's used to verify that other certificates are valid.
- `ca.key` (CA Private Key): This is the private key for your CA. It's used to sign certificates.
- `cert.pem` (Server Certificate): This is the server's own certificate, separate from the CA's. It's presented to clients (like browsers or other servers) to prove its identity.