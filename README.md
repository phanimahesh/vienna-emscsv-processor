# emscsv-processor

Monitors a file server for csv-like files containing energy measurements, and pushes the data into a FIWARE context broker.

## Usage
This is intented to be used by internal services of city of vienna, and makes many assumptions. Specifically, the file formats, simple html page listing on file server, specific url structures and certificate based access. It may not be usable as-is outside its intended environments.

Most configurable options can be controlled with environment variables. For a complete list, see `src/Config.hs`. Each field of `APP` maps to an env var obtained by converting camel case to upper snake case. For example, `fiwareService` becomes `FIWARE_SERVICE`

## Minimal docker image
To build a minimal docker image, run

```
cd docker/minimal
make
```

`stack` is required on the host system to build the minimal image.
