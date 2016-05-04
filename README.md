## profuse

**profuse** is an OCaml implementation of the FUSE protocol versions
 7.8 and 7.23. This protocol version is common to UNIX/GNU Linux, FreeBSD,
 and OS X. Presently, the library only provides the Linux implementation.

### Install

You need to add the ocamllabs opam-repository to have all the right dependencies:

```shell
opam remote add ocl https://github.com/ocamllabs/opam-repo-dev.git
opam install profuse
```

### Use lofs with docker

**Warning:** This is very experimental.

First, prepare your local filesystem:

```shell
mkdir <mnt-dir>  # for instance ~/fuse-mnt
mkdir <data-dir> # for instance ~/fuse-data
```

Note: `<mnt-dir>/` **MUST NOT** be a child of `<data-dir>/`, otherwise
bad things will happen. `<data-dir>/` will contains files mirrored by
`<mnt-dir>/`, and all reads and writes to the `<mnt-dir>/` files will go
through the `lofs` daemon and will be reflected back in `<data-dir>/`.

To "connect" the two directories using the `lofs` daemon, run:

```shell
cd <data-dir> && ./lofs_main.native <mnt-dir>
```

You can test the result by opening a new terminal and modifying things
in `<mnt-dir>`: you will see the `lofs` daemaon spitting out FUSE
requests.

We can now start a docker container and mount `<mnt-dir>` as [data
volume](https://docs.docker.com/userguide/dockervolumes/):

```shell
docker run -it -v <mnt-dir>:/home ubuntu bash
```

If you `cd /home` into the container, you will see the `lofs` daemon
spitting out FUSE requests.
