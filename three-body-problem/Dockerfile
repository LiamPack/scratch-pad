FROM owlbarn/owl:latest
USER root

WORKDIR build/
COPY src/ src/
COPY test/ test/

RUN opam update \
    && opam install dune

RUN opam install ounit owl owl-plplot owl-ode # i wish i didn't have to do this manually
RUN eval $(opam env) \
    && dune external-lib-deps --missing @default
RUN eval $(opam env) && dune build

RUN eval $(opam env) && dune exec test/run_tests.exe

ENTRYPOINT [ "/bin/sh", "dune",  "exec", "test/run_tests.exe" ]
