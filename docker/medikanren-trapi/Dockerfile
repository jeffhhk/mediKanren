FROM racket/racket:8.0
RUN mkdir -p /local
COPY target/medikanren2/.install_pkgs.sh /local/medikanren2/.install_pkgs.sh
RUN bash /local/medikanren2/.install_pkgs.sh
RUN mkdir -p /local/ci_artifacts/status/pass
RUN mkdir -p /local/ci_artifacts/status/fail
#weird error: /usr/bin/raco: Unrecognized command: make
#  RUN bash /local/medikanren2/.compile.sh
COPY target/medikanren2 /local/medikanren2
CMD [ "/usr/bin/racket", "/local/medikanren2/server.rkt" ]
