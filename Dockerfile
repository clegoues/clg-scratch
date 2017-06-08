# Installs GenProg binary to /usr/local/genprog3
FROM ubuntu:16.04
MAINTAINER Chris Timperley "christimperley@googlemail.com"

# install OCaml, OPAM and m4
RUN apt-get update && \
    apt-get install -y opam build-essential jq m4 && \
        echo "yes" >> /tmp/yes.txt && \
	    opam init -y < /tmp/yes.txt && \
	        eval $(opam config env) &&  \
		    opam install -y cil yojson

ADD smallcov /tmp/smallcov

# install smallcov, then remove OCaml and OPAM
ENV PATH "$PATH:/opt/smallcov"
RUN cd /tmp/smallcov && \
    eval $(opam config env) &&  \
        make && \
	    mkdir -p /opt/smallcov && \
	        mv smallcov /opt/smallcov && \
		    apt-get purge -y opam ocaml m4 && \
		        apt-get clean && \
			    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*
VOLUME /opt/smallcov