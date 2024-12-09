ARG VARIANT="4.1"
FROM rocker/r-base:${VARIANT}

RUN install2.r --error --skipinstalled  -ncpus -1 \
      devtools \ 
      coda \
      mvtnorm \ 
      devtools \ 
      loo \ 
      dagitty

RUN installGithub.r rmcelreath/rethinking@slim 

