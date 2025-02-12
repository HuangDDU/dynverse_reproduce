FROM dynverse/dynwrap_latest:v0.1.0

ARG GITHUB_PAT

COPY definition.yml run.R example.sh /code/

ENTRYPOINT ["/code/run.R"]
