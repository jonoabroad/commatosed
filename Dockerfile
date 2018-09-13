FROM node:8

RUN    npm install npm@latest            \        
    && npm install elm                   \          
    && npm install elm-test@0.19.0-beta8 \      
    && npm install elm-verify-examples   \
    && npm install elm-format            \          
    && npm install elm-live              \
    &&  echo 'alias ll="ls -lh"' >> ~/.bashrc
VOLUME ["/workspace"] 
WORKDIR /workspace   
ENV PATH="/node_modules/.bin:${PATH}"
CMD ["/bin/bash"]
