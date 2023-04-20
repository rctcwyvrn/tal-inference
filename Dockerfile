FROM rust:1.68.2

WORKDIR /usr/src/typechecker
COPY . .

RUN cargo install --path .

CMD ["typechecker"]