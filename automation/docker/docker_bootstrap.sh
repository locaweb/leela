#!/usr/bin/env bash

# PARAMETERS
docker_bin_name="docker"
dockerfile_name="Dockerfile"
leela_path="../../../"

# FUNCTIONS

main() {
  docker_exists
  dockerfile_exists
  create_symlink_for_leela
  build_docker
}

docker_exists() {
  type $docker_bin_name >/dev/null 2>&1
  local ret="$?"

  if [ ! "$ret" -eq '0' ]; then
    error "To run the bootstrapper, you must have Docker installed. :facepalm:"
  else
    success "Docker is in da house."
  fi
}

dockerfile_exists() {
  if [ ! -e $dockerfile_name ]; then
    error "To run the bootstrapper, you must have the shipped Dockerfile. SMH, son."
  else
    success "Dockerfile exists. <3"
  fi
}

create_symlink_for_leela() {
  if [ ! -e bundle/leela ]; then
    ln -s $leela_path bundle/leela
    success "Created symlink for leela."
  else
    success "Symlink for leela already exists, it's ok."
  fi
}

build_docker() {
  make_log_dir
  docker build -t leela . > log/docker_bootstrap.log 2>&1

  if [ ! $? -eq 0 ]; then
    error "Something happened while building docker. D:"
  else
    success "Docker was successfully builded. Big win!"
  fi
}

# HELPERS

success() {
  msg "\e[32m[✔]\e[0m ${1}${2}"
}

error() {
  msg "\e[31m[✘]\e[0m ${1}${2}"
  exit 1
}

msg() {
  printf "%b\n" "$1" >&2
}

make_log_dir() {
  if [ ! -e log ]; then
    mkdir log
  fi
}

# EXECUTION

main
