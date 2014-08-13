#!/usr/bin/env bash

# PARAMETERS
basedir=$(dirname $0)
docker_bin_name="docker"
dockerfile="${basedir}/Dockerfile"
leela_path="${basedir}/../../../leela"
bundle_path="${basedir}/bundle"

# FUNCTIONS

main() {
  docker_exists
  dockerfile_exists
  create_tarball_for_leela_repo
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
  if [ ! -e $dockerfile ]; then
    error "To run the bootstrapper, you must have the shipped Dockerfile. SMH, son."
  else
    success "Dockerfile exists. <3"
  fi
}

create_tarball_for_leela_repo() {
  tar -czf ${bundle_path}/leela.tar.gz $leela_path --exclude=".git" \
    --exclude=".gitignore" --exclude=".gitmodules" > /dev/null 2>&1

  if [ ! $? -eq 0 ]; then
    error "Something happened while creating the tarball for Leela."
  else
    success "Created tarball for the Leela repository."
  fi
}

build_docker() {
  make_log_dir
  docker build -t leela ${basedir} > ${basedir}/log/docker_bootstrap.log 2>&1

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
  if [ ! -e ${basedir}/log ]; then
    mkdir ${basedir}/log
  fi
}

# EXECUTION

main
