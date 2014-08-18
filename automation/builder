#!/usr/bin/env bash
# Script for Leela bootstrap using Ansible.
# Author: Stephano Ferreira <stephano.ferreira@locaweb.com.br>

# PARAMETERS
basedir=$(dirname $0)

inventory_file="${basedir}/ansible/hosts"
native_playbook_file="${basedir}/ansible/native.yml"
docker_playbook_file="${basedir}/ansible/docker.yml"

# FUNCTIONS

main() {
  run_based_on_arguments $1 $2
}

run_based_on_arguments() {
  program_exists "ansible-playbook"

  if [ $1 = "-e" ] && [ $2 = "native" ]; then
    build_native
  elif [ $1 = "-e" ] && [ $2 = "docker" ]; then
    build_docker
  else
    error "You must specify the environment to be bootstrapped." \
      "Use argument '-e' passing 'native' or 'docker'."
  fi
}

build_native() {
  msg "\e[34m Bootstrapping Leela"
  ansible-playbook -i ${inventory_file} -v ${native_playbook_file}

  if [ ! $? -eq 0 ]; then
    error "Something happened while building Leela. D:"
  else
    success "Leela was successfully built. Big win!"
  fi
}

build_docker() {
  msg "\e[34m Building Docker"
  ansible-playbook -i ${inventory_file} -v ${docker_playbook_file}

  if [ ! $? -eq 0 ]; then
    error "Something happened while building Leela in the Docker environment. D:"
  else
    success "Leela was successfully built in the Docker environment. Big win!"
  fi
}

# HELPERS

program_exists() {
  type $1 > /dev/null 2>&1

  if [ ! $? -eq 0 ]; then
    error "To run the bootstrapper, you must have ${1} installed"
  else
    success "${1} is in da house"
  fi
}

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

# EXECUTION

main $1 $2
