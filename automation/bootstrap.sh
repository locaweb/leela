#!/usr/bin/env bash
# Script for Leela bootstrap in Docker using Ansible.
# Author: Stephano Ferreira <stephano.ferreira@locaweb.com.br>

# PARAMETERS
basedir=$(dirname $0)
inventory_file="${basedir}/ansible/production"
master_playbook="${basedir}/ansible/site.yml"

# FUNCTIONS

main() {
  ansible_exists
  execute_master_playbook
}

ansible_exists() {
  type ansible-playbook >/dev/null 2>&1
  local ret="$?"

  if [ ! "$ret" -eq '0' ]; then
    error "To run the bootstrapper, you must have Ansible installed"
  else
    success "Ansible is in da house."
  fi
}

execute_master_playbook() {
  msg "Bootstrapping Leela."
  ansible-playbook -i ${inventory_file} -v ${master_playbook}

  if [ ! $? -eq 0 ]; then
    error "Something happened while bootstrapping Leela. D:"
  else
    success "Leela was successfully bootstrapped. Big win!"
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

# EXECUTION

main
