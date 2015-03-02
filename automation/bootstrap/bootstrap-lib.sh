#!/bin/sh

if [ -z "$need_redirect" ]
then
  need_redirect=1
  exec 3>&1; # exec 1>/dev/null
  exec 4>&2; # exec 2>/dev/null
fi

distroot=${distroot:-/usr/local}

msg_debug () {
  [ "$debug_bootstrap" = no ] || message "[DEBUG] $@"
}

msg_info () {
  message "[INFO ] $@"
}

msg_error () {
  message "[ERROR] $@"
}

message () {
  echo "$@" 1>&3 2>&4
}

check_command () {
  for cmd in "$@"
  do
    if ! has_command "$cmd"
    then
      msg_error "unable to find command: \`$cmd'"
      return 1
    fi
  done
}

check_library () {
  for lib in "$@"
  do
    if ! ldconfig -p 2>/dev/null | grep -q "\\b$lib\\b"
    then
      msg_error "unable to find library: $lib"
      return 1
    fi
  done
}

has_command () {
  local ans
  ans=0
  for cmd in "$@"
  do
    if ! run_cmd_quiet command -v "$cmd"
    then ans=1; fi
  done
  return $ans
}

has_file () {
  local ans
  ans=0
  for file in "$@"
  do
    if [ ! -e "$file" ]
    then ans=1; fi
  done
  return $ans
}

deb_update () {
  run_cmd_once "apt-get update" run_cmd_echo apt-get update -qq
}

deb_install () {
  for pkg in "$@"
  do
    if ! run_cmd_quiet dpkg -s "$pkg"
    then
      msg_info "INSTALL[$deb_install_args] $pkg"
      deb_update
      run_cmd_echo apt-get install $deb_install_args -qq --yes --force-yes "$pkg"
    fi
  done
}

rpm_install () {
  for pkg in "$@"
  do
    if ! run_cmd_quiet rpm -qi "$pkg"
    then
      msg_info "INSTALL $pkg"
      run_cmd_echo yum install -y "$pkg"
    fi
  done
}


rpm_install_url () {
  local rpmfile

  if ! run_cmd_quiet rpm -qi "$1"
  then
    rpmfile=$(mktemp) && {
      trap "rm -f \"$rpmfile\"" INT QUIT TERM EXIT
      msg_info "INSTALL $2"
      run_cmd wget -O"$rpmfile" "$2"
      run_cmd rpm -Uvh "$rpmfile"
      run_cmd_quiet rm -f "$rpmfile"
    }
  fi
}

ubuntu_apt_key () {
  if ! apt-key list | grep ^pub | grep -q "$1"
  then run_cmd_echo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys "$1"; fi
}

deb_add_repo () {
  if [ ! -e "/etc/apt/sources.list.d/$1.list" ]
  then
    run_cmd_echo sh -c "echo \"$2\" | tee /etc/apt/sources.list.d/$1.list"
    run_cmd_echo apt-get update -qq
  fi
}

run_installer () {
  local rc
  local distroot
  local buildroot
  rc=0
  distroot=$1; shift 1
  buildroot=$1; shift 1
  if [ -n "$buildroot" ]
  then
    mkdir -p "$buildroot"
    "$1"; rc=$?
  else
    buildroot=$(mktemp -d) && {
      trap "rm -rf \"$buildroot\"" INT QUIT TERM EXIT
      "$1"; rc=$?
      run_cmd_quiet rm -rf "$buildroot"
    }
  fi
  return $rc
}

run_cmd () {
  msg_debug "$ $@"
  "$@"
}

run_cmd_once () {
  local key
  local cmd
  key=$1; shift 1

  if echo "$run_cmd_once" | grep -vq "\\b$key\\b"
  then
    run_cmd_once="$key $run_cmd_once"
    "$@"
  fi
}

run_cmd_quiet () {
  "$@" 1>/dev/null 2>/dev/null
}

run_cmd_echo () {
  run_cmd "$@" 1>&3 2>&4
}

show_self () {
  local extra_vars
  for var in "$@"
  do
    extra_vars="$extra_vars \\\\\n      $var"
  done
  msg_info "using:
  $ env${extra_vars} \\\\
      distroot=\"$distroot\" \\\\
      buildroot=\"$buildroot\" \\\\
      $0"
}

fetch_url () {
  msg_info "FETCH $@"
  if has_command wget
  then wget -O - "$@" 2>&4
  elif has_command curl
  then curl -L "$@" 2>&4
  else
    msg_error "no http client available, aborting"
    return 1
  fi
}
