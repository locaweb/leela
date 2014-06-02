#!/bin/sh

exec 3>&1
exec 1>&2

wl_chkbin () {
  if ! command -v "$1" 2>/dev/null >/dev/null
  then
    echo "ERROR: missing program: $1"
    return 1
  fi
}

wl_cpu_scale () {
  local tick scale

  if ! wl_chkbin "bc"
  then return 1; fi

  if ! wl_chkbin "grep"
  then return 1; fi

  if ! wl_chkbin "getconf"
  then return 1; fi

  tick=$(getconf CLK_TCK)
  if ! echo "$tick" | grep -q "^[0-9][0-9]*$"
  then
    echo "**********************************************"
    echo "WARNING: could not read CLK_TCK [assuming 100]"
    echo "**********************************************"
    return 2
  fi

  if [ "$tick" -eq 100 ]
  then
    return 2
  fi

  scale=$(echo "scale=6; 100 / $tick" | bc)
  cat <<EOF >&3
LoadPlugin "match_regex"
LoadPlugin "target_scale"

<Chain "PreCache">
  <Rule "cpu_rel">
    <Match "regex">
      Plugin "^cpu$"
    </Match>
    <Target "scale">
      Factor $scale
    </Target>
  </Rule>
</Chain>
EOF
}

wl_cpu_scale
