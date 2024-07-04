#!/usr/bin/env bash

if ! type dirname > /dev/null 2>&1; then
    echo "Not even a linux or macOS, Windoze? We don't support it. Abort."
    exit 1
fi

. "$(dirname "$0")"/../common/common.sh

init_with_root_or_sudo "$0"

SCRIPT_ABS_PATH=$(turn_to_absolute_path $0)

begin_banner "Top level" "project deploy - packing"

set +u
[[ -e $HOME/.nix-profile/etc/profile.d/nix.sh ]] && . $HOME/.nix-profile/etc/profile.d/nix.sh
set -u

if [ -e ${SCRIPT_ABS_PATH}/../../result ]; then
  [[ -e ${SCRIPT_ABS_PATH}/../../webdriver_w3c.tar.gz ]] && rm -fr ${CRIPT_ABS_PATH}/../../webdriver_w3c.tar.gz
  tar zPcf ${SCRIPT_ABS_PATH}/../../webdriver_w3c.tar.gz $(nix-store --query --requisites ${SCRIPT_ABS_PATH}/../../result)
else
  info "No ${SCRIPT_ABS_PATH}/../../result, can't pack tarball"
fi

done_banner "Top level" "project deploy - packing"
