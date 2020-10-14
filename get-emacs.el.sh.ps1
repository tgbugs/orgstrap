#!/usr/bin/env bash
":"; # -*-emacs-lisp-*-
":"; function posix () {
":";   eval "$(grep '^":";\ #px' $0 | sed -e 's/^":";\ #px //')"
":";   return "$?"
":"; }
":"; function quirk () {
":";   test "$ZSH_VERSION" && set +nofunctionargzero
":"; }
":"; function unquirk () {
":";   test "$ZSH_VERSION" && set nofunctionargzero
":"; }
":"; "quirk"
":"; "posix"
":"; "unquirk"
":"; "exit"  # NOTE can't pass error code out easily here
":"; ((Get-Content $MyInvocation.MyCommand.Source) -match '^":";\ #ps ' -replace '^":";\ #ps ') -join "`n" | Invoke-Expression
":"; exit
":"; #px echo "Bootstrapping in posix mode."
":"; #px scurl () {
":"; #px     # safe(r) curl, yet still scurrilous thus scurl
":"; #px     # example: scurl my-audited-checksum https://example.org/file.ext /tmp/file.ext
":"; #px     local CYPHER="${1}"
":"; #px     local CHECKSUM="${2}"
":"; #px     local URL="${3}"
":"; #px     local path="${4}"
":"; #px     local dname="$(dirname "${path}")"
":"; #px     local fname="$(basename "${path}")"
":"; #px     if [ $CYPHER != sha256 ]; then
":"; #px         # TODO cypher command ...
":"; #px         echo "Unsupported cypher ${CYPHER}"
":"; #px         return 1
":"; #px     fi
":"; #px     if [ -f "${path}" ]; then
":"; #px         echo "$(sha256sum "${path}" 2>/dev/null || shasum -a 256 "${path}")" | \
":"; #px         awk '$1!="'"${CHECKSUM}"'" { exit 1 }'
":"; #px         local CODE=$?
":"; #px         if [ $CODE -ne 0 ]; then
":"; #px             echo failed with $CODE
":"; #px             echo "${path}" existing checksum does not match new checksum.
":"; #px             local efail_path="$(mktemp -p "${dname}" "${fname}"._existing_failure_.XXXXXX)"
":"; #px             mv "${path}" "${efail_path}"
":"; #px         else
":"; #px             return 0
":"; #px         fi
":"; #px     fi
":"; #px     local temp_path="$(mktemp -p "${dname}" "${fname}"._fetching_.XXXXXX)"
":"; #px     #cmd="$(command -v sha256sum 2>/dev/null || (shasum -a 256))"
":"; #px     # too many ways a streaming check can go wrong that we can't handle correclty
":"; #px     # set -o pipefail is not portable
":"; #px     #curl --location "${URL}" | tee "${temp_path}" | ${cmd} || return $?
":"; #px     curl --location "${URL}" --output "${temp_path}" || return $?
":"; #px     echo "$(sha256sum "${temp_path}" 2>/dev/null || shasum -a 256 "${temp_path}")" | \
":"; #px     awk '$1!="'"${CHECKSUM}"'" { exit 1 }'
":"; #px     local CODE=$?
":"; #px     if [ $CODE -ne 0 ]; then
":"; #px         echo failed with $CODE
":"; #px         echo "${temp_path}" checksum did not pass! something evil is going on!
":"; #px         local fail_path="$(mktemp -p "${dname}" "${fname}"._checksum_failure_.XXXXXX)"
":"; #px         mv "${temp_path}" "${fail_path}"
":"; #px     else
":"; #px         mv "${temp_path}" "${path}"
":"; #px     fi
":"; #px }
":"; #px function install_homebrew {
":"; #px     echo "Not implemented yet!"
":"; #px     return 1
":"; #px     local path_install_sh
":"; #px     path_install_sh="$(mktemp -d)/install.sh"  # FIXME
":"; #px     scurl sha256 "45c12bcd7765986674142230fc0860f5274903adbe37d582e5537771a1bae0b8" "https://raw.githubusercontent.com/Homebrew/install/fea1e80dd6c80ff0ac64e0e78afa387179f08660/install.sh" "${path_isntall_sh}"
":"; #px     /usr/bin/env bash "${path_install_sh}"
":"; #px     # TODO cleanup after ourselves.
":"; #px }
":"; #px function package_manager {
":"; #px     local full;
":"; #px     cmds=(emerge apt yum dnf pacaman nix-env guix brew)
":"; #px     for cmd in "${cmds[@]}"; do
":"; #px         full=$(command -v ${cmd} 2>&1) && break;
":"; #px     done
":"; #px     echo "${cmd}"
":"; #px }
":"; #px function posix_bootstrap {
":"; #px     local nopm
":"; #px     local cmd
":"; #px     cmd=$(package_manager)
":"; #px     case $cmd in
":"; #px         emerge)  $cmd app-editors/emacs ;;
":"; #px         yum)     $cmd -y install emacs-nox ;;
":"; #px         dnf)     $cmd -y install emacs-nox ;;
":"; #px         pacman)  $cmd --noconfirm -S emacs-nox ;;
":"; #px         apt)     $cmd -y install emacs-nox ;;
":"; #px         nix-env) $cmd -i emacs-nox ;;
":"; #px         guix)    $cmd install emacs-nox ;;
":"; #px         brew)    $cmd cask install emacs ;;
":"; #px         *)       nopm=1; echo No package manager found! Checked emerge apt yum dnf pacaman nix-env guix brew. ;;
":"; #px     esac
":"; #px     local CODE=$?
":"; #px     # none -> os detection -> get the right one -> run this again
":"; #px     if [ -n "${nopm}" ]; then
":"; #px         if [ ${OSTYPE%-*} = darwin ]; then
":"; #px             install_homebrew && posix_bootstrap
":"; #px             CODE=$?
":"; #px         else
":"; #px             echo "Don't know how to install a package manager for ${OSTYPE}."
":"; #px             CODE=1
":"; #px         fi
":"; #px     fi
":"; #px     # should probably run emacs in here ??
":"; #px     return $CODE
":"; #px }
":"; #px function missing_emacs {
":"; #px     echo "Emacs missing, preparing to bootstrap."
":"; #px     local BCMD="$(typeset -f package_manager); $(typeset -f posix_bootstrap); posix_bootstrap"
":"; #px     if command -v sudo; then
":"; #px         sudo "$0" -c "${BCMD}" || exit $?
":"; #px     else
":"; #px         echo For su on ${HOSTNAME} 1>& 2;
":"; #px         su -c "${BCMD}" || exit $?
":"; #px     fi
":"; #px }
":"; #px # FIXME sort out the argument passing
":"; #px ( echo "${EMACS}" | grep -q "term" ) && EMACS=emacs || EMACS=${EMACS:-emacs}
":"; #px command -v $EMACS >/dev/null || missing_emacs &&
":"; #px $EMACS --no-site-file --script "$0" -- "$@"
":"; #px CODE=$?
":"; #px exit $CODE
":"; #ps Write-Host "Bootstrapping in windows mode."
":"; #ps $EMACS = if ($Env:EMACS -eq $null) { "emacs" } else { $Env:EMACS }
":"; #ps function bootstrap-windows {
":"; #ps     if (-not (Get-Command $EMACS -errorAction SilentlyContinue)) {
":"; #ps         Write-Host "Emacs missing, preparing to bootstrap."
":"; #ps         if (-not (Get-Command choco -errorAction SilentlyContinue)) {
":"; #ps             Write-Host "Chocolatey missing, preparing to bootstrap."
":"; #ps             Write-Host "Install chocolatey? [y/N]"
":"; #ps             if ('y', 'Y' -contains $Host.UI.RawUI.ReadKey("NoEcho,IncludeKeyDown").Character) {
":"; #ps                 Set-ExecutionPolicy AllSigned -Scope Process -Force;
":"; #ps                 [System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072;
":"; #ps                 Invoke-Expression ((New-Object System.Net.WebClient).DownloadString('https://chocolatey.org/install.ps1'))
":"; #ps                 } else { Write-Host "Not installing chocolatey. If you want to continue you can install emacs manually.";
":"; #ps                     throw "failed" } }
":"; #ps         choco install $EMACS } }
":"; #ps # FIXME may also need to use $MyInvocation.MyCommand.Path .Name
":"; #ps try {
":"; #ps     bootstrap-windows
":"; #ps     emacs --script $MyInvocation.MyCommand.Source -- $args
":"; #ps } finally {
":"; #ps     exit
":"; #ps }
(message "We have Emacs.")
