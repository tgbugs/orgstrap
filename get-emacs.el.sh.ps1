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
":"; #px function package_manager {
":"; #px     local full;
":"; #px     cmds=(emerge apt yum dnf pacaman nix-env guix brew)
":"; #px     for cmd in "${cmds[@]}"; do
":"; #px         full=$(command -v ${cmd} 2>&1) && break;
":"; #px     done
":"; #px     echo "${cmd}"
":"; #px }
":"; #px function posix_bootstrap {
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
":"; #px         *)       echo No package manager found! Checked emerge apt yum dnf pacaman nix-env guix brew. ;return 1 ;;
":"; #px     esac
":"; #px     return $?
":"; #px     # TODO
":"; #px     # get-yourself-a-real-package-manager
":"; #px     # none -> os detection -> get the right one -> run this again
":"; #px     echo yay posix
":"; #px     # should probably run emacs in here ??
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
