;;; blazegraph.el --- Run blazegraph as a subprocess. -*- lexical-binding: t -*-

;; Author: Tom Gillespie
;; Homepage: https://github.com/tgbugs/orgstrap
;; Version: 9999
;; Package-Requires: ((emacs "24.4"))
;; Is-Version-Of: https://raw.githubusercontent.com/tgbugs/orgstrap/master/services/blazegraph.el
;; Reval-Get-Immutable: blazegraph--reval-update

;;;; License and Commentary

;; License:
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ???

;;; Code:

(defvar blazegraph-process-name "*blazegraph-server process*"
  "Name of the process and buffer for blazegraph.")

(defvar blazegraph-port 9999
  "Default port for blazegraph sparql endpoint.")

(defvar blazegraph-dir nil
  "Root folder where the blazegraph")

(defun blazegraph-options (&optional port prefixes journal)
  "Generate alist of blazegraph server options."
  (let ((port (or port blazegraph-port))
        (prefixes (or prefixes "data/prefixes.conf"))
        (journal (or journal "data/blazegraph.jnl")))
    ; this expects to be called with `default-directory' matching
    ; the default directory for the blazegraph process
    ; so it is safe to check relative paths
    (when (and prefixes (not (file-exists-p prefixes)))
      (warning "prefixes file does not exist: %s" prefixes))
    (unless (file-exists-p journal)
      (warning "journal file does not exist: %s" journal))
    `((jetty.port . ,port)
      ;; (log4j.logger.com.bigdata . INFO)
      (com.bigdata.journal.AbstractJournal.file . ,journal)
      (com.bigdata.rdf.sail.sparql.PrefixDeclProcessor.additionalDeclsFile . ,prefixes))))

(defun blazegraph-server-process (&optional port prefixes journal)
  "Start blazegraph server at port PORT."
  (let* ((process-name blazegraph-process-name)
         (buffer (generate-new-buffer process-name))
         (options (blazegraph-options port prefixes journal))
         (options-cli (mapcar (lambda (pair)
                                (format "-D%s=%s" (car pair) (cdr pair)))
                              options)))
    (with-current-buffer buffer
      (let ((argv `(,process-name
                    ,(current-buffer)
                    "java"
                    "-server"
                    "-Xmx4g"
                    ,@options-cli
                    "-jar"
                    "opt/blazegraph.jar")))
        ;;(message "%s" argv)
        (apply #'start-process argv)))))

(defun blazegraph-server-http-ok-p (&optional port)
  "Returns t if if sparql endpoint is up on PORT."
  (let ((port (or port blazegraph-port)))
    (ow-url-head-ok (format "http://localhost:%s/blazegraph/sparql" port))))

(defun blazegraph-server-running-p (&optional port)
  "Returns t if server is running otherwise nil.
Checks for child process, if no process checks sparql endpoint."
  (let ((process (get-process blazegraph-process-name))
        (port (or port blazegraph-port)))
    (or (and process (process-live-p process))
        (blazegraph-server-http-ok-p port))))

(defun blazegraph-create-tab-with-buffer ()
  "Create tab for blazegraph process buffer."
  (when tab-bar-mode
    (tab-bar-new-tab 1)
    (switch-to-buffer blazegraph-process-name)
    (tab-bar-switch-to-prev-tab)))

(defun blazegraph-server-stop ()
  "Stop the blazegraph server."
  (let ((process (get-process blazegraph-process-name)))
    (when process
      (prog1
          (interrupt-process process))
      ;; spin block until the process is dead so that
      ;; Emacs won't prompt when we try to exit
      (while (process-live-p process)
        (sleep-for 0 100)))))

(defun blazegraph-server-start ()
  "Start the blazegraph server."
  (unless (blazegraph-server-running-p)
    (let ((process (if blazegraph-dir
                       (let ((default-directory blazegraph-dir))
                         (blazegraph-server-process))
                     (blazegraph-server-process))))
      (message "Starting Blazegraph ...")
      (while (let ((status (process-status (process-name process))))
               (and (eq status 'run) (not (blazegraph-server-http-ok-p))))
        (sleep-for 0 100)))))

(defun blazegraph-server-restart ()
  "Restart the blazegraph server."
  (blazegraph-server-stop)
  (blazegraph-server-start))

(defun blazegraph--reval-update ()
  "Get the immutable url for the current remote version of this file."
  (reval-get-imm-github "tgbugs" "orgstrap" "services/blazegraph.el"))

(provide 'blazegraph)

;;; blazegraph.el ends here
