;;; ob-ai.el --- AI-mode Org babel integration -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ob-ai
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (ai-mode "0.1") cl-lib)
;; Keywords: help, tools


;; This file is part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; ob-ai is an extension for org-mode that allows execution of source code blocks via an AI backend.
;; Is it part of ai-mode project.
;;
;; You must set `ai--openai--api-key` to your key before using.
;;

;;; Code:

(require 'ob)
(require 'org)
(require 'map)
(require 'cl-lib)

(require 'ai-mode)


(defgroup ob-ai nil
  "Support for AI interactions."
  :prefix "ai-"
  :group 'ai
  :link '(url-link :tag "Repository" "https://github.com/ai-mode/ob-ai"))


(defcustom ob-ai-org-babel-backend 'ai-perform-sync-backend-query
  "Backend used by org-babel-execute:ai."
  :group 'ob-ai)


(defcustom ob-ai--org-babel-backends
  '(("AI mode sync backend" . ai-perform-sync-backend-query))
  "An association list that maps query backend to function."
  :type '(alist :key-type (string :tag "Backend name")
                :value-type (symbol :tag "Backend function"))
  :group 'ob-ai)


(defun ob-ai-change-org-babel-backend ()
  "Change the current backend."
  (interactive)
  (let* ((value (completing-read ai--change-backend-prompt (mapcar #'car ob-ai--org-babel-backends))))
    (setq ob-ai-org-babel-backend (cdr (assoc value ob-ai--org-babel-backends)))
    (message (format "AI Org Babel backend is changed to \"%s\"" value))))


(defvar org-babel-default-header-args:ai '((:results . "raw") (:preface . nil))
  "Default arguments for evaluating a AI block.")


(defun org-babel-execute:ai (body params)
  "Execute a block of AI prompt in BODY with org-babel header PARAMS.
This function is called by `org-babel-execute-src-block'"
  (let* ((backend-args (list body :extra-params params))
         (backend (map-elt params :backend)))

    (if backend
        (if (member (intern backend) (mapcar #'cdr ob-ai--org-babel-backends))
            (apply (intern backend) backend-args)
          (message (format "Backend not found in backends list: %s" backend)))
      (apply ob-ai-org-babel-backend backend-args))))


(provide 'ob-ai)

;;; ob-ai.el ends here
