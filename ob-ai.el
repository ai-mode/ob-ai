;;; ob-ai.el --- AI-mode Org babel integration -*- lexical-binding: t -*-

;; Copyright (C) 2023 Alex (https://github.com/lispython)

;; URL: https://github.com/ai-mode/ai-mode
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: ai, chatgpt, gpt, org-mode, org-babel


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
(require 'ai)
(require 'map)


(defcustom ai-org-babel-backend 'ai--openai--chat-ob-sync-query
  ""
  :group 'ai-mode
  )


(defcustom ai--org-babel-backends
  '(("OpenAI ChatGPT" . ai--openai--chat-ob-sync-query)
    ("OpenAI Completions" . ai--openai--completions-ob-sync-query))
  "An association list that maps query backend to function."
  :type '(alist :key-type (string :tag "Backend name")
                :value-type (symbol :tag "Backend function"))
  :group 'ai-mode)


(defun ai-change-org-babel-backend ()
  ""
  (interactive)
  (let* ((value (completing-read ai--change-backend-prompt (mapcar #'car ai--org-babel-backends))))
    (setq ai-org-babel-backend (cdr (assoc value ai--org-babel-backends)))
    (message (format "AI Org Babel backend is changed to \"%s\"" value))))



(cl-defun ai--openai--completions-ob-sync-query (query &key
                                                       (model ai--openai--completions-model-version)
                                                       (max-tokens ai--openai--default-max-tokens)
                                                       (timeout ai--openai-request-timeout)
                                                       (extra-params nil))
  ""
  (apply 'ai--openai--completions-sync-send-query `(,query :model ,model :timeout ,timeout :max-tokens ,max-tokens :extra-params ,extra-params))
  )

(cl-defun ai--openai--chat-ob-sync-query (query &key
                                                (model ai--openai--chat-model-version)
                                                (max-tokens ai--openai--default-max-tokens)
                                                (timeout ai--openai-request-timeout)
                                                (extra-params nil))
  ""
  (apply 'ai--openai--chat-sync-send-query `(,query :model ,model :timeout ,timeout :max-tokens ,max-tokens :extra-params ,extra-params))
  )


(defvar org-babel-default-header-args:ai '((:results . "raw") (:model . nil) (:preface . nil))
  "Default arguments for evaluating a AI block.")



(defun org-babel-execute:ai (body params)
  "Execute a block of AI prompt in BODY with org-babel header PARAMS.
This function is called by `org-babel-execute-src-block'"
  (let* ((backend-args (list body :extra-params params))
         (model (map-elt params :model))
         (timeout (map-elt params :timeout))
         (max-tokens (map-elt params :max-tokens))
         (backend (map-elt params :backend)))

    (when model
      (setq backend-args (append backend-args `(:model ,model))))
    (when timeout
      (setq backend-args (append backend-args `(:timeout ,timeout))))
    (when max-tokens
      (setq backend-args (append backend-args `(:max-tokens ,max-tokens))))

    (if backend
        (if (member (intern backend) (mapcar #'cdr  ai--org-babel-backends))
            (apply (intern backend) backend-args)
          (message (format "Backend not found in backends list: %s" backend)))
      (apply ai-org-babel-backend backend-args))))


(provide 'ob-ai)

;;; ob-ai.el ends here
