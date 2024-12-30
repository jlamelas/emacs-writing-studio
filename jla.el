;;; ews.el --- Custom variables and funcitons  -*- lexical-binding: t; -*-

;; set the endpoint for the ollama server
(setq jla/ollama-api-endpoint "http://localhost:11434")
(defun jla/get-ollama-models ()
  "Return a list of Ollama models from the API, or nil if the API is not available."
  (when-let*
      ((url (url-generic-parse-url jla/ollama-api-endpoint))
       (host (url-host url))
       (port (url-port url))
       ;; Use short timeout with curl to assure API port is open
       (maybe-ollama-models (s-split "\n"
                                     (shell-command-to-string (concat "curl -s --connect-timeout 0.5 '"
                                                                      jla/ollama-api-endpoint "/api/tags' | jq -r '.models[].name'"))))
       (my-ollama-models (seq-filter (lambda (s) (not (string= s "")))
                                     maybe-ollama-models)))
    ;; (gptel-make-ollama
    ;;  "Ollama"
    ;;  :host (concat host ":" (number-to-string port))
    ;;  :models my-ollama-models
    ;;  :stream nil)
    my-ollama-models))

;; set globally, as ollama-models is relevant to more than gptel
(setq ollama-models (jla/get-ollama-models))

;; functions to insert backend and model in responses buffers
(defun jla/gptel-backend-and-model ()
  "Return gptel backend and model (if any)."
  (let ((backend (if  (boundp 'gptel-backend)  (aref gptel-backend 1)))
        (model (if  (boundp 'gptel-model) gptel-model)))
    (format "(%s %s)" backend model)))

(defun jla/gptel-insert-model-in-non-gptel-buffers ()
  "This function will add the backend and model in the \"dynamic\" buffers, not in dedicated chat buffers.
To be used in `gptel-pre-response-hook'."
  (unless (member 'gptel-mode local-minor-modes)
    (goto-char (point-max))
    (insert (format "\n%s: " (jla/gptel-backend-and-model)))
    (goto-char (point-max))))
(add-hook 'gptel-pre-response-hook 'jla/gptel-insert-model-in-non-gptel-buffers)

(defun jla/gptel-insert-model-in-chat-buffers (response-begin-pos response-end-pos)
  "This function adds the backend and model in dedicated chat buffers.
Can be used with the `gptel-post-response-functions' hook."
  (let* ((inserted-string (format "%s %s\n" (alist-get 'org-mode gptel-prompt-prefix-alist) (jla/gptel-backend-and-model)))
         (len-inserted (length inserted-string )))
    (goto-char response-begin-pos)
    (insert inserted-string)
    (goto-char (+ response-end-pos len-inserted))))
(add-hook 'gptel-post-response-functions 'jla/gptel-insert-model-in-chat-buffers)

;; get the gemini api key
(defvar gptel-api-key-file (expand-file-name "~/.emacs.d/gemini-api-key.txt")
  "Ficheiro que contén a chave API de Google Gemini.")

(defun gptel-get-api-key ()
  "Lé a chave API de Google Gemini dende o ficheiro seguro."
  (with-temp-buffer
    (insert-file-contents gptel-api-key-file)
    (buffer-string)))

(defvar gptel-api-key #'gptel-get-api-key
  "Función que devolve a chave API de Google Gemini.")
