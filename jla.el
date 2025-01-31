;;; ews.el --- Custom variables and funcitons  -*- lexical-binding: t; -*-

(defgroup jla ()
  "Group for JLA configurations."
  :group 'file)

(defgroup jla-gptel-options nil
  "Configuration options for gptel."
  :group 'jla)

(defcustom jla/gptel-user-prefix "Usuario: "
  "User prefix for gptel prompts."
  :group 'jla-gptel-options
  :type 'string)

;; set the endpoint for the ollama server
(defcustom jla/ollama-api-endpoint "http://localhost:11434"
  "The URL for ollama server API endpoint."
  :group 'jla-gptel-options
  :type 'string)

(defun jla/get-ollama-models ()
  "Return a list of Ollama models from the API, or nil if the API is not available."
  (when-let*
      ((url (url-generic-parse-url jla/ollama-api-endpoint))
       (host (url-host url))
       (port (url-port url))
       ;; Use short timeout with curl to assure API port is open
       (maybe-ollama-models (split-string (shell-command-to-string
					   (concat "curl -s --connect-timeout 0.5 '"
						   jla/ollama-api-endpoint "/api/tags' | jq -r '.models[].name'")) "\n" t))
       (my-ollama-models (seq-filter (lambda (s) (not (string= s "")))
                                     maybe-ollama-models)))
    my-ollama-models))

;; set globally, as ollama-models is relevant to more than gptel
(setq ollama-models (jla/get-ollama-models))

(defun jla/get-github-models ()
  "Return a list of GitHub models from the API, nil if the API is not available."
  (when-let*
      ((maybe-github-models (split-string (shell-command-to-string
                                           (concat "curl -X GET -s --connect-timeout 0.5 'https://models.inference.ai.azure.com/models' | jq -r '.[].name'")) "\n" t))
       (my-github-models (seq-filter (lambda (s) (not (string= s "")))
                                     maybe-github-models)))
    my-github-models))

;; functions for terminals
(defun jla/get-terminal-command ()
  "Devolve o comando para abrir a terminal dependendo do sistema operativo."
  (cond
   ((eq system-type 'gnu/linux) 'vterm)
   ((eq system-type 'windows-nt) 'term)
   (t 'term))) ; Default

(defun jla/toggle-terminal-bottom ()
  "Toggle terminal buffer in a window tha occupies the bottom third of the frame."
  (interactive)
  (let* ((terminal-comand (jla/get-terminal-command))
         (terminal-buffer-name (concat "*" (symbol-name terminal-comand) "*"))
         (existing-window nil))
  (dolist (window (window-list))
    (when (equal (buffer-name (window-buffer window)) terminal-buffer-name)
      (setq existing-window window)))
  (if existing-window
      (delete-window existing-window)
    (let ((window (split-window (frame-root-window)
                                (floor (/ (frame-height) 3))
                                'below)))
      (select-window window)
      (let ((buffer (get-buffer-create terminal-buffer-name)))
        (with-current-buffer buffer
          (funcall terminal-comand))
        (set-window-buffer window buffer))))))

;; global `key-binding' for toggle terminal
(global-set-key (kbd "C-c v t") 'jla/toggle-terminal-bottom)
