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
