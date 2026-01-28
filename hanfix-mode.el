;;; hanfix-mode.el --- Korean grammar checker -*- lexical-binding: t; -*-

;; Copyright (C) 2026 ntalbs

;; Author: ntalbs <ntalbsen@gmail.com>
;; License: ISC
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2"))
;; Keywords: convenience, wp
;; URL: https://github.com/ntalbs/hanfix-mode

;;; Commentary:
;; This package provides a minor mode to check and correct Korean
;; grammar and spelling using Daum Spell Checker and Google Gemini API.

;;; License:
;; ISC License Copyright (c) 2004-2010 by Internet Systems Consortium, Inc.
;; ("ISC")

;; Copyright (c) 1995-2003 by Internet Software Consortium

;; Permission to
;; use, copy, modify, and /or distribute this software for any purpose with or
;; without fee is hereby granted, provided that the above copyright notice and this
;; permission notice appear in all copies.

;; THE SOFTWARE IS PROVIDED "AS IS" AND
;; ntalbs DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL ntalbs BE
;; LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF
;; CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION
;; WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'url)

(defgroup hanfix nil
  "한국어 맞춤법 검사기 hanfix 설정." ; Korean grammar checker hanfix config
  :group 'editing)

(defcustom hanfix-default-exec-mode 'hanfix
  "Hanfix 기본 실행 모드.
- `hanfix' : hanfix 실행 파일(다음 맞춤법 검사기 호출) 이용.
- `gemini' : Google Gemini 이용." ; Default exec mode for hanfix
  :type '(choice (const :tag "hanfix 실행파일 사용" hanfix)
                 (const :tag "Google Gemini 사용" gemini))
  :group 'hanfix)

(defcustom hanfix-executable-path "hanfix"
  "Hanfix 실행 파일." ; File name of the hanfix executable.
  :type 'string
  :group 'hanfix)

(defcustom hanfix-gemini-api-key ""
  "Gemini API 키 (https://aistudio.google.com/ 에서 발급)." ; Gemini API key
  :type 'string
  :group 'hanfix)

(defcustom hanfix-gemini-model "gemini-3-flash-preview"
  "사용할 Gemini 모델." ; Gemini model to use
  :type '(choice (const :tag "Gemini 3.0 Flash" "gemini-3-flash-preview")
                 (const :tag "Gemini 2.5 Pro" "gemini-2.5-pro")
                 (const :tag "Gemini 2.5 Flash" "gemini-2.5-flash")
                 (const :tag "Gemini 2.5 Flash-Lite" "gemini-2.5-flash-lite"))
  :group 'hanfix)

(defcustom hanfix-max-length-gemini 3000
  "Gemini에 보낼 문자열 최대 길이." ; Maximum length of text to be sent to Gemini
  :type 'number
  :group 'hanfix)

(defcustom hanfix-gemini-api-timeout 180
  "Gemeni API 타임아웃 (초)." ; Timeout for Gemini API in second
  :type 'number
  :group 'hanfix)

(defcustom hanfix-ignore-words '()
  "맞춤법 검사 시 무시할 단어 목록." ; List of ignore words
  :type '(repeat string)
  :group 'hanfix)

;;; --- Faces ---

(defface hanfix-face-section
  '((t :background "gray90" :extend t))
  "검사 중인 단락 강조." ; Face for highlighting currently checking region
  :group 'hanfix)

(defface hanfix-face-error
  '((t :background "salmon" :foreground "white" :weight bold))
  "오류 단어 강조." ; Face for highlighting error word
  :group 'hanfix)

(defface hanfix-face-buffer-section-header
  '((t :background "gray90" :extend t))
  "Hanfix 버퍼 섹션 헤더." ; Face for section header in *Hanfix* buffer
  :group 'hanfix)

(defface hanfix-face-buffer-original-text
  '((t :foreground "red"))
  "Hanfix 버퍼 원문." ; Face for original text in *Hanfix* buffer
  :group 'hanfix)

(defface hanfix-face-buffer-suggestion-text
  '((t :foreground "forestgreen"))
  "Hanfix 버퍼 수정 제안 텍스트." ; Face for suggested text in *Hanfix* buffer
  :group 'hanfix)

;;; --- Internal Utilities ---

(defconst hanfix-buffer "*Hanfix*"
  "Buffer name for hanfix.")

(defconst hanfix-max-length-hanfix 1000
  "Maximum length of text when use hanfix.")

(defconst hanfix-korean-josa-list
  '(;; 3-chars
    "에게는" "에게도" "에게로" "에게만" "에게서" "이라고" "으로서" "으로써"
    ;; 2-chars
    "과는" "까지" "께서" "라도" "라고" "라니" "라서" "만큼" "마저" "밖에"
    "보다" "부터" "에게" "와는" "으로" "이나" "이라" "조차" "처럼" "하고"
    ;; 1-chars
    "가" "과" "께" "나" "는" "도" "랑" "를" "만" "야" "와" "은" "을" "이" "에"

    ;; not postpositions, but can be treaated in similar way.
    ":" ",")
  "List of Korean postpositions (Josa) for word stem extraction.")

(defun hanfix-max-length ()
  "Return maximum length of text based on the current mode."
  (pcase hanfix-default-exec-mode
   ('hanfix hanfix-max-length-hanfix)
   ('genini hanfix-max-length-gemini)
   (_ hanfix-max-length-hanfix)))

(defun hanfix--check-executable ()
  "Check if hanfix executable path is valid."
  (if (not (executable-find hanfix-executable-path))
      (progn
        (warn "hanfix 실행 파일을 찾을 수 없습니다. '%s' 경로를 확인하거나 CLI 도구를 설치하세요."
              hanfix-executable-path)
        nil)
    't))

(defun hanfix--check-gemini-api-key ()
  "Check if the Gemini API key is set."
  (if (string-empty-p hanfix-gemini-api-key)
      (progn
        (warn "'M-x customize-group hanfix'에서 Gemini API 키를 설정하세요")
        nil)
    't))

(defun hanfix--split-korean-josa-word (word)
  "Split WORD into (stem . josa) or (word . nil)."
  (cl-loop for j in hanfix-korean-josa-list
           when (and (> (length word) (length j))
                     (string-suffix-p j word))
           do (let ((stem (substring word 0 (- (length word) (length j)))))
                (when (> (length stem) 0)
                  (cl-return (cons stem j))))
           finally return (cons word nil)))

(defun hanfix--remove-josa (word)
  "Return WORD without its postposition (josa)."
  (car (hanfix--split-korean-josa-word word)))

(defun hanfix--read-char (chars)
  "Read a character from the user, restected to those in CHARS."
  (let ((ch nil))
    (while (not (memq ch chars))
      (clear-this-command-keys)
      (setq ch (read-char-exclusive)))
    ch))

(defun hanfix--recenter (n)
  "Recenter to keep the correction overlay visible upper half of the window.
With a numeric prefix argument N, recenter putting point on screen line N
relative to the selected window."
  (let* ((pos (pos-visible-in-window-p (point) nil t))
         (y-pos (cadr pos))
         (win-height (window-body-height nil t)))
    (when (or (null pos)
              (> y-pos (* win-height 0.5)))
      (recenter n))))

(defun hanfix--show-control-buffer (original suggestions explanation)
  "Display Hanfix control buffer.
The user can see the ORIGINAL text, and list of SUGGESTIONS, and
detailed EXPLANATION."
  (with-current-buffer (get-buffer-create hanfix-buffer)
    (let ((len (length suggestions)))
      (erase-buffer)
      (insert (propertize "교정 제안:\n\n" 'face 'font-lock-keyword-face))
      (insert (propertize " 원문:\n" 'face 'hanfix-face-buffer-section-header))
      (insert "\n    " (propertize original 'face 'hanfix-face-buffer-original-text) "\n\n")
      (insert (propertize " 수정 제안:\n" 'face '(:background "gray90" :extend t)))
      (insert "\n")

      (dotimes (i len)
        (insert (format "(%d) " (1+ i))
                (propertize (aref suggestions i) 'face 'hanfix-face-buffer-suggestion-text)
                "\n"))

      (insert "\n")
      (insert (propertize " 설명:\n" 'face 'hanfix-face-buffer-section-header))
      (let ((filled-help (with-temp-buffer
                           (insert explanation)
                           (fill-region (point-min) (point-max))
                           (buffer-string))))
        (insert "\n" filled-help "\n\n"))
      (if (> len 1)
          (insert " (1-" (format "%d" len)  "/y/n/e/i/q/?):")
        (insert " (1/y/n/e/i/q/?):"))
      (let ((desired-height (+ 2 (count-lines (point-min) (point-max)))))
        (pop-to-buffer hanfix-buffer `((display-buffer-at-bottom) (window-height . ,desired-height)))))))

(defun hanfix--show-help ()
  "Display the keybinding help in the control buffer."
  (with-current-buffer (get-buffer-create hanfix-buffer)
    (erase-buffer)
    (insert (propertize "맞춤법 검사 옵션 (y/n/e/i/q/?) 설명\n" 'face 'font-lock-keyword-face))
    (insert (propertize " y " 'face 'font-lock-string-face) "(yes)   : 제안된 단어로 교체\n")
    (insert (propertize " n " 'face 'font-lock-string-face) "(no)    : 교정하지 않고 다음으로 이동\n")
    (insert (propertize " e " 'face 'font-lock-string-face) "(edit)  : 직접 수정할 내용 입력\n")
    (insert (propertize " i " 'face 'font-lock-string-face) "(ignore): 무시 목록에 추가 및 저장\n")
    (insert (propertize " q " 'face 'font-lock-string-face) "(quit)  : 검사 중단\n")
    (insert (propertize " ? " 'face 'font-lock-string-face) "(help)  : 도움말 표시\n\n")
    (insert "맞춤법 검사로 돌아가려면 '?'를 누르세요.\n")
    (insert "맞춤법 검사를 끝내려면 'q' 또는 'C-g'를 누르세요.\n")
    (set-buffer-modified-p nil)))

(defun hanfix--cleanup-ui ()
  "Remove Hanfix control buffer and its associated window."
  (let ((buf (get-buffer hanfix-buffer)))
    (when (buffer-live-p buf)
      (delete-windows-on buf)
      (kill-buffer buf))))

(defun hanfix--exec-hanfix-bin (text)
  "Call hanfix executable with the provided TEXT.
Return errors as JSON."
  (let* ((json-raw (with-temp-buffer
                     (insert text)
                     (shell-command-on-region (point-min) (point-max) hanfix-executable-path nil t)
                     (buffer-string)))
         (json (condition-case nil
                   (json-parse-string json-raw :object-type 'alist :array-type 'array)
                 (error nil)))
         (errors (cdr (assoc 'errors json))))
    errors))

(defun hanfix--build-prompt (text)
  "Create a prompt text for Gemini using the provided TEXT."
  (let ((ignored (if hanfix-ignore-words
                     (concat "\n단, 다음 단어들은 사용자가 의도한 것이므로 절대 수정하지 마세요!: "
                             (mapconcat 'identity hanfix-ignore-words ", ")))))
    (concat
     "당신은 한국어 맞춤법 및 문법 교정 전문가입니다."
     "아래 제공된 텍스트의 오류를 찾아 교정안을 제시하세요."
     ignored
     "\n\n응답은 반드시 아래와 같은 JSON 형식이어야 합니다."
     "다른 설명은 생략하세요:"
     "\n{\"errors\": [{\"original\": \"틀린부분\", \"suggestions\": [\"교정안1\", \"교정안2\"], \"explanation\": \"이유\"}]}"
     "\n\n텍스트: "
     text)))

(defun hanfix--send-request (text)
  "Prepare request with provided TEXT and send it to Gemini.
Returns the buffer containing the data from Gemini."
  (let* ((model hanfix-gemini-model)
         (api-url (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent" model))
         (url-request-method "POST")
         (api-key (encode-coding-string hanfix-gemini-api-key 'utf-8))
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("x-goog-api-key" . ,api-key)))
         (json-str (json-encode
                    `((contents . [((parts . [((text . ,(hanfix--build-prompt text)))]))])
                      (generationConfig . ((response_mime_type . "application/json"))))))
         (url-request-data (encode-coding-string json-str 'utf-8))
         (response (with-timeout (hanfix-gemini-api-timeout :timeout)
                     (url-retrieve-synchronously api-url))))
    (cond
     ((eq response :timeout)
      (error "Gemini 서버 응답이 너무 늦습니다.  다시 시도해 주세요"))
     ((null response)
      (error "네트워크 연결에 실패했습니다"))
     (t response))))

(defun hanfix--send-request-dummy (_text)
  "Prepare mock response from file."
  (find-file-noselect "/Users/ntalbs/workspace/hanfix-mode/gemini-response.txt"))

(defun hanfix--exec-gemini (text)
  "Call Gemini API with the provided TEXT.
Generate a prompt for TEXT with `hanfix--build-prompt',
then parse the response, and return a vector of errors."
  (let* ((buffer (hanfix--send-request text)))
    (if (not buffer)
        (error "API 호출 중 응답 버퍼를 생성하지 못했습니다")
      (with-current-buffer buffer
        (set-buffer-multibyte t)
        (decode-coding-region (point-min) (point-max) 'utf-8)

        (goto-char (point-min))

        (if (not (re-search-forward "^HTTP/[0-9.]+\\s-+200" nil t))
            (let ((resp-content (buffer-string)))
              (error "Gemini API 호출 실패 (HTTP Error): %s" resp-content))
          (goto-char (point-min))
          (if (not (re-search-forward "^$" nil t))
              (error "API 응답 헤더와 본문을 분리할 수 없습니다")
            (let* ((json-object (json-read))
                   (candidates (cdr (assoc 'candidates json-object)))
                   (first-candidate (aref candidates 0))
                   (content (cdr (assoc 'content first-candidate)))
                   (parts (cdr (assoc 'parts content)))
                   (first-part (aref parts 0))
                   (content-text (cdr (assoc 'text first-part))))
              (if (not content-text)
                  (message "API 응답 본문에 텍스트 데이터가 없습니다.")
                (let* ((parsed-res (json-parse-string content-text
                                                      :object-type 'alist
                                                      :array-type 'array))
                       (errors (cdr (assoc 'errors parsed-res))))
                  errors)))))))))

(defun hanfix--check-errors (text)
  "Call actual grammar checker with TEXT and return errors.
Call either `hanfix--exec-hanfix-bin' or `hanfix--exec-gemini'
based on `hanfix-default-exec-mode'."
  (pcase hanfix-default-exec-mode
    ('hanfix (hanfix--exec-hanfix-bin text))
    ('gemini (hanfix--exec-gemini text))
    (_ (hanfix--exec-hanfix-bin text))))

(defun hanfix--fix-errors (start end errors)
  "Iterate through the ERRORS within the region between START and END.
The user can see the errors, correction suggestions, and detailed explanation,
and decide how to handle the error through control buffer."
  (let ((main-buffer (current-buffer))
        (search-from start)
        (search-until (copy-marker end)))
    (save-excursion
      (cl-loop named main-loop
               for err across errors
               for original = (cdr (assoc 'original err))
               for suggestions = (cdr (assoc 'suggestions err))
               for explanation = (cdr (assoc 'explanation err))
               unless (or (member (hanfix--remove-josa original) hanfix-ignore-words)
                          (string-match-p "\\`[[:ascii:]]*\\'" original))
               do
               (with-current-buffer main-buffer
                 (goto-char search-from)

                 (when (search-forward original search-until t)
                   (setq search-from (point))
                   (let* ((m-data (match-data))
                          (ov (make-overlay (match-beginning 0) (match-end 0)))
                          (s-len (length suggestions))
                          (numeric-keys (number-sequence ?1 (+ ?0 (min 9 s-len))))
                          (valid-keys (append '(?y ?n ?e ?i ?q ??) numeric-keys)))
                     (overlay-put ov 'face 'hanfix-face-error)

                     (with-selected-window (get-buffer-window main-buffer)
                       (goto-char search-from)
                       (hanfix--recenter 10))

                     (hanfix--show-control-buffer original suggestions explanation)

                     (unwind-protect
                         (cl-loop named interaction
                                  for choice = (hanfix--read-char valid-keys)
                                  do
                                  (cl-case choice
                                    (?? (hanfix--show-help)
                                        (when (eq (hanfix--read-char '(?q ??)) ?q)
                                          (cl-return-from main-loop 'quit))
                                        (hanfix--show-control-buffer original suggestions explanation))
                                    ((?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)
                                     (let ((idx (- choice ?1)))
                                       (when (< idx (length suggestions))
                                         (with-current-buffer main-buffer
                                           (set-match-data m-data)
                                           (replace-match (aref suggestions idx))
                                           (undo-boundary)
                                           (cl-return-from interaction)))))
                                    (?y (with-current-buffer main-buffer
                                          (set-match-data m-data)
                                          (replace-match (aref suggestions 0))
                                          (undo-boundary))
                                        (cl-return-from interaction))
                                    (?n (cl-return-from interaction))
                                    (?e (with-current-buffer main-buffer
                                          (set-match-data m-data)
                                          (replace-match (read-string "수정: " (aref suggestions 0)) t t)
                                          (undo-boundary))
                                        (cl-return-from interaction))
                                    (?i (add-to-list 'hanfix-ignore-words (hanfix--remove-josa original))
                                        (cl-return-from interaction))
                                    (?q (cl-return-from main-loop 'quit))))
                       (delete-overlay ov)))))))))

(defun hanfix--process-region (start end)
  "Execute grammar check for the region defined by START and END."
  (let* ((section-ov (make-overlay start end))
         (status 'ok))
    (overlay-put section-ov 'face 'hanfix-face-section)
    (recenter 10)
    (redisplay t)
    (unwind-protect
        (let* ((text (buffer-substring-no-properties start end))
               (errors (hanfix--check-errors text)))
          (if errors
              (when (hanfix--fix-errors start end errors)
                (setq status 'quit))))
      ;; cleanup
      (delete-overlay section-ov)
      (hanfix--cleanup-ui))
    status))

(defun hanfix--get-next-region (start end)
  "Find the optimal region for checking within the region defined by START and END.
The length of the computed region is supposed to be as close as possible,
but not exceed `hanfix-max-length'"
  (save-excursion
    (let* ((current-point start)
           (end-point start)
           (end (if (not end) (point-max) end)))
      (cl-loop while (and (< end-point end) (not (eobp)))
               do
               (setq end-point (save-excursion (forward-paragraph) (point)))

               if (> (- end-point start) (hanfix-max-length))
                   if (= start current-point)
                       return (save-excursion
                                (goto-char (+ start (hanfix-max-length)))
                                (backward-word)
                                (cons start (min (point) end)))
                   else
                       return (cons start (min current-point end))
               else do
                   (forward-paragraph)
                   (setq current-point end-point)
                   (setq end-point (point))

               finally
                   return (cons start end)))))

(defun hanfix--run-loop (start &optional end)
  "Iteratively check text from START to END.
If END is not provided, it will check to the end of the buffer."
  (setq end (if end end (point-max)))
  (save-excursion
    (goto-char start)
    (cl-loop
     ;; 1. skip whitespaces
     do (skip-chars-forward " \t\n\r")

     ;; 2. check if it reached the end
     until (or (eobp) (>= (point) end))

     ;; 3. get next region to check (p-start . p-end)
     for (p-start . p-end) = (hanfix--get-next-region (point) end)


     ;; 4. check the region obtained above. quit the loop if the user selected 'quit
     do (if (eq (hanfix--process-region p-start p-end) 'quit)
            (cl-return)
          (goto-char p-end))))

  ;; 5. cleanup
  (hanfix--cleanup-ui)

  (setq hanfix-ignore-words (sort (delete-dups hanfix-ignore-words) #'string<))
  (customize-save-variable 'hanfix-ignore-words hanfix-ignore-words)

  (message "맞춤법 검사가 완료되었습니다."))

;;; --- User Commands ---

(defun hanfix-highlight-region ()
  "Test function, which highlight the next region for 5 secs."
  (interactive)
  (cl-destructuring-bind (s . e) (hanfix--get-next-region (point) (point-max))
    (let ((ov (make-overlay s e)))
      (overlay-put ov 'face '(:background "yellow" :extend t))
      (run-with-timer 5 nil 'delete-overlay ov))))

(defun hanfix-check-region ()
  "Check the current region.
If no region is specified, check the current paragraph."
  (interactive)
  (let* ((use-region (use-region-p))
         (start (if use-region (region-beginning) (save-excursion (backward-paragraph) (point))))
         (end (if use-region (region-end) (save-excursion (forward-paragraph) (point)))))
    (hanfix--run-loop start end)))

(defun hanfix-check-all ()
  "Check the entire document."
  (interactive)
  (hanfix--run-loop (point-min)))

(defun hanfix-check-from-here ()
  "Check the document starting from the current cursor position."
  (interactive)
  (hanfix--run-loop (point)))

(defvar hanfix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h c") 'hanfix-check-region)
    (define-key map (kbd "C-c h a") 'hanfix-check-all)
    (define-key map (kbd "C-c h h") 'hanfix-check-from-here)
    (define-key map (kbd "C-c h o") 'hanfix-highlight-region)
    map)
  "Keymap for `hanfix-mode'.")

;;;###autoload
(define-minor-mode hanfix-mode
  "Minor mode for Korean grammar checking."
  :lighter " Hanfix"
  :keymap hanfix-mode-map
  :group 'hanfix
  (when hanfix-mode
    (let* ((exec (hanfix--check-executable))
           (api-key (hanfix--check-gemini-api-key)))
      (if (or exec api-key)
          t
        (error "Hanfix-mode를 사용하려면 hanfix 실행파일 또는 Gemini API key를 설정해야 합니다")))))

(provide 'hanfix)

;;; hanfix.el ends here
