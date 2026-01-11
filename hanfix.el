;;; hanfix.el --- 한국어 맞춤법 검사기 마이너 모드 -*- lexical-binding: t; -*-

;; Author: ntalbs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2"))
;; Keywords: convenience, wp
;; URL: https://github.com/ntalbs/hanfix-mode

;;; Commentary:
;; 이 패키지는 Hanfix CLI 도구를 사용해 Org/Markdown 모드에서
;; 한국어 맞춤법 검사를 돕는 마이너 모드입니다.

;;; Code:

(require 'json)
(require 'seq)

(defgroup hanfix nil
  "한국어 맞춤법 검사기 hanfix 설정."
  :group 'editing)

(defcustom hanfix-path "hanfix"
  "Hanfix 실행 파일 경로."
  :type 'file
  :group 'hanfix)

(defcustom hanfix-max-length 1000
  "한 번에 검사할 최대 글자수."
  :type 'integer
  :group 'hanfix)

(defcustom hanfix-ignored-words '()
  "맞춤법 검사 시 무시할 단어 목록."
  :type '(repeat string)
  :group 'hanfix)

;;; --- 스타일 ---

(defface hanfix-face-section
  '((t :background "gray90" :extend t))
  "검사 중인 단락 강조."
  :group 'hanfix)

(defface hanfix-face-error
  '((t :background "salmon" :foreground "white" :weight bold))
  "오류 단어 강조."
  :group 'hanfix)

;;; --- 내부 유틸리티 ---

(defun hanfix--cleanup-ui ()
  (let ((buf (get-buffer "*Hanfix*")))
    (when (buffer-live-p buf)
      (delete-windows-on buf)
      (kill-buffer buf))))

(defun hanfix--update-info-buffer (input output help)
  (with-current-buffer (get-buffer-create "*Hanfix*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "── [교정 제안] ────────────────────\n\n" 'face 'shadow))
      (insert (propertize "[X] " 'face '(:foreground "red")) input "\n")
      (insert (propertize "[O] " 'face '(:foreground "forest green")) output "\n")
      (insert "\n" (propertize "── [상세 설명] ────────────────────\n\n" 'face 'shadow))
      (insert help)
      (read-only-mode 1)))
  (display-buffer "*Hanfix*" '((display-buffer-at-bottom) (window-height . 12))))

(defun hanfix--update-help-buffer ()
  "정보 버퍼에 조작법 도움말을 표시합니다."
  (with-current-buffer (get-buffer-create "*Hanfix*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "── [도움말: 조작 방법] ────────────────\n\n" 'face 'shadow))
      (insert (propertize " y " 'face 'bold) "(yes)   : 제안된 단어로 교체\n")
      (insert (propertize " n " 'face 'bold) "(no)    : 교정하지 않고 다음으로 이동\n")
      (insert (propertize " e " 'face 'bold) "(edit)  : 직접 수정할 내용 입력\n")
      (insert (propertize " i " 'face 'bold) "(ignore): 무시 목록에 추가 및 저장\n")
      (insert (propertize " q " 'face 'bold) "(quit)  : 검사 중단\n")
      (insert (propertize " ? " 'face 'bold) "(help)  : 교정 정보 보기로 돌아가기\n\n")
      (insert (propertize "──────────────────────────────────" 'face 'shadow))
      (set-buffer-modified-p nil)
      (read-only-mode 1))))

(defun hanfix--fix-errors (main-buffer start end errors)
  (let ((stop-p nil))
    (with-current-buffer main-buffer
      (save-excursion
        (goto-char start)
        (dolist (err errors)
          (unless stop-p
            (let ((input (cdr (assoc 'input err)))
                  (output (cdr (assoc 'output err)))
                  (help (cdr (assoc 'helpText err))))
              (when (search-forward (regexp-quote input) end t)
                (let ((m-data (match-data))
                      (ov (make-overlay (match-beginning 0) (match-end 0))))
                  (overlay-put ov 'face 'hanfix-face-error)
                  (recenter 10)
                  (hanfix--update-info-buffer input output help) ; 기본 정보 표시

                  (unwind-protect
                      (let ((done nil)
                            (showing-help nil)
                            (choice nil))
                        (while (not done)
                          (setq choice (read-char-choice
                                        (format "[%s -> %s]? (y/n/e/i/q/?) " input output)
                                        '(?y ?n ?e ?i ?q ??)))
                          (cond
                           ;; '?' 토글 로직: 분리된 함수들 호출
                           ((eq choice ??)
                            (if showing-help
                                (hanfix--update-info-buffer input output help)
                              (hanfix--update-help-buffer))
                            (setq showing-help (not showing-help)))

                           ;; 기능 수행 및 루프 종료
                           (t (setq done t)
                              (cond
                               ((eq choice ?y) (set-match-data m-data) (replace-match output) (undo-boundary))
                               ((eq choice ?e) (let ((new (read-string "수정: " output))) (set-match-data m-data) (replace-match new)) (undo-boundary))
                               ((eq choice ?i)
                                (add-to-list 'hanfix-ignored-words input)
                                (customize-save-variable 'hanfix-ignored-words hanfix-ignored-words)
                                (message "단어 '%s'를 무시 목록에 추가했습니다." input))
                               ((eq choice ?q) (setq stop-p t)))))))
                    (delete-overlay ov)))))))))
    stop-p))

(defun hanfix--process-region (start end)
  (let* ((main-buffer (current-buffer))
         (section-ov (make-overlay start end))
         (status 'ok))
    (overlay-put section-ov 'face 'hanfix-face-section)
    (recenter 10)
    (unwind-protect
        (let* ((json-raw (with-temp-buffer
                           (insert-buffer-substring main-buffer start end)
                           (shell-command-on-region (point-min) (point-max) hanfix-path nil t)
                           (buffer-string)))
               (data (condition-case nil (json-parse-string json-raw :object-type 'alist) (error nil)))
               (errors (seq-filter (lambda (err)
                                     (not (member (cdr (assoc 'input err)) hanfix-ignored-words)))
                                   (append (cdr (assoc 'errors data)) nil))))
          (if errors
              (when (hanfix--fix-errors main-buffer start end errors) (setq status 'quit))
            (progn (message "오류 없음...") (sit-for 0.05))))
      (delete-overlay section-ov)
      (hanfix--cleanup-ui))
    status))

(defun hanfix--run-loop (start-point)
  (save-excursion
    (goto-char start-point)
    (let ((continue t))
      (while (and continue (not (eobp)))
        (skip-chars-forward " \t\n\r")
        (unless (eobp)
          (let* ((p-start (point))
                 (p-end (save-excursion (forward-paragraph) (point))))
            (while (let ((next-p (save-excursion (goto-char p-end) (forward-paragraph) (point))))
                     (and (not (eobp)) (> next-p p-end) (<= (- next-p p-start) hanfix-max-length)))
              (setq p-end (save-excursion (goto-char p-end) (forward-paragraph) (point))))
            (if (eq (hanfix--process-region p-start p-end) 'quit)
                (setq continue nil)
              (goto-char p-end))))))
    (hanfix--cleanup-ui)
    (message "맞춤법 검사가 완료되었습니다.")))

(defun hanfix--check-executable ()
  "Hanfix 실행 파일이 있는지 확인."
  (if (executable-find hanfix-path)
      t
    (warn "hanfix 실행 파일을 찾을 수 없습니다. '%s' 경로를 확인하거나 CLI 도구를 설치하세요." hanfix-path)
    nil))

;;; --- 사용자 명령어 ---

(defun hanfix-check ()
  "현재 영역 또는 단락 하나만 검사합니다."
  (interactive)
  (let* ((use-region (use-region-p))
         (start (if use-region (region-beginning) (save-excursion (backward-paragraph) (point))))
         (end (if use-region (region-end) (save-excursion (forward-paragraph) (point)))))
    (deactivate-mark)
    (hanfix--process-region start end)
    (message "검사 완료.")))

(defun hanfix-check-all ()
  "문서 처음부터 끝까지 전체를 검사합니다."
  (interactive)
  (hanfix--run-loop (point-min)))

(defun hanfix-check-from-here ()
  "현재 커서 위치부터 문서 끝까지 검사합니다."
  (interactive)
  (hanfix--run-loop (point)))

(defvar hanfix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h c") 'hanfix-check)
    (define-key map (kbd "C-c h a") 'hanfix-check-all)
    (define-key map (kbd "C-c h h") 'hanfix-check-from-here)
    map)
  "Keymap for `hanfix-mode'.")

;;;###autoload
(define-minor-mode hanfix-mode
  "Minor mode for Korean grammar checking."
  :lighter " Hanfix"
  :keymap hanfix-mode-map
  :group 'hanfix
  (when hanfix-mode
    (hanfix--check-executable)))

(provide 'hanfix)

;;; hanfix.el ends here
