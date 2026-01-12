;;; hanfix.el --- 한국어 맞춤법 검사기 마이너 모드 -*- lexical-binding: t; -*-

;; Author: ntalbs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2"))
;; Keywords: convenience, wp
;; URL: https://github.com/ntalbs/hanfix-mode

;;; Commentary:
;; 이 패키지는 Hanfix CLI(https://www.npmjs.com/package/hanfix) 도구를
;; 사용해 Org/Markdown 모드에서 한국어 맞춤법 검사를 돕는 마이너
;; 모드입니다.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)

(defgroup hanfix nil
  "한국어 맞춤법 검사기 hanfix 설정."
  :group 'editing)

(defcustom hanfix-path "hanfix"
  "Hanfix 실행 파일 경로."
  :type 'file
  :group 'hanfix)

(defcustom hanfix-ignored-words '()
  "맞춤법 검사 시 무시할 단어 목록."
  :type '(repeat string)
  :group 'hanfix)

(defface hanfix-face-section
  '((t :background "gray90" :extend t))
  "검사 중인 단락 강조."
  :group 'hanfix)

(defface hanfix-face-error
  '((t :background "salmon" :foreground "white" :weight bold))
  "오류 단어 강조."
  :group 'hanfix)

;;; --- 내부 유틸리티 ---

(defconst hanfix-buffer "*Hanfix*"
  "Hanfix에서 사용할 버퍼 이름.")

(defconst hanfix-max-length 1000
  "Hanfix CLI로 보낼 문자열 최대 길이.")

;; 조사 목록 (길이가 긴 것부터!)
(defconst hanfix-korean-josa-list
  '("에서" "부터" "까지" "조차" "마저" "밖에" "보다" "으로서" "으로써"
    "에게서" "에게로" "에게도" "에게만"
    "으로" "라고" "이라고" "하고" "처럼" "만큼" "에게" "께서"
    "라도" "부터" "까지" "이라" "이나" "든지" "라서" "라니"
    "와는" "과는" "에게는"
    "을" "를" "이" "가" "은" "는" "도" "만" "나" "야" "께" "랑" "와" "과"))

(defun hanfix--check-executable ()
  "Hanfix 실행 파일이 있는지 확인."
  (if (executable-find hanfix-path)
      t
    (warn "hanfix 실행 파일을 찾을 수 없습니다. '%s' 경로를 확인하거나 CLI 도구를 설치하세요." hanfix-path)
    nil))

(defun hanfix--split-korean-josa-word (word)
  "WORD에서 조사 분리. (stem . josa) 또는 (word . nil) 반환."
  (cl-loop for j in hanfix-korean-josa-list
           when (and (> (length word) (length j))
                     (string-suffix-p j word))
           do (let ((stem (substring word 0 (- (length word) (length j)))))
                (when (> (length stem) 0)
                  (cl-return (cons stem j))))
           finally return word))

(defun hanfix--read-char (chars)
  (let ((ch nil))
    (while (not (memq ch chars))
      (clear-this-command-keys)
      (setq ch (read-char-exclusive)))
    ch))

(defun hanfix--cleanup-ui ()
  (let ((buf (get-buffer hanfix-buffer)))
    (when (buffer-live-p buf)
      (delete-windows-on buf)
      (kill-buffer buf))))

(defun hanfix--show-control-buffer (input output help)
  (with-current-buffer (get-buffer-create hanfix-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "교정 제안:\n\n" 'face 'font-lock-keyword-face))
      (if (eq input output)
          (insert (propertize input 'face 'error) "\n\n")
        (insert (propertize input 'face 'error) " -> " (propertize output 'face 'success) "\n\n"))
      (insert help "\n\n")
      (insert "적용(y/n/e/i/q/?):")
      (read-only-mode 1)))
  (pop-to-buffer hanfix-buffer '((display-buffer-at-bottom) (window-height . 13))))

(defun hanfix--show-help ()
  "정보 버퍼에 조작법 도움말을 표시합니다."
  (with-current-buffer (get-buffer-create hanfix-buffer)
    (let ((inhibit-read-only t))
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
      (set-buffer-modified-p nil)
      (read-only-mode 1))))

(defun hanfix--recenter (n)
  (let* ((pos (pos-visible-in-window-p (point) nil t))
         (y-pos (cadr pos))
         (win-height (window-body-height nil t)))
    (when (or (null pos)
              (> y-pos (* win-height 0.5)))
      (recenter n))))

(defun hanfix--fix-errors (start end errors)
  (let ((main-buffer (current-buffer))
        (search-from start))
    (save-excursion
      (cl-loop for err in errors
               for input = (cdr (assoc 'input err))
               for output = (cdr (assoc 'output err))
               for help = (cdr (assoc 'helpText err))
               do
               (with-current-buffer main-buffer
                 (goto-char search-from)

                 (when (search-forward input end t)
                   (setq search-from (point))
                   (let ((m-data (match-data)))
                     (let ((ov (make-overlay (match-beginning 0) (match-end 0))))
                       (overlay-put ov 'face 'hanfix-face-error)

                       (with-selected-window (get-buffer-window main-buffer)
                         (goto-char search-from)
                         (hanfix--recenter 10))

                       (hanfix--show-control-buffer input output help)

                       (unwind-protect
                           (cl-loop named interaction
                                    for choice = (hanfix--read-char '(?y ?n ?e ?i ?q ??))
                                    do
                                    (cl-case choice
                                      (?? (hanfix--show-help)
                                          (when (eq (hanfix--read-char '(?q ??)) ?q)
                                            (cl-return-from nil 'quit))
                                          (hanfix--show-control-buffer input output help))
                                      (?y (with-current-buffer main-buffer
                                            (set-match-data m-data)
                                            (replace-match output)
                                            (undo-boundary))
                                          (cl-return-from interaction))
                                      (?n (cl-return-from interaction))
                                      (?e (with-current-buffer main-buffer
                                            (set-match-data m-data)
                                            (replace-match (read-string "수정: " output) t t)
                                            (undo-boundary))
                                          (cl-return-from interaction))
                                      (?i (add-to-list 'hanfix-ignored-words input)
                                          (cl-return-from interaction))
                                      (?q (cl-return-from nil 'quit))))
                         (delete-overlay ov))))))

               ;; 루프가 모든 errors를 순회하고 정상 종료되면 nil 반환
               finally return nil))))

(defun hanfix--exec-hanfix-bin (text)
  "TEXT를 hanfix 실행파일 전달하고 실행해 결과 JSON을 파싱해 리턴."
  (let* ((json-raw (with-temp-buffer
                     (insert text)
                     (shell-command-on-region (point-min) (point-max) hanfix-path nil t)
                     (buffer-string)))
         (data (condition-case nil
                   (json-parse-string json-raw :object-type 'alist)
                 (error nil))))
    (seq-filter (lambda (err)
                  (not (member (cdr (assoc 'input err)) hanfix-ignored-words)))
                (append (cdr (assoc 'errors data)) nil))))

(defun hanfix--process-region (start end)
  "START, END로 지정된 영역에서 맞춤법 검사 실행."
  (let* ((section-ov (make-overlay start end))
         (status 'ok))
    (overlay-put section-ov 'face 'hanfix-face-section)
    (recenter 10)
    (unwind-protect
        (let* ((text (buffer-substring-no-properties start end))
               (errors (hanfix--exec-hanfix-bin text)))
          (if errors
              (when (hanfix--fix-errors start end errors)
                (setq status 'quit))
            (progn
              (message "오류 없음...")
              (sit-for 0.05))))
      ;; cleanup
      (delete-overlay section-ov)
      (hanfix--cleanup-ui))
    status))

(defun hanfix--get-next-region (region-start region-end)
  "Hanfix-max-length를 넘지 않으면서 가장 가깝게 되도록 단락을 병합해 범위 리턴.
REGION-START부터 REGION-END 범위에서 전체 텍스트 길이가"
  (save-excursion
    (let* ((current-point region-start)
           (end-point region-start)
           (region-end (if (not region-end) (point-max) region-end)))
      (cl-loop while (and (< end-point region-end) (not (eobp)))
               do
               (setq end-point (save-excursion (forward-paragraph) (point)))

               if (> (- end-point region-start) hanfix-max-length)
                   if (= region-start current-point) ; 한 단락이 너무 커서 forward-paragraph 시 hanfix-max-length를 넘은 경우
                       return (save-excursion
                                (goto-char (+ region-start hanfix-max-length))
                                (backward-word)
                                (cons region-start (min (point) region-end)))
                   else
                       return (cons region-start (min current-point region-end))
               else do
                   (message ">>> forward-para")
                   (forward-paragraph)
                   (setq current-point end-point)
                   (setq end-point (point))

               finally
                   return (cons region-start region-end)))))

(defun hanfix-highlight-region ()
  "테스트 함수.  hanfix--get-next-region을 얻어 표시한 다음 5초 후 표시를 제거."
  (interactive)
  (cl-destructuring-bind (s . e) (hanfix--get-next-region (point) (point-max))
    (let ((ov (make-overlay s e)))
      (overlay-put ov 'face '(:background "yellow" :extend t))
      (run-with-timer 5 nil 'delete-overlay ov))))

(defun hanfix--run-loop (start-point &optional end-point)
  "START-POINT부터 END-POINT까지 hanfix--get-next-region을 호출해 맞춤법 검사할 영역을 얻어가며 검사 진행."
  (setq end-point (if end-point end-point (point-max)))
  (save-excursion
    (goto-char start-point)
    (cl-loop
     ;; 1. 공백 건너뛰고 시작 지점 잡기
     do (skip-chars-forward " \t\n\r")

     ;; 2. 종료 조건 확인 (끝이면 여기서 멈춤)
     until (or (eobp) (>= (point) end-point));; 문서 끝 또는 범위 끝이면 종료

     ;; 3. 다음 처리할 영역(p-start . p-end) 계산
     for (p-start . p-end) = (hanfix--get-next-region (point) end-point)


     ;; 4. 영역 처리 및 'quit 신호 확인. 처리가 끝난 지점으로 이동하여 다음 루프 준비
     do (if (eq (hanfix--process-region p-start p-end) 'quit)
            (cl-return) ;; 사용자가 q를 누르면 루프 즉시 탈출
          (goto-char p-end))))

  ;; 5. 사후 정리
  (hanfix--cleanup-ui)
  (message "맞춤법 검사가 완료되었습니다."))

;;; --- 사용자 명령어 ---

(defun hanfix-check-region ()
  "현재 영역 또는 단락 하나만 검사합니다."
  (interactive)
  (let* ((use-region (use-region-p))
         (start (if use-region (region-beginning) (save-excursion (backward-paragraph) (point))))
         (end (if use-region (region-end) (save-excursion (forward-paragraph) (point)))))
    (hanfix--run-loop start end)))

(defun hanfix-check-all ()
  "문서 처음부터 끝까지 전체를 검사합니다."
  (interactive)
  (hanfix--run-loop (point-min)))

(defun hanfix-check-from-here ()
  "현재 커서 위치부터 문서 끝까지 검사합니다."
  (interactive)
  (hanfix--run-loop (point)))

(defun hanfix-check-dummy ()
  "For test only."
  (interactive)
  (hanfix--show-control-buffer "솔루션을" "설루션을" "솔루션의 정확한 표기는 설루션이라는데 이게 진짜 맞아?")
  (unwind-protect
      (let ((done nil)
            (choice nil))
        (while (not done)
          (setq choice (hanfix--read-char '(?y ?n ?e ?i ?q ??)))
          (cond
           ((eq choice ??)
            (hanfix--show-help)
            (if (eq (hanfix--read-char '(?q ??)) ?q)
                (setq done t)
              (hanfix--show-control-buffer "솔루션을" "설루션을" "솔루션의 정확한 표기는 설루션이라는데 이게 진짜 맞아?")))
           ((eq choice ?q)
            (setq done t))
           (t (message "%c is selected" choice)))))
    (hanfix--cleanup-ui)))

(defvar hanfix-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c h c") 'hanfix-check-region)
    (define-key map (kbd "C-c h a") 'hanfix-check-all)
    (define-key map (kbd "C-c h h") 'hanfix-check-from-here)
    (define-key map (kbd "C-c h d") 'hanfix-check-dummy)
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
    (hanfix--check-executable)))

(provide 'hanfix)

;;; hanfix.el ends here
