;;; hanfix.el --- Gemini-based Korean grammar checker -*- lexical-binding: t; -*-

;; Author: ntalbs
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.2"))
;; Keywords: convenience, wp
;; URL: https://github.com/ntalbs/hanfix-mode

;;; Commentary:
;; 이 패키지는 Gemini API를 사용해 한국어 맞춤법 및 문법 교정을 돕는
;; 마이너 모드입니다.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'url)

(defgroup hanfix nil
  "한국어 맞춤법 검사기 hanfix 설정."
  :group 'editing)

(defcustom hanfix-gemini-api-key ""
  "Gemini API 키 (https://aistudio.google.com/ 에서 발급)."
  :type 'string
  :group 'hanfix)

(defcustom hanfix-max-length 1000
  "Hanfix CLI로 보낼 문자열 최대 길이."
  :type 'number
  :group 'hanfix)

(defcustom hanfix-ignore-words '()
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

(defface hanfix-face-buffer-section-header
  '((t :background "gray90" :extend t))
  "Hanfix 버퍼 섹션 헤더."
  :group 'hanfix)

(defface hanfix-face-buffer-original-text
  '((t :foreground "red"))
  "Hanfix 버퍼 원문."
  :group 'hanfix)

(defface hanfix-face-buffer-suggestion-text
  '((t :foreground "forestgreen"))
  "Hanfix 버퍼 수정 제안 텍스트."
  :group 'hanfix)

;;; --- 내부 유틸리티 ---

(defconst hanfix-buffer "*Hanfix*"
  "Hanfix에서 사용할 버퍼 이름.")

;; 조사 목록 (길이가 긴 것부터!)
(defconst hanfix-korean-josa-list
  '(;; 3글자
    "에게는" "에게도" "에게로" "에게만" "에게서" "이라고" "으로서" "으로써"
    ;; 2글자
    "과는" "까지" "께서" "라도" "라고" "라니" "라서" "만큼" "마저" "밖에"
    "보다" "부터" "에게" "와는" "으로" "이나" "이라" "조차" "처럼" "하고"
    ;; 1글자
    "가" "과" "께" "나" "는" "도" "랑" "를" "만" "야" "와" "은" "을" "이" "에"

    ;; 기타 (조사는 아니지만, 어구 끝에 조사처럼 붙어서 처리되는 녀석들)
    ":" ","))

(defun hanfix--check-gemini-api-key ()
  "Google API Key가 설정되었는지 확인."
  (if (string-empty-p hanfix-gemini-api-key)
      (error "'M-x customize-group hanfix'에서 Gemini API 키를 설정하세요")))

(defun hanfix--build-prompt (text)
  "TEXT를 이용해 Gemini에 보낼 프롬프트 생성."
  (let ((ignored (if hanfix-ignore-words
                     (concat "\n단, 다음 단어들은 사용자가 의도한 것이므로 절대 수정하지 마: "
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

(defun hanfix--exec-gemini (text)
  ;; (message "테스트 모드: Mock 데이터를 반환합니다.")
  ;; '(((original . "스크립트 조합해 사용해")
  ;;    (suggestions . ["스크립트를 조합해 사용하여" "스크립트와 조합하여 사용해"])
  ;;    (explanation . "목적격 조사(를)가 생략되어 문맥이 매끄럽지 않으며, 동일한 어미 '-해'가 반복적으로 사용되었습니다. 조사를 추가하고 어미를 다양화하는 것이 좋습니다."))
  ;;   ((original . "써왔는데")
  ;;    (suggestions . ["써 왔는데"])
  ;;    (explanation . "본동사 '쓰다'와 보조 동사 '오다'가 결합한 경우로, '아/어 오다' 구성은 띄어 쓰는 것이 원칙이며 붙여 쓰는 것도 허용되나, 의미 전달의 명확성을 위해 띄어 쓰는 것이 권장됩니다."))
  ;;   ((original . "찝찝했다")
  ;;    (suggestions . ["찜찜했다"])
  ;;    (explanation . "'찝찝하다'는 표준어이지만, '마음이 가볍지 않고 언짢은 느낌이 있다'는 의미로는 '찜찜하다'가 더 널리 쓰이는 표준 표현입니다."))))
  "Gemini API를 호출하여 결과를 파싱해 에러 리스트를 반환합니다."
  (let* ((model "gemini-3-flash-preview")
         (api-url (format "https://generativelanguage.googleapis.com/v1beta/models/%s:generateContent" model))
         (url-request-method "POST")
         ;; API 키를 헤더용 유니바이트 바이트 문자열로 변환
         (safe-api-key (encode-coding-string hanfix-gemini-api-key 'utf-8))
         (url-request-extra-headers
          `(("Content-Type" . "application/json")
            ("x-goog-api-key" . ,safe-api-key)))
         ;; 프롬프트 생성 및 JSON 인코딩
         (json-str (json-encode
                    `((contents . [((parts . [((text . ,(hanfix--build-prompt text)))]))])
                      (generationConfig . ((response_mime_type . "application/json"))))))
         ;; 본문 데이터(한글 포함)를 UTF-8 바이트 문자열로 변환
         (url-request-data (encode-coding-string json-str 'utf-8))
         (buffer (url-retrieve-synchronously api-url)))

    (if (not buffer)
        (error "API 호출 중 응답 버퍼를 생성하지 못했습니다.")
      (with-current-buffer buffer
        (set-buffer-multibyte t)
        (decode-coding-region (point-min) (point-max) 'utf-8)

        (goto-char (point-min))

        ;; HTTP 응답 코드 확인 (200 OK 여부)
        (if (not (re-search-forward "^HTTP/[0-9.]+\\s-+200" nil t))
            (let ((resp-content (buffer-string)))
              (kill-buffer buffer)
              (error "Gemini API 호출 실패 (HTTP Error): %s" resp-content))
          ;; 본문 시작 위치로 이동
          (goto-char (point-min))
          (if (not (re-search-forward "^$" nil t))
              (progn (kill-buffer buffer) (error "API 응답 헤더와 본문을 분리할 수 없습니다."))
            (let* ((json-object (json-read))
                   ;; 안전한 데이터 추출
                   (candidates (append (cdr (assoc 'candidates json-object)) nil))
                   (first-candidate (elt candidates 0))
                   (content (cdr (assoc 'content first-candidate)))
                   (parts (append (cdr (assoc 'parts content)) nil))
                   (first-part (elt parts 0))
                   (content-text (cdr (assoc 'text first-part))))

              (kill-buffer buffer)
              (if (not content-text)
                  (progn
                    (message "API 응답 본문에 텍스트 데이터가 없습니다.")
                    nil)
                ;; 제미니가 반환한 JSON 텍스트 파싱 (original, suggestions, explanation 구조)
                (let* ((parsed-res (json-read-from-string content-text))
                       (errors (append (cdr (assoc 'errors parsed-res)) nil)))
                  (message ">>> %s" errors)
                  errors)))))))))

(defun hanfix--split-korean-josa-word (word)
  "WORD에서 조사 분리. (stem . josa) 또는 (word . nil) 반환."
  (cl-loop for j in hanfix-korean-josa-list
           when (and (> (length word) (length j))
                     (string-suffix-p j word))
           do (let ((stem (substring word 0 (- (length word) (length j)))))
                (when (> (length stem) 0)
                  (cl-return (cons stem j))))
           finally return (cons word nil)))

(defun hanfix--remove-josa (word)
  (car (hanfix--split-korean-josa-word word)))

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

(defun hanfix--show-control-buffer (original suggestions help)
  "벡터 형식의 suggestions를 받아 번호와 함께 표시합니다."
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
                           (insert help)
                           (fill-region (point-min) (point-max))
                           (buffer-string))))
        (insert "\n" filled-help "\n\n"))
      (if (> len 1)
          (insert " (1-" (format "%d" len)  "/y/n/e/i/q/?):")
        (insert " (1/y/n/e/i/q/?):"))
      (let ((desired-height (+ 2 (count-lines (point-min) (point-max)))))
        (pop-to-buffer hanfix-buffer `((display-buffer-at-bottom) (window-height . ,desired-height)))))))

(defun hanfix--show-help ()
  "정보 버퍼에 조작법 도움말을 표시합니다."
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

(defun hanfix--recenter (n)
  (let* ((pos (pos-visible-in-window-p (point) nil t))
         (y-pos (cadr pos))
         (win-height (window-body-height nil t)))
    (when (or (null pos)
              (> y-pos (* win-height 0.5)))
      (recenter n))))

(defun hanfix--fix-errors (start end errors)
  (let ((main-buffer (current-buffer))
        (search-from start)
        (search-until (copy-marker end)))
    (save-excursion
      (cl-loop named main-loop
               for err in errors
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
  "START, END로 지정된 영역에서 맞춤법 검사 실행."
  (let* ((section-ov (make-overlay start end))
         (status 'ok))
    (overlay-put section-ov 'face 'hanfix-face-section)
    (recenter 10)
    (redisplay t)
    (unwind-protect
        (let* ((text (buffer-substring-no-properties start end))
               (errors (hanfix--exec-gemini text)))
          (if errors
              (when (hanfix--fix-errors start end errors)
                (setq status 'quit))))
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

  (setq hanfix-ignore-words (sort (delete-dups hanfix-ignore-words) #'string<))
  (customize-save-variable 'hanfix-ignore-words hanfix-ignore-words)

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
    (hanfix--check-gemini-api-key)))

(provide 'hanfix)

;;; hanfix.el ends here
