(defcustom *motd-preferred-languages* '(:en :fr :es :de :ja :zh)
  "The ordered list of preferences for language codes."
  :group 'motd
  :type 'list)

(defcustom *motd-url* "http://motd.lisp.org/motds/most-recent"
  "The URL at which to retrieve the most recent messages of the day."
  :group 'motd
  :type 'url)
;(setq *motd-url* "http://localhost:8000/motds/most-recent")

(defcustom *motd-messages-to-cache* 10
  "The number of messages to keep on-hand in the cache."
  :group 'motd
  :type 'integer
  :safe 'integerp)

;;; This is :latin1 right now because that is what portable AllegroServe
;;; writes when not running under Allegro.
(defvar +cache-external-format+ :latin1)
;;; XXX - need to use :latin1-ness
(defvar *motd-local-cache* (expand-file-name "~/.lisp-motd"))
(defvar *motd-cache-expiry* (* 12 60 60))

(defun motd-cache-exists-p ()
  (file-exists-p *motd-local-cache*))

(defun motd-file-age (filename)
  (destructuring-bind (now-high now-low n-ms n-ps) (current-time)
    (destructuring-bind (file-high file-low f-ms f-ps)
        (fifth (file-attributes filename))
      (+ (* (- now-high file-high) 65536)
         (- now-low file-low)))))

(defun motd-cache-expired-p ()
  (if (motd-cache-exists-p)
      (< *motd-cache-expiry* (motd-file-age *motd-local-cache*))
      t))

(defun motd-cache-backup-name ()
  (expand-file-name ".lisp-motd-cache"
                    (file-name-directory *motd-local-cache*)))

(defun motd-careful-rename-file (old new)
  (when (file-exists-p old)
    (when (file-exists-p new)
      (delete-file new))
    (rename-file old new)))

(defun motd-restore-cache-from-backup ()
  (motd-careful-rename-file (motd-cache-backup-name) *motd-local-cache*))

(defun motd-delete-cache ()
  ;; Need to delete the backup file, too, so we don't just
  ;; restore from it instead of fetching next time.
  (let ((backup (motd-cache-backup-name)))
    (when (file-exists-p backup)
      (delete-file backup)))
  (when (motd-cache-exists-p)
    (delete-file *motd-local-cache*)))

(defun motd-slurp-cache ()
  (with-current-buffer
      (find-file-noselect *motd-local-cache*)
    (save-restriction
      (widen)
      (buffer-substring-no-properties
       (point-min)
       (point-max)))))

(defun motd-load-cache ()
  (motd-restore-cache-from-backup)
  (read (motd-slurp-cache)))

(defun motd-motds-if-enough (contents)
  (if contents
    (destructuring-bind (requested &rest motds) contents
      (if (<= *motd-messages-to-cache* requested)
        (values motds requested)
        (values nil 0)))
    (values nil 0)))

(defun motd-load-cached-motds ()
  (motd-motds-if-enough (motd-load-cache)))

(defun motd-http-fetch-motds (url)
  (let ((result (url-retrieve-synchronously url)))
    (with-current-buffer result
      (save-restriction
        (widen)
        (let ((beg (beginning-of-buffer)))
          (forward-paragraph)
          (forward-line 2))
        (buffer-substring-no-properties
         (point)
         (point-max))))))

(defun motd-cache-results (results)
  (with-temp-file (motd-cache-backup-name)
    (let ((standard-output (current-buffer)))
      (insert results)
      standard-output))
  (motd-restore-cache-from-backup))

(defun motd-fetch-motds ()
  (let* ((url (mapconcat 'identity
                         (list *motd-url*
                               (prin1-to-string *motd-messages-to-cache*))
                         "/"))
         (result (motd-http-fetch-motds url)))
    (motd-cache-results result)
    (multiple-value-bind (motds requested) (motd-load-cached-motds)
      motds)))

(defun motd-enough-motds-cached-p ()
  (multiple-value-bind (motds requested) (motd-load-cached-motds)
    (when (<= *motd-messages-to-cache* requested)
      motds)))

(defun motd-load-or-fetch-motds ()
  (cond
    ((motd-cache-expired-p)
     (motd-fetch-motds))
    (t
     (multiple-value-bind (motds requested) (motd-load-cached-motds)
       (or motds
           (motd-fetch-motds))))))

(defun motd-symbol-name (sym)
  ;; Remove the leading colon from the symbol-name of keywords
  (substring (symbol-name sym) 1))

(defun motd-print-tags (tags)
  (let* ((str (mapconcat 'identity (mapcar 'motd-symbol-name tags) ","))
         (pre-len (- 70 (length str))))
    (princ
     (substring
      "----------------------------------------------------------------------"
      0
      (max 0 pre-len)))
    (princ (downcase str))
    (princ "--")
    (terpri)))

(defun motd-print-motd-header ()
  (princ "Lisp Message of the Day")
  (terpri)
  (princ
   "========================================================================")
  (terpri))

(defun motd-rank-language (language)
  (or (position language *motd-preferred-languages*)
      (length *motd-preferred-languages*)))

(defun motd-tracking-best (translations best best-score)
  (cond
   (translations
    (destructuring-bind ((language . text) &rest translations) translations
      (let ((rank (motd-rank-language language)))
        (if (or (not best-score)
                (< rank best-score))
            (motd-tracking-best translations text rank)
          (motd-tracking-best translations best best-score)))))
   (t
    best)))

(defun motd-find-best-translation (translations)
  (motd-tracking-best translations nil nil))

(defun motd-print-motd (motd)
  (let* ((p-list (rest motd))
         (translations (plist-get p-list :TRANSLATIONS))
         (tags (plist-get p-list :TAGS)))
    (princ (motd-find-best-translation translations))
    (terpri)
    (motd-print-tags tags)))

(defun motd-delete-old-contents ()
  (with-current-buffer standard-output
    (erase-buffer)))

(defun motd-print-motds (display-at-most)
  (let* ((standard-output (get-buffer-create "*Lisp Message of the Day*"))
         (all-motds (when (plusp display-at-most)
                      (motd-load-or-fetch-motds)))
         (motds (subseq all-motds 0 (min (length all-motds)
                                         display-at-most))))
    (when motds
      (motd-delete-old-contents)
      (motd-print-motd-header)
      (mapcar 'motd-print-motd motds)
      motds)))

(defun motd (display-at-most)
  (interactive "P")
  (motd-print-motds (or display-at-most
                        *motd-messages-to-cache*))
  (display-buffer "*Lisp Message of the Day*")
  (values))

(provide 'motd)