; author: Thomas Hintz
; email: t@thintz.com
; license: bsd

; Copyright (c) 2012, 2020, Thomas Hintz
; All rights reserved.

; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;     * Redistributions in binary form must reproduce the above copyright
;       notice, this list of conditions and the following disclaimer in the
;       documentation and/or other materials provided with the distribution.
;     * Neither the name of the <organization> nor the
;       names of its contributors may be used to endorse or promote products
;       derived from this software without specific prior written permission.

; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THOMAS HINTZ BE LIABLE FOR ANY
; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; 	    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; 	    ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; 	    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; 	    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(module send-grid
  (;; params
   api-user api-key api-version

   ;; procs
   send-mail)

(import scheme chicken extras ports)
(use data-structures http-client uri-common intarweb json srfi-1 srfi-18 base64)

;; intarweb doesn't support Bearer authorization so we do it ourselves
(header-unparsers (append (header-unparsers) `((Authorization . ,(lambda (x) (list (conc (vector-ref (car x) 0) " " (vector-ref (car x) 1))))))))

(define api-user (make-parameter ""))
(define api-key (make-parameter ""))
(define api-version (make-parameter "2"))

(define (rest-action url method parameters #!key (headers (headers '())))
  (with-input-from-request
      (make-request method: method uri: (uri-reference url) headers: headers) parameters read-string))

(define (send-mail-2 #!key (subject #f) (text #f) (html #f) (from #f) (from-name #f) (to #f) (reply-to #f) (api-user (api-user)) (api-key (api-key)) files)
  (if (and subject (or text html) from from-name to reply-to)
      (rest-action "https://sendgrid.com/api/mail.send.json" 'POST
                   `((api_user . ,api-user)
                     (api_key . ,api-key)
                     (subject . ,subject)
                     (to . ,to)
                     (replyto . ,reply-to)
                     ,(if html `(html . ,html) `(text . ,text))
                     (from . ,from)
                     (fromname . ,from-name)
                     ,@(map
                        (lambda (file-details)
                          `(,(string->symbol
                              (string-append
                               "files[" (alist-ref 'filename file-details) "]"))
                            file: ,(alist-ref 'filepath file-details)
                            filename: ,(alist-ref 'filename file-details)
                            headers: ((content-type
                                       ,(alist-ref 'content-type file-details)))))
                        files)))
      (abort "All parameters are required for successfully sending mail.")))

(define (send-mail-3 #!key (subject #f) (text #f) (html #f) (from #f) (from-name #f) (to #f) (reply-to #f) (api-user (api-user)) (api-key (api-key)) files)
  (if (and subject (or text html) from from-name to reply-to)
      (let ((res (with-input-from-request
                     (make-request method: 'POST uri: (uri-reference "https://api.sendgrid.com/v3/mail/send")
                                   headers: (headers `((content-type #(application/json ()))
                                                       (Authorization #(Bearer ,api-key)))))
                   (lambda ()
                     (json-write
                      `#((personalizations . (#((to . (#((email . ,to)
                                                         (name . ,from-name))))
                                                (subject . ,subject))))
                         (from . #((email . ,from)
                                   (name . ,from-name)))
                         (reply_to . #((email . ,reply-to)))
                         (content . (,@(if text
                                           `(#((type . "text/plain")
                                               (value . ,text)))
                                           '())
                                     ,@(if html
                                           `(#((type . "text/html")
                                               (value . ,html)))
                                           '())))
                         ,@(if files
                               `((attachments . ,(map
                                                  (lambda (file-details)
                                                    `#((type . ,(symbol->string (alist-ref 'content-type file-details)))
                                                       (content . ,(base64-encode (with-input-from-file (alist-ref 'filepath file-details) read-string)))
                                                       (filename . ,(alist-ref 'filename file-details))))
                                                  files)))
                               '()))))
                   read-string)))
        (if (equal? res "")
            (with-output-to-string (lambda () (json-write #((message . "success"))))) ;; mimic v2 API
            (with-input-from-string res json-read)))
      (abort "All parameters are required for successfully sending mail.")))

(define (send-mail #!rest r)
  (apply (cond ((equal? (api-version) "2")
                send-mail-2)
               ((equal? (api-version) "3")
                send-mail-3)
               (else
                (error "Unknown API version")))
         r))

)
