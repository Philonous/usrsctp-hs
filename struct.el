(defun struct--read-body (beg end)
  (let ((fields nil)
        (cont 't)
        )
    (goto-char beg)
    (while cont
      (if (looking-at
           "[[:blank:]\n]*\\([[:alnum:]_]+\\)[[:blank:]]+\\([[:alnum:]_]+\\)\\(;\\)")
          (progn
            (setq fields (append fields (list (cons (match-string-no-properties 2)
                                                    (match-string-no-properties 1)))
                                 ))
            (goto-char (match-end 3))
            )
        (setq cont nil)))
    fields))

(defun struct--read (beg end)
  (goto-char beg)
  (if (looking-at "[ ]*struct[ ]+\\([[:alpha:]_]+\\)[ ]+{\\([^}]+\\)}[ ]*;")
      (let ((struct-name (match-string-no-properties 1))
            (struct-body-beg (match-beginning 2))
            (struct-body-end (match-end 2))
            )
        (cons struct-name (struct--read-body struct-body-beg struct-body-end))
        )
    (error "Could not parse struct")
    ))

(defun struct--write-hsc-bindings (struct prefix)
  (pcase-let* ((`(,c-name . (,first-field . ,other-fields)) struct)
               (`(,first-field-name . ,first-field-type) first-field)
               (chopped-name (s-chop-prefix prefix c-name))
               (hs-name (s-upper-camel-case chopped-name))
               (hs-field-prefix (s-lower-camel-case chopped-name)))

    ;; Data definition
    (insert "-- struct " c-name "\n")
    (insert "data " hs-name " = " hs-name "\n")

    (insert "  { " hs-field-prefix (s-upper-camel-case first-field-name)
            " :: #{type " first-field-type "}\n")
    (pcase-dolist (`(,field-name . ,field-type) other-fields)
      (insert "  , " hs-field-prefix (s-upper-camel-case field-name)
              " :: #{type " field-type "}\n"))

    (insert "  }\n\n")


    ;; Storable instance
    ;;;;;;;;;;;;;;;;;;;;
  (insert "instance Storable " hs-name " where\n" )
  (insert "  sizeOf _ = #size struct " c-name "\n")
  (insert "  alignment _ =  #alignment struct " c-name "\n")
  ;; peek
  (insert "  peek ptr = do" "\n")
  (pcase-dolist (`(,field-name . ,field-type) (cons first-field other-fields) )
    (insert "    " hs-field-prefix (s-upper-camel-case field-name)
            " <- #{peek struct " c-name ", " field-name "} ptr" "\n")
    )
  (insert "    return " hs-name "{..}" "\n")
  ;; poke
  (insert "  poke ptr " hs-name  "{..} = do" "\n")
  (pcase-dolist (`(,field-name . ,field-type) (cons first-field other-fields) )
    (insert "    #{poke struct " c-name ", " field-name "} ptr "
            hs-field-prefix (s-upper-camel-case field-name) "\n")
    )))

(defun struct--storable-instance (prefix)
  (interactive "sprefix: ")
  (if (region-active-p)
      (let* ((beg (region-beginning))
             (end (region-end))
             (struct (struct--read beg end)))
        (delete-region beg end)
        (struct--write-hsc-bindings struct prefix))
    (error "Select a struct to replace with the instance")
    ))
