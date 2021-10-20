;; TODO: Index icons considering hierarchies of classes. Specially, "subclass of"

(require 'org-roam)

(defvar orkg-class-p (lambda (node) t)
  "Store a lambda function that returns t when a given node is a
  class.")

(defvar orkg-property-p (lambda (node) t)
  "Store a lambda function that returns t when a given node is a
  property.")

1(defvar orkg-instance-of-id nil
  "Store the id of the property which is used for setting the
  \"instance of\" property.")

(defvar orkg-class-id nil
  "Store the id of the node whose name is \"Class\".

The node whose id has been specified is used to mark other nodes
as being classes.")

(defvar orkg-subclass-of-id nil
  "Store the id of the \"subclass of\" property.

This property is used to identify classes.")

(defvar orkg-id-re "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}"
  "A regular expression that matches all the node IDs.

The value shouldn't include the id: part. It must match the ID of
any indexed node.

The default value was retrieved from the function `org-uuidgen-p'
from v9.4.4.")

(defun orkg-node-read-class ()
  "A similar function to `org-roam-node-read' but it only lists
those nodes whose evaluation of `orkg-class-p' returns t."
  (let* ((org-roam-node-display-template "${title-resolved-ids}")
         (node (org-roam-node-read nil orkg-class-p nil :require-match)))
    node))

(defun orkg-node-read-property ()
  "A similar function to `org-roam-node-read' but it only lists
those nodes whose evaluation of `orkg-property-p' returns t"
  (let* ((org-roam-node-display-template "${title-resolved-ids}")
         (node (org-roam-node-read nil orkg-property-p nil :require-match)))
    node))

(defun orkg-set-instance-of-property (&optional arg)
  "Set the \"instance of\" property for the node at point.

Preconditions:
+ The major mode of the current buffer is `org-mode' or derived."
  (interactive "p")
  (let* ((class-node (orkg-node-read-class))
         (class-id (org-roam-node-id class-node)))
    (when (and class-id orkg-instance-of-id)
      (setq class-id (concat "id:" class-id))
      (orkg-set-property arg orkg-instance-of-id class-id))))

(defun orkg-set-subclass-of-property (&optional arg)
  "Set the \"subclass of\" property for the node at point.

Preconditions:
+ The major mode of the current buffer is `org-mode' or derived."
  (interactive "p")
  (let* ((class-node (orkg-node-read-class))
         (class-id (org-roam-node-id class-node)))
    (when (and class-id orkg-subclass-of-id)
      (setq class-id (concat "id:" class-id))
      (orkg-set-property arg orkg-subclass-of-id class-id))))

(defun orkg-set-property (&optional arg property value)
  "Prompt for a property and a value and set it for the node at point. "
  (interactive "p")
  (let (node func)
    ;; Get the property
    (unless property
      (setq property (org-roam-node-id (orkg-node-read-property))))
    ;; Get the value
    (unless value
      (if (setq func (gethash property orkg-prompt-functions-hash))
          (setq value (funcall func))
        (progn
          ;; We don't require a match for the value, beacuse the value of a
          ;; property can be a literal string.
          (setq node (org-roam-node-read nil (lambda (node) (not (funcall orkg-property-p node)))))
          (cond ((org-roam-node-id node)
                 (setq value (concat "id:" (org-roam-node-id node))))
                (t
                 (setq value (org-roam-node-title node)))))))
    (cond
     ;; Set property to the current headline
     ((eq arg 4)
      (org-set-property property value))
     ;; Set property to the current node
     (t
      (save-excursion
        (setq node (org-roam-node-at-point))
        (goto-char (org-roam-node-point node))
        (org-set-property property value))))))

(defun orkg-id-at-point ()
  (let* (bounds id title)
    (cond ((setq bounds (org-in-regexp orkg-id-re))
           (setq id (downcase
                     (buffer-substring-no-properties
                      (car bounds)
                      (cdr bounds)))))
          ((setq bounds (org-in-regexp org-link-any-re))
           (setq link (buffer-substring-no-properties
                       (car bounds)
                       (cdr bounds)))
           (with-temp-buffer
             (org-mode)
             (insert link)
             (backward-char)
             (setq id (org-element-property :path (org-element-context))))))))

(defun orkg-resolve-ids-in-string (string)
  "Replace all IDs with their corresponding titles in a string.

If there's a node which references itself, then this will result
in an infinite loop."
  (let (bounds id)
    (with-temp-buffer
      (insert string)
      (goto-char 1)
      (while (re-search-forward orkg-id-re nil :noerror)
        (backward-char)
        (setq bounds (org-in-regexp orkg-id-re))
        (setq id (buffer-substring-no-properties (car bounds) (cdr bounds)))
        (delete-region (car bounds) (cdr bounds))
        (insert
         "["
         ;; If, for some reason, the ID is not shown, then show
         ;; nil. This to avoid throwing an error.
         (or (caar
              (org-roam-db-query
               [:select [title]
                        :from nodes
                        :where (= id $s1)]
               id))
             "nil")
         "]")
        (goto-char (car bounds)))
      (buffer-string))))

(defun orkg-visit-node-with-id-at-point ()
  (interactive)
  (let ((id (orkg-id-at-point)))
    (when id
      (org-roam-visit-node-with-id id))))

(defun orkg-show-title-id-at-point ()
  (interactive)
  (let ((id (orkg-id-at-point)))
    (unless id
      (error "The cursor at point is neither an id nor a link"))
    (setq title (orkg-resolve-ids-in-string id))
    (unless title
      (error "There doesn't exist a node with the id at point"))
    (posframe-show "foobar"
                   :string title
                   :timeout 1
                   :position (point))))

(defun orkg-show-title-id-buffer ()
  "Show the title of all the"
  (interactive)
  (save-excursion
    (save-restriction
      (let (bounds id)
        (goto-char 1)
        (while (re-search-forward orkg-id-re nil t)
          (backward-char)
          (setq bounds (org-in-regexp orkg-id-re))
          (setq id (buffer-substring-no-properties
                    (car bounds)
                    (cdr bounds)))
          (setq title
                (orkg-resolve-ids-in-string
                 (caar
                  (org-roam-db-query [:select title
                                              :from nodes
                                              :where (= id $s1)]
                                     id))))
          (posframe-show (format-time-string "%s.%N")
                         :string title
                         :position (point)))
        ;; After we press a key, all posframe frames are deleted.
        (read-char)
        (posframe-delete-all)))))

(defvar orkg-instance-of-icons nil
  "List of cons cells containing the ID of the classes and the
icons that are to be shown along for those entities.

After making a change to this variable, make sure to call
`orkg-hash-instance-of-icons-update' to make the changes visible.")

(defvar orkg-instance-of-icons-hash (make-hash-table :test 'equal)
  "A hash table containing the data stored in
  `orkg-instance-of-icons'.

The content of this hash table is updated by
`orkg-hash-instance-of-icons-update'.")

(defun orkg-instance-of-icons-hash-refresh ()
  "Updates the value of `orkg-hash-instance-of-icons' with the
values of `orkg-instance-of-icons'."
  (interactive)
  (clrhash orkg-instance-of-icons-hash)
  (dolist (items orkg-instance-of-icons)
    (cl-loop for index from 0
             for item in items
             do (unless (eq index 0)
                  (puthash item (car items) orkg-instance-of-icons-hash)))))

(defvar orkg-prompt-functions nil
  "List of lists. The first element in the list corresponds to
  the function. The next elements are the id of the properties
  where the function is to be used for prompting a value.")

(defvar orkg-prompt-functions-hash (make-hash-table :test 'equal)
  "A hash table containing the data stored in
  `orkg-instance-of-icons'.

The content of this hash table is updated by
`orkg-hash-instance-of-icons-update'.")

(defun orkg-prompt-functions-hash-refresh ()
  "Clear the value of `orkg-hash-prompt-functions' and includes
all the values in `orkg-prompt-functions'."
  (interactive)
  (clrhash orkg-prompt-functions-hash)
  (dolist (items orkg-prompt-functions)
    (cl-loop for index from 0
             for item in items
             do (unless (eq index 0)
                  (puthash item (car items) orkg-prompt-functions-hash)))))

(defun orkg-instance-of-icons-hash-refresh-subclasses ()
  "Includes icons of subclasses in `orkg-instance-of-icons-hash'

The function iterates through all classes and check whether any
of their superclasses exist in `orkg-instance-of-icons-hash'. If
so, the icon of the nearest superclass is used for the current
subclass. If the subclass is explicitly included in
`orkg-instance-of-icons', then that icon is used instead.

Precondition:
+ The function `orkg-instance-of-icons-hash-refresh' needs to be
executed. This because it consider the existing icons of the
superclasses."
  (let (properties subclass-of superclass)
    (dolist (node (org-roam-db-query [:select [id properties] :from nodes]))
      (catch 'continue
        (setq properties (nth 1 node))
        ;; If the class doesn't have a subclass, then skip it
        (unless (setq subclass-of
                      (cdr
                       (assoc
                        (upcase orkg-subclass-of-id) properties)))
          (throw 'continue t))
        ;; If the class have a subclass empty, then skip it.
        ;;
        ;; FIXME: Check that the entity is not a subclass of
        ;; itself. Having an empty property can be confusing and make
        ;; the user think that he forgot to put something into that
        ;; empty value.
        (when (equal subclass-of "")
          (throw 'continue t))
        (setq subclass-of (substring subclass-of 3))
        ;; If the class is included in orkg-instance-of-icons, then skip
        ;; it.
        (when (gethash (nth 0 node) orkg-instance-of-icons-hash)
          (throw 'continue t))
        ;; If it is a subclass of something, then search if any of the
        ;; superclasses have an icon.
        (while (and
                subclass-of
                (not (gethash subclass-of orkg-instance-of-icons-hash)))
          (setq superclass (org-roam-db-query
                            [:select [id properties]
                                     :from nodes
                                     :where (= id $s1)]
                            subclass-of))
          (setq subclass-of
                (cdr
                 (assoc
                  (upcase orkg-subclass-of-id) (nth 1 superclass))))
          (when subclass-of
            (setq subclass-of (substring subclass-of 3))))
        (when subclass-of
          (puthash (nth 0 node)
                   (gethash subclass-of orkg-instance-of-icons-hash)
                   orkg-instance-of-icons-hash))))))

(defun orkg-visit-instance-of-class ()
  "Prompt for a class, then for an instance and visit."
  (interactive)
  (let* ((class-id (org-roam-node-id (orkg-node-read-class)))
         (node (org-roam-node-read
                nil
                (lambda (node)
                  (string-match class-id
                                (or
                                 (cdr
                                  (assoc
                                   (upcase orkg-instance-of-id)
                                   (org-roam-node-properties node)))
                                 ""))) nil t)))
    (org-roam-node-visit node)))
