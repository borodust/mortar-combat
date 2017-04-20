(in-package :mortar-combat)


(defvar *identity-mat4* (identity-mat4))


(defun asset-path (file)
  (merge-pathnames file (merge-working-pathname (property :assets "assets/"))))
