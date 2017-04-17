(in-package :mortar-combat)


(defun asset-path (file)
  (merge-pathnames file (merge-working-pathname (property :assets "assets/"))))
