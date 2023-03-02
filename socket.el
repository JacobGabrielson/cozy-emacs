
(defstruct socket host port (buffer nil) (process nil) (finalizer nil))

(defun socket-close (socket)
  (unless (null (socket-process socket))
    (delete-process (socket-process socket)))
  (when (buffer-live-p (socket-buffer socket))
    (kill-buffer (socket-buffer socket))))

(defun socket-connect (socket)
  (setf (socket-finalizer socket) (make-finalizer #'(lambda ()
						      (socket-close socket))))
  (let* ((buffer (generate-new-buffer " *socket*")))
      (setf (socket-buffer socket) buffer)
      (setf (socket-process socket)
	    (make-network-process :name "socket"
				  :buffer buffer
				  :host (socket-host socket)
				  :service (socket-port socket)))))

(defun socket-send (socket string)
  (process-send-string (socket-process socket) string))

(defun socket-recv (socket)
  (with-current-buffer (socket-buffer socket)
    (let ((beg (point-min))
	  (end (point-max)))
      (prog1 (buffer-substring beg end)
	(delete-region beg end)))))
