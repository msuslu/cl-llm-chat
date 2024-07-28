(defpackage llm-chat
  (:use #:cl)
  (:export #:llm-chat #:anyscale-completion #:get-completion
		   #:deftool))

(in-package #:llm-chat)

(defvar *tools* (make-hash-table :test 'equalp))

(defclass llm-chat ()
  ()
  (:documentation "Base class for completions"))

(defclass anyscale-completion (llm-chat)
  ((endpoint  :initform "https://api.endpoints.anyscale.com/v1/chat/completions")
   (api-key :initarg :api-key)
   (model :initform "meta-llama/Meta-Llama-3-8B-Instruct" :initarg :model)
   (tools :initarg :tools :initform nil)
   (parameters :initarg :parameters :initform nil)
   (msgs :initform (list '(("role" . "system") ("content" . "You are a helpful assistant")))))
  (:documentation "Anyscale completion"))

(defclass function-tool ()
  ((name :initarg :name)
  (fn :initarg :fn)
  (parameters :initarg :parameters)
  (description :initarg :description))
  (:documentation "LLM tool function"))

(defmethod get-tool ((tool function-tool))
  (with-slots (name fn parameters description) tool
	`(("type" . "function")
	  ("function" . (("name" . ,name)
					 ("description" . ,description)
					 ,(when parameters
						`("parameters" . (("type" . "object")
										 ("properties" . ,(loop for p in parameters
															   collect (cons (first p)
																			 (list
																			  (cons "type" (string-downcase (second p)))
																			  (cons "description" (third p))))))
										 ("required" . ,(loop for p in parameters
															 collect (first p)))))))))))

(defmacro deftool (name args description &rest body)
  (unless (listp args)
	(error "ARGS must be a list."))
  (unless (stringp description)
	(error "DESCRIPTION must be a string."))
  (dolist (arg args)
	(unless (member (second arg) '(string number boolean) :test #'equal)
	  (error "Unsupported deftool argument type: ~a" (second arg))))
  ;; Generate code
  (let ((name-str (if (symbolp name) (symbol-name name) name))
		(arg-names (mapcar #'first args)))
	`(progn
	   ;; Define and set the function in *tools*
	   (setf (gethash ,name-str *tools*)
			 (make-instance 'function-tool
							:name ,name-str
							:description ,description
							:parameters ',args
							:fn (lambda (&key ,@arg-names)
								  ,@body))))))


(defmethod get-completion ((completer anyscale-completion) prompt)
  (with-slots (endpoint api-key model msgs tools parameters) completer
	(let ((resp (dex:post endpoint
						  :headers `((:authorization . ,(format nil "Bearer ~a" api-key))
									 (:content-type . "application/json"))
						  :content (json:encode-json-alist-to-string
										   `(("model" . ,model)
											 ("messages" .
														 (,@(loop for msg in msgs
																  collect msg)
															(("role" . "user")
															 ("content" . ,prompt))))
											 ,(if tools
												`("tools" . ,(loop for tool-sym in tools
																   collect (let ((tool (gethash (symbol-name tool-sym) *tools*)))
																			 (if tool
																				 (get-tool tool)
																				 (error "Tool ~a not found" tool-sym))))))
											 ,(if parameters
													   (loop for param in parameters
														collect (cons (first param)
																	  (second param)))))
										   ))))
	  (let* ((res (json:decode-json-from-string resp))
			 (choices (cdr (assoc :choices res))))
		(if choices
			(let* ((msg (cdr (assoc :message (first choices))))
				   (content (cdr (assoc :content msg)))
				   (tool_calls (cdr (assoc :tool_calls msg)))
				   (role (cdr (assoc :role msg))))
				(setf msgs (append msgs `((("role" . "user") ("content" . ,prompt)))))
				(if content
					(progn
					(setf msgs (append msgs `((("role" . ,role) ("content"  . ,content)))))
					(values content))
				  (if tool_calls
					  (loop for tool in tool_calls
							do (let* ((tool-id (getf tool :id))
									  (tool-func (getf tool :function))
									  (tool-name (getf tool-func :name))
									  (tool-args (json:decode-json-from-string (getf tool-func :arguments)))
									  (tool-obj (gethash tool-name *tools*)))
								 (if tool-obj
									 (let* ((fn (slot-value tool-obj 'fn))
											(res (apply fn (loop for arg in tool-args
																 collect arg))))
									   (progn
										 (setf msgs (append msgs `((("role" . "tool")
																   ("content" . ,res)
																   ("tool_call_id" . ,tool-id)
																   ("name" . ,tool-name)))))
										 (return res)))
									 (error "Tool ~a not found" tool-name)))))))
			(values "I'm sorry, I don't know what to say." res ))))))



;; (defparameter *completer* (make-instance 'anyscale-completion
;; 										 :model "mistralai/Mistral-7B-Instruct-v0.1"
;; 										 :api-key *token*
;; 										 :tools '(get-temp)))

;; (get-completion *completer* "What is the weather in San Francisco?")
