WORK IN PROGRESS

# cl-llm-chat

## Usage

(defvar *token* (uiop:getenv "ANYSCALE_TOKEN"))

(defparameter chat (make-instance 'llm-chat:anyscale-completion :api-key *token*))

(llm-chat:get-completion chat "Hello! How are you?")

## Installation
