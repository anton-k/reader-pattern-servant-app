.PHONY: build, test, run

build:
	stack build

test:
	stack test

run:
	stack run

# call:
#
# make message='{"message": "waiting for the summer", "tags": ["random"] }' post-save
post-save:
	curl http://localhost:7070/api/v1/save  -d '$(message)' -v -H "Content-Type: application/json"

# call:
# > make id=0 get-id
get-id:
	curl http://localhost:7070/api/v1/get/message/$(id) -v

# call:
# > make tag=info get-tag
get-tag:
	curl http://localhost:7070/api/v1/get/tag/$(tag) -v

toggle-log:
	curl http://localhost:7070/api/v1/toggle-logs -d '{}' -v -H "Content-Type: application/json"



