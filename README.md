# Summary

This is a project showcasing the application of the [Reader pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/)
in an implementation of a web-service.
Heavily focuses on codebase decoupling and provides for injection of mocking functionality in various layers of the app.

## Functionality

The application allows to save messages with tags. 
The user can create a new message and then query it by id or by tag.

API methods:

```
POST: api/v1/save:
   input: JSON text of the message and list of tags
   output: id of the message 

GET: api/v1/get/message/{message-id}
  input: id of the message
  output: message or error  

GET: api/v1/list/tag/{tag} 
  input: tag 
  output: list of messages that belong to the tag

POST: api/v1/toggle-logs
  toggles the logs (active or silent)
```

See [Makefile](Makefile) for available actions.
