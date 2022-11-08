# Haskell Reader pattern and servant example app

Show case for a small application written with servant and [Reader-pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern/).
It is example on how to make Dependency Injection in Haskell with Reader-patter.

Application allows to save messages with tags. 
User can create a new message and then user can query it by id
or by tag.

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

Applications shows how to  create interfaces for mocks and real instances.
Also we show how to use interfaces that depen on run-time data 
and how we can split the top-level interface to smaller ones didicated to concrete methods.

See makefile for available actions.

