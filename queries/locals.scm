
(root)                                   @local.scope

(defProc)                                @local.scope
(interface   (declProc)                  @local.scope)
(interface   (declFunc)                  @local.scope)
(declSection (declProc)                  @local.scope)
(declSection (declFunc)                  @local.scope)
(declClass   (declProc)                  @local.scope)
(declClass   (declFunc)                  @local.scope)
(declHelper  (declProc)                  @local.scope)
(declHelper  (declFunc)                  @local.scope)
(declProcRef)                            @local.scope
(declFuncRef)                            @local.scope

(exceptionHandler)                       @local.scope
(exceptionHandler variable: (identifier) @local.definition)

(declArg          name: (identifier)     @local.definition)
(declVar          name: (identifier)     @local.definition)
(declConst        name: (identifier)     @local.definition)
(declLabel        name: (identifier)     @local.definition)
(genericArg       name: (identifier)     @local.definition)
(declEnumValue    name: (identifier)     @local.definition)
(declType         name: (identifier)     @local.definition)
(declType         name: (genericTpl entity: (identifier)     @local.definition))

(declProc         name: (identifier)     @local.definition)
(declFunc         name: (identifier)     @local.definition)

(identifier)                             @local.reference
