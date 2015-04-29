(function() {

Elm.embedWithPointerLock = function(module, div, args) {
  var component = Elm.embed(module, div, initArgs(args));
  return setup(component, div);
}

Elm.fullscreenWithPointerLock = function(module, args) {
  var component = Elm.fullscreen(module, initArgs(args));
  return setup(component, document.body);
}

function initArgs(args) {
  args = args || {};
  args.movement = [0,0];
  args.isLocked = false;
  return args;
}

function setup(component, node) {
  var supportsPointerLock = true;
  var REQUEST_POINTER_LOCK;
  var EXIT_POINTER_LOCK;
  var POINTER_LOCK_ELEMENT;
  var POINTER_LOCK_CHANGE;
  var MOVEMENT_X;
  var MOVEMENT_Y;

  if (node.requestPointerLock) {
    REQUEST_POINTER_LOCK = 'requestPointerLock';
    EXIT_POINTER_LOCK = 'exitPointerLock';
    POINTER_LOCK_ELEMENT = 'pointerLockElement';
    POINTER_LOCK_CHANGE = 'pointerlockchange';
    MOVEMENT_X = 'movementX';
    MOVEMENT_Y = 'movementY';
  } else if (node.mozRequestPointerLock) {
    REQUEST_POINTER_LOCK = 'mozRequestPointerLock';
    EXIT_POINTER_LOCK = 'mozExitPointerLock';
    POINTER_LOCK_ELEMENT = 'mozPointerLockElement';
    POINTER_LOCK_CHANGE = 'mozpointerlockchange';
    MOVEMENT_X = 'mozMovementX';
    MOVEMENT_Y = 'mozMovementY';
  } else if (node.webkitRequestPointerLock) {
    REQUEST_POINTER_LOCK = 'webkitRequestPointerLock';
    EXIT_POINTER_LOCK = 'webkitExitPointerLock';
    POINTER_LOCK_ELEMENT = 'webkitPointerLockElement';
    POINTER_LOCK_CHANGE = 'webkitpointerlockchange';
    MOVEMENT_X = 'webkitMovementX';
    MOVEMENT_Y = 'webkitMovementY';
  } else {
    supportsPointerLock = false;
  }

  function isLocked(node) {
    return node === document[POINTER_LOCK_ELEMENT];
  }

  // REAL HANDLERS

  function request() {
    document.addEventListener(POINTER_LOCK_CHANGE, pointerLockChange, false);
    node[REQUEST_POINTER_LOCK]();
  }

  function exit() {
    document[EXIT_POINTER_LOCK]();
    document.removeEventListener(POINTER_LOCK_CHANGE, pointerLockChange);
  }

  function move(e) {
    component.ports.movement.send([e[MOVEMENT_X], e[MOVEMENT_Y]]);
  }

  function pointerLockChange() {
    if (isLocked(node)) {
      node.addEventListener('mousemove', move);
      component.ports.isLocked.send(true);
    } else {
      node.removeEventListener('mousemove', move);
      component.ports.isLocked.send(false);
    }
  }

  // POLYFILLED HANDLERS

  function polyfillRequest() {
    document.body.style.cursor = 'none';
    node.addEventListener('mousemove', polyfillMove);
    component.ports.isLocked.send(true);
  }

  function polyfillExit() {
    document.body.style.cursor = 'pointer';
    node.removeEventListener('mousemove', polyfillMove);
    component.ports.isLocked.send(false);
  }

  var prevX = 0;
  var prevY = 0;

  function polyfillMove(e) {
    var clientX = e.clientX;
    var clientY = e.clientY;
    component.ports.movement.send([clientX - prevX, clientY - prevY]);
    prevX = clientX;
    prevY = clientY;
  }

  var requestPointerLock;
  var exitPointerLock;

  if (supportsPointerLock) {
    requestPointerLock = request;
    exitPointerLock = exit;
  } else {
    requestPointerLock = polyfillRequest;
    exitPointerLock = polyfillExit;
  }

  if (component.ports.requestPointerLock) {
    component.ports.requestPointerLock.subscribe(requestPointerLock);
  }
  if (component.ports.exitPointerLock) {
    component.ports.exitPointerLock.subscribe(exitPointerLock);
  }
  component.requestPointerLock = requestPointerLock;
  component.exitPointerLock = exitPointerLock;

  return component;
}

}())
