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

function isLocked(node) {
  return (
    node === document.pointerLockElement ||
    node === document.mozPointerLockElement ||
    node === document.webkitPointerLockElement
  );
}

function isFullscreen(node) {
  return (
    document.webkitFullscreenElement === node ||
    document.mozFullscreenElement === node ||
    document.mozFullScreenElement === node
  );
}

function setup(component, node) {
  function request() {
    document.addEventListener('pointerlockchange', pointerLockChange, false);
    document.addEventListener('mozpointerlockchange', pointerLockChange, false);
    document.addEventListener('webkitpointerlockchange', pointerLockChange, false);

    var requestPointerLock = (
      node.requestPointerLock  ||
      node.mozRequestPointerLock ||
      node.webkitRequestPointerLock
    );
    requestPointerLock.call(node);
  }

  function exit() {
    var exitPointerLock = (
      document.exitPointerLock  ||
      document.mozExitPointerLock ||
      document.webkitExitPointerLock
    );
    exitPointerLock.call(document);
    document.removeEventListener('pointerlockchange', pointerLockChange);
    document.removeEventListener('mozpointerlockchange', pointerLockChange);
    document.removeEventListener('webkitpointerlockchange', pointerLockChange);
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

  function move(e) {
    var movementX = e.movementX || e.mozMovementX || e.webkitMovementX || 0;
    var movementY = e.movementY || e.mozMovementY || e.webkitMovementY || 0;
    component.ports.movement.send([movementX, movementY]);
  }

  if (component.ports.requestPointerLock) {
    component.ports.requestPointerLock.subscribe(request);
  }
  if (component.ports.exitPointerLock) {
    component.ports.exitPointerLock.subscribe(exit);
  }

  component.requestPointerLock = request;
  component.exitPointerLock = exit;

  return component;
}

}())
