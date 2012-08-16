(function() {

  Signal = function(f) {
    var s = {};
    
    s.evaluate = f;
    this.sources = [];
    this.inputs = [];
    for (var i = 1; i < arguments.length; i++) {
      this.sources.push(arguments[i]);
    }
    for (var i = 0; i < this.sources.length; i++) {
      this.sources[i].subscribe(function(value) {
        this.inputs[i] = value;
        this.update(this.evaluate.apply(this, inputs));
      }
    }

    s.subscribe = function(subscriber) {
      this.subscribers.push(subscriber);
    }
    s.update = function(next) {
      this.value = next;
      for (var i = 0; i < subscribers.length; i++) {
        subscribers[i](this.value);
      }
    }

    return s;
  }

})();