// Generated by purs bundle 0.14.0
var PS = {};
(function(exports) {
  "use strict";

  exports.parseIntImpl = function (rawString) {
      return function (just) {
          return function (nothing) {
              const num = parseInt(rawString, 10);
              return Number.isInteger(num) ? just(num) : nothing;
          }
      }
  }
})(PS["Data.Gospel"] = PS["Data.Gospel"] || {});
(function(exports) {
  "use strict";

  exports.showIntImpl = function (n) {
    return n.toString();
  };
})(PS["Data.Show"] = PS["Data.Show"] || {});
(function($PS) {
  // Generated by purs version 0.14.0
  "use strict";
  $PS["Data.Show"] = $PS["Data.Show"] || {};
  var exports = $PS["Data.Show"];
  var $foreign = $PS["Data.Show"];
  var Show = function (show) {
      this.show = show;
  };                                                 
  var showInt = new Show($foreign.showIntImpl);
  var show = function (dict) {
      return dict.show;
  };
  exports["Show"] = Show;
  exports["show"] = show;
  exports["showInt"] = showInt;
})(PS);
(function($PS) {
  // Generated by purs version 0.14.0
  "use strict";
  $PS["Data.Maybe"] = $PS["Data.Maybe"] || {};
  var exports = $PS["Data.Maybe"];
  var Data_Show = $PS["Data.Show"];                
  var Nothing = (function () {
      function Nothing() {

      };
      Nothing.value = new Nothing();
      return Nothing;
  })();
  var Just = (function () {
      function Just(value0) {
          this.value0 = value0;
      };
      Just.create = function (value0) {
          return new Just(value0);
      };
      return Just;
  })();
  var showMaybe = function (dictShow) {
      return new Data_Show.Show(function (v) {
          if (v instanceof Just) {
              return "(Just " + (Data_Show.show(dictShow)(v.value0) + ")");
          };
          if (v instanceof Nothing) {
              return "Nothing";
          };
          throw new Error("Failed pattern match at Data.Maybe (line 216, column 1 - line 218, column 28): " + [ v.constructor.name ]);
      });
  };
  exports["Nothing"] = Nothing;
  exports["Just"] = Just;
  exports["showMaybe"] = showMaybe;
})(PS);
(function($PS) {
  "use strict";
  $PS["Data.Gospel"] = $PS["Data.Gospel"] || {};
  var exports = $PS["Data.Gospel"];
  var $foreign = $PS["Data.Gospel"];
  var Data_Maybe = $PS["Data.Maybe"];
  var $$parseInt = function (s) {
      return $foreign.parseIntImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value)(s);
  };
  exports["parseInt"] = $$parseInt;
})(PS);
(function(exports) {
  "use strict";

  exports.log = function (s) {
    return function () {
      console.log(s);
    };
  };
})(PS["Effect.Console"] = PS["Effect.Console"] || {});
(function($PS) {
  // Generated by purs version 0.14.0
  "use strict";
  $PS["Effect.Console"] = $PS["Effect.Console"] || {};
  var exports = $PS["Effect.Console"];
  var $foreign = $PS["Effect.Console"];
  var Data_Show = $PS["Data.Show"];
  var logShow = function (dictShow) {
      return function (a) {
          return $foreign.log(Data_Show.show(dictShow)(a));
      };
  };
  exports["logShow"] = logShow;
})(PS);
(function($PS) {
  "use strict";
  $PS["Main"] = $PS["Main"] || {};
  var exports = $PS["Main"];
  var Data_Gospel = $PS["Data.Gospel"];
  var Data_Maybe = $PS["Data.Maybe"];
  var Data_Show = $PS["Data.Show"];
  var Effect_Console = $PS["Effect.Console"];                
  var main = function __do() {
      Effect_Console.logShow(Data_Maybe.showMaybe(Data_Show.showInt))(Data_Gospel["parseInt"]("3"))();
      return Effect_Console.logShow(Data_Maybe.showMaybe(Data_Show.showInt))(Data_Gospel["parseInt"]("345"))();
  };
  exports["main"] = main;
})(PS);
PS["Main"].main();