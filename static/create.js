
const State = {
  MultiSing: 0,
  Multi:     1,
  Sing:      2
}

var state = State.MultiSing;

nextBtn.onclick = function() {
  switch (state) {
    case State.MultiSing:
      var selected = document.getElementById("singMulti");
      var prevBtn = document.getElementById("previousBtn");
      var old = document.getElementById("elecType");
      var neww; 
      if (selected.value == "single") {
        neww = document.getElementById("singleSys");
        state = State.Sing;
      } else {
        neww = document.getElementById("multiSys");
        state = State.Multi;
      }
      old.hidden = true;
      neww.hidden = false;
      prevBtn.hidden = false;
      break;
  }
}

previousBtn.onclick = function() {
  switch (state) {
    case State.Multi:
    case State.Sing:
      var hid1 = document.getElementById("singleSys");
      var hid2 = document.getElementById("multiSys");
      var sho  = document.getElementById("elecType");
      var btn  = document.getElementById("previousBtn");
      hid1.hidden = true;
      hid2.hidden = true;
      sho.hidden  = false;
      btn.hidden = true;
      state = State.MultiSing;
      break;
  }
}
