
const State = {
  MultiSing: 0,
  Multi:     1,
  Sing:      2,
  FptpSing:  3
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
    case State.Sing:
      var cur = document.getElementById("singleSys");
      var selected = document.getElementById("singSys");
      var next;
      if (selected.value == "fptp") {
        state = State.FptpSing;
        next = document.getElementById("fptpSing");
      } else {
        break;
      }
      cur.hidden = true;
      next.hidden = false;
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
    case State.FptpSing:
      var prev = document.getElementById("singleSys");
      var cur  = document.getElementById("fptpSing");
      state = State.Sing;
      prev.hidden = false;
      cur.hidden = true;
  }
}

var fptpSingCount = 0;

fptpSingAddBtn.onclick = function() {
  var el = document.createElement('input');
  var br = document.createElement('br');
  el.type = 'text';
  el.name = 'fptpSingCand' + fptpSingCount;
  el.id = 'fptpSingCand' + fptpSingCount;
  br.id = 'fptpSingBr' + fptpSingCount;
  el.classList.add("text-input");
  var div = document.getElementById('candidates');
  div.appendChild(el);
  div.appendChild(br);
  fptpSingCount++;
}

fptpSingRemBtn.onclick = function() {
  if (fptpSingCount == 0) return;
  fptpSingCount--;
  document.getElementById('fptpSingCand' + fptpSingCount).remove();
  document.getElementById('fptpSingBr' + fptpSingCount).remove();
}
