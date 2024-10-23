pct_change <-
function (x){
  (x/lag(x) - 1) * 100
}
