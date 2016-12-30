
def revers(s: String): String = {
  s.foldLeft("")((x, y) => y + x)
}
revers("asfggg")

def ascend(s: String): String = {
  s.foldLeft("")((x, y) => y + x)
}

ascend("1 3 2 4 5 6 3 1")

"1 3 2 4 5 6 3 1".split(" ").toList.foldLeft("")((x, y) => {
  if (x.isEmpty || x.tail < y) {
    x + y
  } else {
    x + "," + y
  }
})
