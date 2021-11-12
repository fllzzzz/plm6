// Map 转 数组
export function map2arr(map) {
  const arr = []
  for (const [key, value] of map) {
    arr.push({
      key,
      value
    })
  }
  return arr
}
