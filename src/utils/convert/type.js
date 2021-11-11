/**
 * 数组转对象
 * @param {*} arr 需要处理的数组
 * @param {*} mark 唯一标识 作为对象key
 * @returns
 */
export function arr2obj(arr, mark = 'id') {
  const newObj = {}
  for (const item of arr) {
    newObj[item[mark]] = item
  }
  return newObj
}

/**
 * 对象转数组
 * @param {*} obj 需要处理的对象
 * @returns
 */
export function obj2arr(obj) {
  const newArr = []
  for (const key in obj) {
    if (obj[key]) {
      newArr.push(obj[key])
    }
  }
  return newArr
}
