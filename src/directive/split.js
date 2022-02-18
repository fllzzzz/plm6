// 时间转换
export default {
  mounted(el, binding) {
    resolve(el, binding)
  },
  updated(el, binding) {
    resolve(el, binding)
  }
}

function resolve(el, binding) {
  const { value } = binding
  let val
  let symbol
  if (Array.isArray(value)) {
    val = value
  } else if (value && typeof value === 'object') {
    val = value.val
    symbol = value.symbol
  } else {
    val = value
  }
  // 非数组返回
  if (!Array.isArray(val)) {
    el.innerText = val
    return
  }
  symbol = symbol || '、'
  el.innerText = val.join(symbol)
}
