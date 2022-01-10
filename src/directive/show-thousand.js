// 千分位数值
export default {
  mounted(el, binding) {
    resolve(el, binding)
  }
}

function resolve(el, binding) {
  if (el.tagName.toLocaleUpperCase() !== 'INPUT') {
    el = el.getElementsByTagName('input')[0]
  }
  // 保留2位小数+千分位
  if (el.value) {
    el.value = parseFloat(el.value).toLocaleString(undefined, { 'minimumFractionDigits': 2, 'maximumFractionDigits': 2 })
  }
  el.onblur = (e) => {
    const a = el.value.replace(/,/g, '')
    if (a) {
      el.value = parseFloat(el.value).toLocaleString(undefined, { 'minimumFractionDigits': 2, 'maximumFractionDigits': 2 })
    }
  }
}
