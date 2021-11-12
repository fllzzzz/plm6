
// 判断元素是否可以滚动
export function elHScrollable(el) {
  if (!(el instanceof HTMLElement)) {
    return
  }
  if (el.scrollTop > 0) {
    return true
  } else {
    el.scrollTop++
    // 元素不能滚动的话，scrollTop 设置不会生效，还会置为 0
    const left = el.scrollTop
    // 重置滚动位置
    left && (el.scrollTop = 0)
    return left > 0
  }
}

// 获取文字类型dom宽度
// TODO: 在不同的盒子中默认的字体宽度之类的都不同，默认插入在body中，width会有问题。
export function getTextDomWidth(str = '', { attribute } = {}) {
  const dom = document.createElement('span')
  attribute && attribute.forEach((val, key) => {
    dom.setAttribute(key, val)
  })
  dom.style.display = 'inline-block'
  dom.textContent = str
  document.body.appendChild(dom)
  const width = dom.clientWidth
  document.body.removeChild(dom)
  return width
}
