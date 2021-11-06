
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
