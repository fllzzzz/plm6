import useCheckPermission from '@compos/use-check-permission'

// 验证权限
export default {
  mounted(el, binding) {
    const { value } = binding
    const passable = useCheckPermission(value)
    if (passable) {
      return true
    } else {
      // 权限未通过则移除当前DOM
      el.parentNode && el.parentNode.removeChild(el)
    }
  }
}
