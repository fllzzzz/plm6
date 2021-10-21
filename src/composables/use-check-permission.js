import store from '@/store'
import { isBlank } from '@/utils/data-type'
// admin 拥有所有权限
const fixedRoles = ['admin']

// 校验权限
export default function useCheckPermission(value) {
  // 没有权限返回通过
  if (isBlank(value)) return true
  if (value instanceof Array && value.length > 0) {
    // 获取当前用户权限
    const roles = store.getters.roles
    // 加入固定权限
    const permissionRoles = [...fixedRoles, ...value]
    // 判断是否拥有权限
    const hasPermission = roles.some(role => {
      return permissionRoles.includes(role)
    })
    if (!hasPermission) {
      return false
    }
    return true
  } else {
    throw new Error('权限解析失败，请传入数组结构')
  }
}
