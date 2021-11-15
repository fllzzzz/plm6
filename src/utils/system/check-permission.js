import store from '@/store'
import { isBlank } from '@data-type/index'
// admin 拥有所有权限
const fixedRoles = ['admin']

/**
 * 校验权限
 * @param {array} value 权限
 * @returns
 */
export default function checkPermission(value) {
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
