import store from '@/store'

/**
 * @param {Array} value
 * @returns {Boolean}
 * @example see @/views/permission/directive.vue
 */
export default function checkPermission(value) {
  console.log('value', value)
  if (Array.isArray(value) && value.length > 0) {
    const roles = store.getters && store.getters.roles
    const permissionRoles = ['admin', ...value]
    const hasPermission = roles.some(role => {
      return permissionRoles.includes(role)
    })
    if (!hasPermission) {
      return false
    }
    return true
  } else {
    console.error('权限解析失败')
    return false
  }
}
