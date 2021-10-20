import request from '@/utils/request'
import { encrypt } from '@/utils/rsa-encrypt'

/**
 * 用户登录，获取token
 * @param {string} username 用户名
 * @param {string} password 密码 （请求加密）
 */
export function login({ username, password }) {
  return request({
    module: 'user',
    url: 'user/login',
    method: 'post',
    data: {
      username,
      password: encrypt(password)
    }
  })
}

/**
 * 获取用户信息
 * 用户登录后请求（token在请求头中）
 */
export function getInfo() {
  return request({
    module: 'user',
    url: 'user/info',
    method: 'get'
  })
}

/**
 * 登出
 */
export function logout() {
  return request({
    module: 'user',
    url: 'user/logout',
    method: 'delete'
  })
}

// 加载菜单
export function fetchMenus() {
  return request({
    module: 'user',
    url: 'user/menu',
    method: 'get'
  })
}
