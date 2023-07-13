import request from '@/utils/request'

// 获取全部菜单模块
export function getMenuModule() {
  return request({
    url: '/api/menu/list-module',
    method: 'get'
  })
}

// 获取所有模块小数精度
export function getAllDecimal() {
  return request({
    url: '/api/system/module-decimal-scale/all',
    method: 'get'
  })
}

export function get() {
  return request({
    url: '/api/system/module-decimal-scale',
    method: 'get'
  })
}

export function add(data) {
  return request({
    url: '/api/system/module-decimal-scale',
    method: 'post',
    data: data.list
  })
}

export function batchAdd(data) {
  return request({
    url: '/api/system/module-decimal-scale',
    method: 'post',
    data
  })
}

export function del(ids) {
  return request({
    url: '/api/system/module-decimal-scale',
    method: 'delete',
    data: ids
  })
}

export default { get, batchAdd, add, del }
