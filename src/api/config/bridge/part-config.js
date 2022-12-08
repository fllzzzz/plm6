import request from '@/utils/request'

// 获取钢材对应材料配置的简要信息
export function getSteelClassifyConfBrief() {
  return request({
    url: '/api/bridge/steel-class/all/brief',
    method: 'get'
  })
}

export function get(params) {
  return request({
    url: '/api/bridge/steel-class',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    url: '/api/bridge/steel-class',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    url: '/api/bridge/steel-class',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    url: '/api/bridge/steel-class',
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
