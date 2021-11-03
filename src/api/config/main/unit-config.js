import request from '@/utils/request'

// 获取所有单位
export function getAllUnit() {
  return request({
    module: 'config',
    url: 'base-config/unit/all',
    method: 'get'
  })
}

export function get(params) {
  return request({
    module: 'config',
    url: 'base-config/unit',
    method: 'get',
    params
  })
}

export function batchAdd(data) {
  return request({
    module: 'config',
    url: 'base-config/unit/batch',
    method: 'post',
    data
  })
}

export function editEnabled(data) {
  return request({
    module: 'config',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: `base-config/unit/enabled`,
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'config',
    url: 'base-config/unit',
    method: 'delete',
    data: ids
  })
}

export default { get, batchAdd, del }
