import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'system/assemble-config',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'system/assemble-config',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'system/assemble-config',
    method: 'put',
    data
  })
}

export function del(data) {
  return request({
    module: 'contract',
    url: 'system/assemble-config',
    method: 'delete',
    data
  })
}

// 部件特征定义修改审核
export function audit(data) {
  return request({
    module: 'contract',
    url: 'system/assemble-config/auditUpdate',
    method: 'put',
    data
  })
}

export default { get, add, edit, del }
