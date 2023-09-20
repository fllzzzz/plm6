import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'system/structure-classification',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'system/structure-classification',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'system/structure-classification',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: 'system/structure-classification',
    method: 'delete',
    data: ids
  })
}

// 构件特征定义修改审核
export function audit(data) {
  return request({
    module: 'contract',
    url: 'system/structure-classification/auditUpdate',
    method: 'put',
    data
  })
}

export default { get, add, edit, del }
