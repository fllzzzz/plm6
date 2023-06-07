import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/exportTaxRebate/list',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/exportTaxRebate/save',
    method: 'post',
    data: data.list
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/exportTaxRebate/update',
    method: 'put',
    data
  })
}

export function del(ids) {
  return request({
    module: 'contract',
    url: `contract/exportTaxRebate/delete`,
    method: 'delete',
    data: ids
  })
}

export function editStatus(data) {
  return request({
    module: 'contract',
    url: 'contract/exportTaxRebate/confirm',
    method: 'put',
    data
  })
}

export default { get, add, edit, del }
