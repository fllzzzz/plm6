import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/listPage',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/collection/save',
    method: 'post',
    data
  })
}

export function edit(data) {
  return request({
    module: 'contract',
    url: 'contract/collection/edit',
    method: 'post',
    data
  })
}

export function contractCollectionInfo(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/getFinanceInfo',
    method: 'get',
    params,
    cancelKey: false
  })
}

export function editStatus(id, status) {
  return request({
    module: 'contract',
    url: `contract/collection/audit/collectionId/${id}/status/${status}`,
    method: 'put'
  })
}

export default { get, add }
