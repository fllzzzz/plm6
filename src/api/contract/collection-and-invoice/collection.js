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

export function contractCollectionInfo(params) {
  return request({
    module: 'contract',
    url: 'contract/collection/getFinanceInfo',
    method: 'get',
    params
  })
}

export default { get, add }