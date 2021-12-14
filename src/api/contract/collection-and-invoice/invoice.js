import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'contract/invoice/listPage',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'contract/invoice/save',
    method: 'post',
    data
  })
}

export default { get, add }