import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'contract',
    url: 'config/listMarkLetter',
    method: 'get',
    params
  })
}

export function add(data) {
  return request({
    module: 'contract',
    url: 'config/setMarkLetter',
    method: 'post',
    data
  })
}

export default { get, add }
