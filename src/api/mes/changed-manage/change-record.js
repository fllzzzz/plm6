import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'tech/change/page',
    method: 'get',
    params
  })
}

export function detail(id) {
  return request({
    module: 'mes',
    url: `tech/change/${id}`,
    method: 'get'
  })
}

export default { get, detail }
