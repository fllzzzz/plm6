import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'project',
    url: `data-image`,
    method: 'get',
    params
  })
}

export default { get }
