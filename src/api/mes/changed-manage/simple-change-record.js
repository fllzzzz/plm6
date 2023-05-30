import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'plan',
    url: 'artifact/change',
    method: 'get',
    params
  })
}

export function check({ id, handleStatusEnum }) {
  return request({
    module: 'plan',
    url: `artifact/change/handle/${id}/${handleStatusEnum}`,
    method: 'put'
  })
}

export default { get }
