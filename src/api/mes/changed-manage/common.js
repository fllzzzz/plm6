import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: 'abnormal/page',
    method: 'get',
    params
  })
}

export function taskList({ productId, productType }) {
  return request({
    module: 'mes',
    url: 'abnormal/task',
    method: 'get',
    params: { productId, productType }
  })
}

export default { get }
