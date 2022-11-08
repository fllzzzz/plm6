import request from '@/utils/request'

export function get(params) {
  return request({
    url: '/api/mes/building/warehouse/finish-product/page',
    method: 'get',
    params
  })
}

export function detail(params) {
  return request({
    url: '/api/mes/building/warehouse/finish-product/detail',
    method: 'get',
    params
  })
}

export function summaryData(params) {
  return request({
    url: '/api/mes/building/warehouse/finish-product/summary',
    method: 'get',
    params
  })
}

export default { get }
