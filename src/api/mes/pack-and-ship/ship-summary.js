import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: `cargo/project/summary`,
    method: 'get',
    params
  })
}

// 产品清单列表
export function inboundDetail(params) {
  return request({
    url: ``,
    method: 'get',
    params
  })
}

// 发运汇总
export function shipmentSummary(params) {
  return request({
    module: 'mes',
    url: `cargo/year/summary`,
    method: 'get',
    params
  })
}

export default { get }
