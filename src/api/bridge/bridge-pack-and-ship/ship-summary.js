import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'bridge',
    url: `cargo/project/summary`,
    method: 'get',
    params
  })
}

// 项目发运详情
export function inboundDetail(params) {
  return request({
    module: 'bridge',
    url: `cargo/project/cargoList/details/page`,
    method: 'get',
    params
  })
}

// 发运汇总
export function shipmentSummary(params) {
  return request({
    module: 'bridge',
    url: `cargo/year/summary`,
    method: 'get',
    params
  })
}

export default { get }
