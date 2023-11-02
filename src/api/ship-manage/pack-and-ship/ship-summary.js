import request from '@/utils/request'

export function get(params) {
  return request({
    module: 'mes',
    url: `cargo/project/summary`,
    method: 'get',
    params
  })
}

// 项目发运详情
export function inboundDetail(params) {
  return request({
    url: `/api/mes/building/cargo/project/cargoList/details/page`,
    method: 'get',
    params
  })
}

// 配套件发运详情
export function auxInboundDetail(params) {
  return request({
    url: `/api/mes/building/cargo/project/cargoList/auxiliary/details/page`,
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

// 查询项目发运数据结构汇总
export function projectSummary(params) {
  return request({
    module: 'mes',
    url: `cargo/project/detail/summary`,
    method: 'get',
    params
  })
}

// 查询项目直发件发运数据
export function structureDirectSummary(params) {
  return request({
    module: 'mes',
    url: `cargo/project/detail/directShipment/summary`,
    method: 'get',
    params
  })
}

// 发运统计-查询项目各类数据详情
export function summaryDetail(params) {
  return request({
    module: 'mes',
    url: `cargo/project/detail/type`,
    method: 'get',
    params
  })
}

// 发运统计-查询项目各类数据详情
export function structureDirectSummaryDetail(params) {
  return request({
    module: 'mes',
    url: `cargo/project/detail/directShipment/type`,
    method: 'get',
    params
  })
}
// 项目直发件发运详情
export function inboundDetailDirect(params) {
  return request({
    url: `/api/mes/building/cargo/project/cargoList/directShipment/details/page`,
    method: 'get',
    params
  })
}

export default { get }
