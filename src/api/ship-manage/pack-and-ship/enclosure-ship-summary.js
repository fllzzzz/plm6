import request from '@/utils/request'

// 围护： 发运统计

export function get(params) {
  return request({
    url: `/api/mes/building/cargo/project/enclosure/summary`,
    method: 'get',
    params
  })
}

// 项目发运详情
export function inboundDetail(params) {
  return request({
    url: `/api/mes/building/cargo/project/cargoList/enclosure/details/page`,
    method: 'get',
    params
  })
}

// 发运汇总
export function shipmentSummary(params) {
  return request({
    url: `/api/mes/building/cargo/year/enclosure/summary`,
    method: 'get',
    params
  })
}

// 查询项目发运数据汇总
export function projectSummary(params) {
  return request({
    url: `/api/mes/building/cargo/project/detail/enclosure/summary`,
    method: 'get',
    params
  })
}

// 发运统计-查询项目各类数据详情
export function summaryDetail(params) {
  return request({
    url: `/api/mes/building/cargo/project/enclosure/detail/type`,
    method: 'get',
    params
  })
}

export default { get }
