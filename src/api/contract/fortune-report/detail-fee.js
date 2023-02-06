import request from '@/utils/request'

// 运输费列表
export function getShippingList(params) {
  return request({
    url: `/api/mes/building/cargo/shipment/list/page`,
    method: 'get',
    params
  })
}

// 运输费汇总
export function getShippingSummary(id) {
  return request({
    url: `/api/mes/building/cargo/shipment/summary/${id}`,
    method: 'get'
  })
}

// 折旧费
export function getDepreciationList(params) {
  return request({
    url: `/api/contract/project-finance/list-depreciation`,
    method: 'get',
    params
  })
}

// 主材辅材费
export function getMainAuxiliaryList(params) {
  return request({
    url: `/api/contract/project-finance/list-outbound`,
    method: 'get',
    params
  })
}

// 人工费
export function getLaborFeeList(params) {
  return request({
    url: `/api/contract/project-finance/list-wage`,
    method: 'get',
    params
  })
}

// 分包费
export function getSubcontractList(params) {
  return request({
    url: `/api/contract/project-finance/list-sub`,
    method: 'get',
    params
  })
}

// 检测费
export function getTestingList(params) {
  return request({
    url: `/api/contract/project-finance/list-testing`,
    method: 'get',
    params
  })
}

// 水电费
export function getWaterElectricList(params) {
  return request({
    url: `/api/contract/project-finance/list-water`,
    method: 'get',
    params
  })
}

// 管理费
export function getManagementList(params) {
  return request({
    url: `/api/contract/project-finance/list-manage`,
    method: 'get',
    params
  })
}
