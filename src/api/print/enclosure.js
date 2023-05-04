import request from '@/utils/request'

// 生产管理
/**
 * 排产工单详情
 */
export function schedulingWorkOrderDetail(params) {
  return request({
    url: `/api/mes/enclosure/order/order/detail/print`,
    method: 'get',
    params
  })
}

/**
 * 生产跟踪详情
 */
export function taskTrackingDetail(params) {
  return request({
    url: `/api/mes/enclosure/produce/order/detail/print`,
    method: 'get',
    params
  })
}

/**
 * 围护生产统计
 */
export function productionStatistics(params) {
  return request({
    url: `/api/mes/enclosure/produce/overview/order/detail/print`,
    method: 'get',
    params
  })
}

/**
 * 围护班组产量
 */
export function teamProduction(params) {
  return request({
    url: `/api/mes/enclosure/produce/overview/order/team/produce/print`,
    method: 'get',
    params
  })
}

/**
 * 围护班组产量详情
 */
export function teamProductionDetail(params) {
  return request({
    url: `/api/mes/enclosure/produce/overview/order/team/produce/detail/print`,
    method: 'get',
    params
  })
}

export default {
  // 生产管理
  schedulingWorkOrderDetail, // 排产工单详情
  taskTrackingDetail, // 生产跟踪详情
  productionStatistics, // 围护生产统计
  teamProduction, // 围护班组产量
  teamProductionDetail // 围护班组产量详情
}

