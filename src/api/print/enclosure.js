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

export default {
  // 生产管理
  schedulingWorkOrderDetail // 排产工单详情
}

