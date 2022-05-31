import request from '@/utils/request'

/**
 * 订单交付率
 * @param {string} dataTime
 * @param {}
 * @returns
 */
export function getOrderDeliveryRate(params) {
  return request({
    url: `/api/operational/analysis/deliver`,
    method: 'get',
    params: params
  })
}

