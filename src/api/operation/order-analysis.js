import request from '@/utils/request'

/**
 * 订单分析
 * @param {string} dataTime
 * @param {}
 * @returns
 */
export function getOrderAnalysis(params) {
  return request({
    url: `/api/operational/analysis/order`,
    method: 'get',
    params: params
  })
}

