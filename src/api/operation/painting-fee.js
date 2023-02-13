import request from '@/utils/request'

/**
 * 涂装费
 */
export function getPaintingFee(params) {
  return request({
    url: `/api/contract/finance/operationAnalysis/painting-price`,
    method: 'get',
    params
  })
}

// 导出清单
export function getPaintingFeeListFn(params) {
  return request({
    url: `/api/contract/finance/operationAnalysis/painting-price/print`,
    method: 'get',
    params
  })
}

