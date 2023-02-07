import request from '@/utils/request'

/**
 * 检测费
 */
export function getTestingFee(params) {
  return request({
    url: `/api/contract/testing-fee/findAll`,
    method: 'get',
    params
  })
}

// 导出清单
export function downloadTestingFee(params) {
  return request({
    url: `/api/contract/testing-fee/exportExcel`,
    method: 'get',
    params,
    responseType: 'blob'
  })
}

