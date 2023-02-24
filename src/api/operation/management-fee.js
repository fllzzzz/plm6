import request from '@/utils/request'

/**
 * 管理费
 */
export function getManageFee({ year }) {
  return request({
    url: `/api/contract/Management-fee/findAll/${year}`,
    method: 'get'
  })
}

// 导出清单
export function getManageFeeListFn({ year }) {
  return request({
    url: `/api/contract/Management-fee/export/${year}`,
    method: 'get',
    responseType: 'blob'
  })
}

