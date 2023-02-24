import request from '@/utils/request'

/**
 * 水电费
 */
export function getWaterElectric(params) {
  return request({
    url: `/api/contract/water-electricity/statistics`,
    method: 'get',
    params
  })
}

// 导出清单
export function getWaterElectricListFn(params) {
  return request({
    url: `/api/contract/water-electricity/exportExcel`,
    method: 'get',
    responseType: 'blob',
    params
  })
}

