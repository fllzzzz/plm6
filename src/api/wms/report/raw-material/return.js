import request from '@/utils/request'

/**
 * 退库明细
 */
export function getDetails(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/return/details`,
    method: 'get',
    params
  })
}

/**
 * 退库明细excel导出
 */
export function exportDetailsExcel(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/return/details/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}
