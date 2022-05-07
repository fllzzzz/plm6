import request from '@/utils/request'

/**
 * 调拨明细
 */
export function getDetails(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/transfer/details`,
    method: 'get',
    params
  })
}

/**
 * 调拨明细excel导出
 */
export function exportDetailsExcel(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/transfer/details/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}
