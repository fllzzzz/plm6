import request from '@/utils/request'

/**
 * 出库明细
 */
export function getDetails(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/outbound/details`,
    method: 'get',
    params
  })
}

/**
 * 出库明细excel导出
 */
export function exportDetailsExcel(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/outbound/details/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}
