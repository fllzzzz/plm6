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
 * 出库钢材核算量
 */
export function getSummary(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/outbound/details/mete`,
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

/**
 * 退库明细体现出库excel导出
 */
export function exportReturnDetailsExcel(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/outboundAndReturn/details/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}
