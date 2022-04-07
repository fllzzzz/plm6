import request from '@/utils/request'

/**
 * 获取原材料入库单列表
 */
export function getReceiptList(params) {
  return request({
    module: 'wms',
    url: 'report/raw-materials/inbound/receipt',
    method: 'get',
    params
  })
}

/**
 * 入库单详情（含退货记录）
 */
export function getReceiptDetail(id) {
  return request({
    module: 'wms',
    url: `report/raw-materials/inbound/receipt/${id}`,
    method: 'get'
  })
}

/**
 * 入库明细
 */
export function getDetails(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/inbound/details`,
    method: 'get',
    params
  })
}

/**
 * 入库明细
 */
export function exportDetailsExcel(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/inbound/details/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}
