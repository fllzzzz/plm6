import request from '@/utils/request'

/**
 * 获取原材料退货单列表
 */
export function getReceiptList(params) {
  return request({
    module: 'wms',
    url: 'report/raw-materials/reject/receipt',
    method: 'get',
    params
  })
}

/**
 * 退货单详情（含退货记录）
 */
export function getReceiptDetail(id) {
  return request({
    module: 'wms',
    url: `report/raw-materials/reject/receipt/${id}`,
    method: 'get'
  })
}
