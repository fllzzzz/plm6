import request from '@/utils/request'

/**
 * 库存报表
 */
export function get(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/send-and-receive-storage/inventory`,
    method: 'get',
    params
  })
}

/**
 * 退库明细excel导出
 */
export function excel(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/inventory/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}
