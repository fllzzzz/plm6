
import request from '@/utils/request'

// 获取库存预警
export function fetchInventoryNotify(params) {
  return request({
    module: 'wms',
    url: 'material/inventory/reminder',
    method: 'get',
    params,
    cancelKey: false
  })
}
