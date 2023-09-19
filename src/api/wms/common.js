
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

// 获取已入库物料的理论厚度列表
export function getInboundThicknessById(classifyId) {
  return request({
    module: 'wms',
    url: `inbound/application/review/theory-thickness/${classifyId}`,
    method: 'get',
    cancelKey: false
  })
}
