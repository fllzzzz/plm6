
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

// 导入表格模板下载
export function downloadExcelTemplate(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/template/excel`,
    method: 'get',
    params,
    responseType: 'blob'
  })
}
