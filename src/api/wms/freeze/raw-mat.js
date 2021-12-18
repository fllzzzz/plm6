import request from '@/utils/request'

/**
 * 冻结原材料列表
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'freeze/raw-material',
    method: 'get',
    params
  })
}

/**
 * 解冻
 */
export function unfreezeHandling(data) {
  return request({
    module: 'wms',
    url: 'freeze/raw-material/unfreeze',
    method: 'put',
    data
  })
}

/**
 * 获取物料冻结详情
 * @param {*} materialId 物料id
 * @returns
 */
export function getMaterialFreezeRecordById(materialId) {
  return request({
    module: 'wms',
    url: `freeze/raw-material/record`,
    method: 'get',
    params: { materialId }
  })
}

export default { get }
