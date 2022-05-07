import request from '@/utils/request'

/**
 * 获取高新研发费列表
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'report/raw-materials/high-tech-rd-fee',
    method: 'get',
    params
  })
}

/**
 * 高薪研发费excel导出
 */
export function exportExcel(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/high-tech-rd-fee/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}

export default { get }
