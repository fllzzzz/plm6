import request from '@/utils/request'

/**
 * 进销存记录
 */
export function get(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/psi`,
    method: 'get',
    params
  })
}

/**
 * 单据明细
 * @param {number} type | required 1：入库明细 2：出库 3：库存明细
 * @param {*} {...row} 当前行的所有数据
 */
export function detail(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/psi/detail`,
    method: 'get',
    params
  })
}

/**
 * 单据明细下载（参数同上）
 */
export function download(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/psi/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}

