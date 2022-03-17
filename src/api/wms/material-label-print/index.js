import request from '@/utils/request'

/**
 * 添加打印记录
 *
 * @param {Number} page 页码
 * @param {Number} size 每页数量
 * @returns
 */
export function addPrintRecord(data) {
  return request({
    module: 'wms',
    url: 'material/label-print/print-record',
    method: 'post',
    data
  })
}

/**
 * 获取未打印数量
 */
export function getNotPrintedMaterialNumber() {
  return request({
    module: 'wms',
    url: 'material/label-print/not-printed-number',
    method: 'get'
  })
}
