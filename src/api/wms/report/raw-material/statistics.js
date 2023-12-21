import request from '@/utils/request'

/**
 * 收发存报表
 */
export function getSendAndReceiveStorage(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/send-and-receive-storage`,
    method: 'get',
    params
  })
}

/**
 * 收发存excel导出
 */
export function exportSendAndReceiveStorageExcel(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/send-and-receive-storage/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}

// 手动月末加权
export function monthWeighting() {
  return request({
    module: 'wms',
    url: `report/test/calculateAveragePrice`,
    method: 'get'
  })
}

/**
 * 收发存报表：具体物料详情
 */
export function getSendAndReceiveStorageDetail(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/send-and-receive-storage/detail`,
    method: 'get',
    params
  })
}

/**
 * 收发存excel导出
 */
export function exportSendAndReceiveStorageDetailExcel(params) {
  return request({
    module: 'wms',
    url: `report/raw-materials/send-and-receive-storage/detail/excel`,
    method: 'get',
    responseType: 'blob',
    timeout: 60000000,
    params
  })
}
