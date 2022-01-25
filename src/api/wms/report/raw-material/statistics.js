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
