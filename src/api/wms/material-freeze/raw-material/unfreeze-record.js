import request from '@/utils/request'

/**
 * 原材料-解冻记录
 * @returns
 */
export function get(params) {
  return request({
    module: 'wms',
    url: 'freeze/raw-material/unfreeze',
    method: 'get',
    params
  })
}

export default { get }
