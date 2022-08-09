import request from '@/utils/request'

/**
 * 获取入库质检物料
 */
export function get() {
  return request({
    module: 'config',
    url: 'classification/material/measure/quality-testing',
    method: 'get'
  })
}

/**
 * 添加入库质检物料
 */
export function add({ ids }) {
  return request({
    module: 'config',
    url: 'classification/material/measure/quality-testing',
    method: 'put',
    data: ids
  })
}

/**
 * 删除入库质检物料
 */
export function del(ids) {
  return request({
    module: 'config',
    url: 'classification/material/measure/quality-testing',
    method: 'delete',
    data: ids
  })
}

export default { get, add, del }

