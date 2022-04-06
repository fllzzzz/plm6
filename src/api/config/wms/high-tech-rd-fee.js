import request from '@/utils/request'

/**
 * 获取高新研发费配置
 */
export function get() {
  return request({
    module: 'wms',
    url: 'config/material/high-tech-rd-fee',
    method: 'get'
  })
}

/**
 * 批量保存 data[]
 * @param {number} thirdId|required 三级科目id
 * @param {number} rdRate 研发费占比
 */
export function save(data) {
  return request({
    module: 'wms',
    url: 'config/material/high-tech-rd-fee',
    method: 'put',
    data
  })
}

export default { get, save }
