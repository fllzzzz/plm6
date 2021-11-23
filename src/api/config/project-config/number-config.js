import request from '@/utils/request'

/**
 * 获取编号列表
 */
export function get() {
  return request({
    module: 'config',
    url: 'project-config/number',
    method: 'get'
  })
}

/**
 * 批量保存 data[]
 * @param {number} type 类型
 * @param {string} code 代号
 * @param {number} number 流水号位数
 */
export function save(data) {
  return request({
    module: 'config',
    url: 'project-config/number',
    method: 'post',
    data
  })
}

export default { get, save }
