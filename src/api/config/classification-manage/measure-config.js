import request from '@/utils/request'

/**
 * 获取计量配置
 */
export function get() {
  return request({
    module: 'config',
    url: 'classification/material/measure',
    method: 'get'
  })
}

/**
 * 批量保存 data[]
 * @param {number} thirdId|required 三级科目id
 * @param {string} measure|required 计量单位 字典值
 * @param {string} nuclear|required 计量单位 字典值
 * @param {number} decimalPoint|required 小数点保留
 */
export function save(data) {
  return request({
    module: 'config',
    url: 'classification/material/measure',
    method: 'put',
    data
  })
}

export default { get, save }

