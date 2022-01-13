// 表格模板
import request from '@/utils/request'

/**
 * 获取表格模板列表
 * @param {number} page | required 页码
 * @param {number} size | required 页大小
 * @param {number} type  表格类型
 * @param {number} name  表格名称
 */
export function get(params) {
  return request({
    url: 'api/tablePrintTemplate',
    method: 'get',
    params
  })
}

/**
 * 添加表格模板
 * @param {number} name | required 表格名称
 * @param {number} type | required 表格类型
 * @param {number} moduleType | required 表格所属模块类型
 * @param {number} config | required 表格配置
 * @param {number} enabled 是否启用
 * @param {number} default 是否默认
 * @param {number} remark 备注
 */
export function add(data) {
  return request({
    url: 'api/tablePrintTemplate',
    method: 'post',
    data
  })
}

/**
 * 添加表格模板
 * @param {number} id | required 表格id
 * @param {number} name | required 表格名称
 * @param {number} type | required 表格类型
 * @param {number} moduleType | required 表格所属模块类型
 * @param {number} config | required 表格配置
 * @param {number} enabled 是否启用
 * @param {number} default 是否默认
 * @param {number} remark 备注
 */
export function edit(data) {
  return request({
    url: `api/tablePrintTemplate/${data.id}`,
    method: 'put',
    data
  })
}

/**
 * 修改表格状态
 * @param {number} id | required 表格id
 * @param {number} enabled | required 是否启用
 */
export function editEnabled({ id, enabled }) {
  return request({
    url: `api/tablePrintTemplate/${id}/enabled`,
    method: 'put',
    params: { enabled }
  })
}

/**
 * 修改默认模板
 * @param {number} id | required 表格id
 * @param {number} isDefault | required 是否默认模板
 */
export function editDefault({ id, isDefault }) {
  return request({
    url: `api/tablePrintTemplate/${id}/default/${isDefault}`,
    method: 'put'
  })
}

/**
 * 删除
 * @param {number} ids | required 表格ids
 */
export function del(ids) {
  return request({
    url: 'api/tablePrintTemplate',
    method: 'delete',
    data: ids
  })
}

/**
 * 获取表格模板
 * 无权限
 */
export function getByType(type) {
  return request({
    url: `api/tablePrintTemplate/type/${type}`,
    method: 'get'
  })
}

/**
 * 根据条件查询
 * 无权限
 */
export function getByCondition({ types }) {
  return request({
    url: `api/tablePrintTemplate/condition`,
    method: 'get',
    params: { types }
  })
}

export default { get, add, edit, del }
