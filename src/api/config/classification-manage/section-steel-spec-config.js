import request from '@/utils/request'

/**
 * 获取国标列表
 * @param {*} params
 * @returns
 */
export function getStandard() {
  return request({
    module: 'config',
    url: 'classification/material/section-steel/standard',
    method: 'get'
  })
}

/**
 * 添加国标
 * @param {*} data
 * @returns
 */
export function addStandard(data) {
  return request({
    module: 'config',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: 'classification/material/section-steel/standard',
    method: 'post',
    data
  })
}

/**
 * 删除国标
 * @param {array} ids
 * @returns
 */
export function delStandard(id) {
  return request({
    module: 'config',
    headers: { 'Content-Type': 'application/x-www-form-urlencoded' },
    url: 'classification/material/section-steel/standard',
    method: 'delete',
    data: { id }
  })
}

/**
 * 为单个型材设置国标
 * @param {*} data
 * @returns
 */
export function setStandard(standardId, sectionSteelId) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel/standard/settings',
    method: 'put',
    data: {
      standardId,
      sectionSteelId
    }
  })
}

/**
 * 为多个型材设置国标
 * @param {*} data
 * @returns
 */
export function batchSetStandard(standardId, sectionSteelIds) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel/standard/batch-settings',
    method: 'put',
    data: {
      standardId,
      sectionSteelIds
    }
  })
}

/**
 * 查询型材库列表
 *
 * @export
 * @returns
 */
export function get(params) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel',
    method: 'get',
    params
  })
}

export function detailList(params) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel/detail',
    method: 'get',
    params
  })
}

/**
 *添加、编辑型材
 *
 * @export
 * @param {Number} secondId 物料二级分类id
 * @param {String} secondName 物料二级分类名称
 * @param {String} standard 执行标准
 * @param {Number} attachmentId 清单文件id
 * @returns
 */
export function add(data) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel',
    method: 'put',
    timeout: 6000000,
    data
  })
}

/**
 *编辑供应商
 *
 * @export
 * @param 见上
 * @returns
 */
export function edit(data) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel',
    method: 'put',
    data
  })
}

/**
 *删除型材
 *
 * @export
 * @param {Array} ids 要删除的id集合
 * @returns
 */
export function del(ids) {
  return request({
    module: 'config',
    url: 'classification/material/section-steel',
    method: 'delete',
    data: ids
  })
}

export default { get, add, edit, del }
