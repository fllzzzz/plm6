import request from '@/utils/request'

/**
 * @description:新增工单
 */
export function save(data) {
  return request({
    module: 'mes',
    url: 'scheduling',
    method: 'post',
    data
  })
}

/**
 * @description:清空工单
 */
export function clear(data) {
  return request({
    module: 'mes',
    url: 'scheduling/clear',
    method: 'put',
    data
  })
}

/**
 * 下载任务导入模板
 */
export function downloadTemplate(params) {
  return request({
    module: 'mes',
    url: 'scheduling/template/export',
    responseType: 'blob',
    method: 'get',
    params
  })
}

/**
 * 任务导入
 */
export function taskImport(data, params) {
  return request({
    module: 'mes',
    url: 'scheduling/import',
    method: 'post',
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    params,
    data
  })
}
