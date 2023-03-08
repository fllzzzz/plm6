
import request from '@/utils/request'

/**
 * 下载任务导入模板
 */
export function downloadTemplate(params) {
  return request({
    module: 'mes',
    url: 'scheduling/artifact/template/export',
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
    url: 'scheduling/artifact/import',
    method: 'post',
    headers: {
      'Content-Type': 'multipart/form-data'
    },
    params,
    data
  })
}

/**
 * @description: 保存任务工单【任务下发】
 */
export function saveTask(data) {
  return request({
    module: 'mes',
    url: 'task/order',
    method: 'post',
    data
  })
}
/**
 * @description: 保存套料完成进行下发任务
 */
export function saveNestingTask(data) {
  return request({
    module: 'mes',
    url: 'machine_part/scheduling/nest_cut/task',
    method: 'post',
    data
  })
}
