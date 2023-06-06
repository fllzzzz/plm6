
import request from '@/utils/request'

/**
 * @description: 保存任务工单【任务下发】
 */
export function saveTask(data) {
  return request({
    module: 'bridge',
    url: 'task/order',
    method: 'post',
    data
  })
}
