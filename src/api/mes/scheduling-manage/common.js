
import request from '@/utils/request'

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
