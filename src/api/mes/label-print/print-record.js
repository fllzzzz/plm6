
import request from '@/utils/request'

/**
 * 获取任务打印记录
 * @export
 * @param {number} taskId|required 任务id
 * @returns
 */
export function getForTask(taskId) {
  return request({
    url: 'api/mes/print/task/record',
    method: 'get',
    params: { taskId }
  })
}

/**
 * 获取材料打印记录
 * @export
 * @param {number} materialId|required 材料id
 * @param {number} materialListType|required 材料类型
 * @returns
 */
export function getForMaterial({ materialId, materialListType }) {
  return request({
    url: 'api/mes/print/material/record',
    method: 'get',
    params: { materialId, materialListType }
  })
}

/**
 * 获取包单打印记录
 * @export
 * @param {number} id|required 包id
 * @returns
 */
export function getForPackage(id) {
  return request({
    url: 'api/mes/print/package/record',
    method: 'get',
    params: { id }
  })
}

/**
 * 添加任务打印记录
 * @export
 * @param {number} taskId|required 任务id
 * @param {number} quantity|required 打印数量
 * @param {number} startTime|required 打印开始时间【时间戳】
 * @param {number} endTime|required 打印结束时间【时间戳】
 * @returns
 */
export function taskAdd({ taskId, quantity, startTime, endTime }) {
  return request({
    url: 'api/mes/print/task/record',
    method: 'post',
    data: { taskId, quantity, startTime, endTime }
  })
}

/**
 * 添加材料打印记录
 * @export
 * @param {number} taskId|required 材料id
 * @param {number} materialListType|required 材料类型
 * @param {number} quantity|required 打印数量
 * @param {number} startTime|required 打印开始时间【时间戳】
 * @param {number} endTime|required 打印结束时间【时间戳】
 * @returns
 */
export function materialAdd({ materialId, materialListType, quantity, startTime, endTime }) {
  return request({
    url: 'api/mes/print/material/record',
    method: 'post',
    data: { materialId, materialListType, quantity, startTime, endTime }
  })
}

/**
 * 添加包单打印记录
 * @export
 * @param {number} id|required 包id
 * @param {number} quantity|required 打印数量
 * @param {number} startTime|required 打印开始时间【时间戳】
 * @param {number} endTime|required 打印结束时间【时间戳】
 * @returns
 */
export function packageRecordAdd({ id, quantity, startTime, endTime }) {
  return request({
    url: 'api/mes/print/package/record',
    method: 'post',
    data: { id, quantity, startTime, endTime }
  })
}
