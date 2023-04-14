// --------------------------- 公共权限 start ------------------------------

export const commonPM = {
}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 生产管理 start ------------------------------

// 生产管理/排产管理
export const enclosureSchedulingManagePM = {
  get: ['enclosure_scheduling_manage:get'], // 列表
  add: ['enclosure_scheduling_manage:add'] // 下发
}

// 生产管理/排产工单
export const enclosureSchedulingWorkOrderPM = {
  get: ['enclosure_scheduling_work_order:get'], // 列表
  del: ['enclosure_scheduling_work_order:del'], // 删除
  detail: ['enclosure_scheduling_work_order:detail'], // 详情
  print: ['enclosure_scheduling_work_order:print'] // 打印
}

// 生产跟踪/任务跟踪
export const enclosureTaskTrackingPM = {
  get: ['enclosure_task_tracking:get'], // 列表
  detail: ['enclosure_task_tracking:detail'], // 详情
  print: ['enclosure_task_tracking:print'] // 打印
}

// 生产跟踪/项目全貌
export const enclosureProjectOverviewPM = {
  get: ['enclosure_project_overview:get'] // 列表
}

// --------------------------- 生产管理 end --------------------------------
