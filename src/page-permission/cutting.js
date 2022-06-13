// 云切割
// --------------------------- 公共权限 start ------------------------------

export const commonPM = {}

// --------------------------- 公共权限 end --------------------------------

// ########################################################################

// --------------------------- 机器配置 start ------------------------------

// 机器配置管理/机器配置
export const machineConfigurationPM = {
  get: ['machine_configuration:get'], // 机器配置列表
  del: ['machine_configuration:del'], // 删除机器
  edit: ['machine_configuration:edit'] // 编辑机器配置

}
// --------------------------- 机器配置 end --------------------------------

// ########################################################################

// --------------------------- 切割报表 start ------------------------------

// 切割工单管理/切割报表
export const cuttingReportPM = {
  get: ['cutting_report:get'], // 切割报表列表
  detail: ['cutting_report:detail'] // 查看详情
}
// --------------------------- 切割报表 end --------------------------------

// ########################################################################

// --------------------------- 套料工单管理套料工单 start -------------------

// 套料工单管理/套料工单
export const nestWorkListPM = {
  get: ['nest_work_list:get'], // 套料工单列表
  detail: ['nest_work_list:detail'], // 钢板清单
  download: ['nest_work_list:download'], // 套料成果
  Production: ['nest_work_list:Production'], // 任务排产
  taskLoading: ['nest_work_list:taskLoading'] // 任务下发
}
// --------------------------- 套料工单管理/套料工单 end --------------------

// ########################################################################

// --------------------------- 套料任务 start ------------------------------

// 套料任务管理-套料任务
export const nestingTaskPM = {
  get: ['nesting_task:get'], // 套料任务列表
  detail: ['nesting_task:detail'], // 查看
  statistics: ['nesting_task:statistics'] // 项目统计
}
// --------------------------- 套料任务 end --------------------------------

// ########################################################################

// --------------------------- 套料任务管理/套料工单 start ------------------

// 套料任务管理-套料工单
export const nestingListPM = {
  get: ['nesting_list:get'], // 套料工单列表
  edit: ['nesting_list:edit'], // 修改
  revoke: ['nesting_list:revoke'], // 撤销
  Nesting: ['nesting_list:Nesting'], // 套料
  views: ['nesting_list:views'], // 查看
  delPart: ['nesting_list:delPart'], // 从工单删除零件
  addPart: ['nesting_list:addPart'] // 从工单添加零件

}
// ---------------------------  套料任务管理/套料工单  end ------------------

// ########################################################################

// --------------------------- 切割工单管理/切割排产 start ------------------

// 切割工单管理/切割排产
export const cuttingSchedulingPM = {
  get: ['cutting_scheduling:get'], // 切割排产列表
  detailMachine: ['cutting_scheduling:detailMachine'], // 按设备查看
  detailProject: ['cutting_scheduling:detailProject'] // 按项目查看
}
// ---------------------------  切割工单管理/切割排产  end ------------------

// ########################################################################

// 切割工单管理/切割工单
export const cuttingWorkingPM = {
  get: ['cutting_working:get'], // 切割工单列表
  detail: ['cutting_working:detail'], // 钢板清单
  download: ['cutting_working:download'], // 下载套料成果
  Production: ['cutting_working:Production'], // 任务排产
  taskLoading: ['cutting_working:taskLoading'], // 任务下发
  cleanLoading: ['cutting_working:cleanLoading'], // 任务清除
  resetLoading: ['cutting_working:resetLoading'], // 任务重置
  downloadDrawing: ['cutting_working:downloadDrawing'] // 下载钢板图形
}
// ---------------------------  切割工单管理/切割工单  end ------------------

// ########################################################################

// ---------------------------切割工单管理/项目任务 start ------------------

// 切割工单管理/项目任务
export const projectTaskingPM = {
  get: ['project_tasking:get'], // 项目任务列表
  detail: ['project_tasking:detail'], // 项目任务查看钢板切割状态
  detailResult: ['project_tasking:detailResult'], // 查看套料成果
  detailDelete: ['project_tasking:detailDelete'], // 查看切割状态中的删除
  pauseCutting: ['project_tasking:pauseCutting'], // 暂停切割
  statistics: ['project_tasking:statistics'] // 项目统计
}
// ---------------------------  切割工单管理/项目任务  end ------------------

// 切割工单管理/设备任务
export const deviceTaskingPM = {
  get: ['device_tasking:get'], // 设备任务列表
  detail: ['device_tasking:detail'], //  设备任务查看钢板切割状态
  detailResult: ['device_tasking:detailResult'] // 查看套料成果

}
// ---------------------------  切割工单管理/设备任务  end ------------------
