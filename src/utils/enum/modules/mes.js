import { constantize } from '../base'

// 班组类型
const teamTypeEnum = {
  TEAM: { L: '班组', V: 1 << 0 },
  INSPECTION: { L: '质检', V: 1 << 1 }
}
constantize(teamTypeEnum)

// 班组属性
const teamAttributeEnum = {
  IN_STAFF: { L: '编制内', K: 'IN_STAFF', V: true, T: 'success' },
  OFF_STAFF: { L: '编制外', K: 'OFF_STAFF', V: false, T: 'warning' }
}
constantize(teamAttributeEnum)

// 工序类
const processCategoryEnum = {
  ASSEMBLY_RIVETING_WELDING: { L: '组铆焊类', K: 'ASSEMBLY_RIVETING_WELDING', V: 1 << 0 },
  PAINT: { L: '油漆类', K: 'PAINT', V: 1 << 1 },
  MAKINGS: { L: '下料类', K: 'MAKINGS', V: 1 << 2 },
  DRILL_HOLE: { L: '钻孔类', K: 'DRILL_HOLE', V: 1 << 3 },
  PRESSING: { L: '压板类', K: 'PRESSING', V: 1 << 4 }
}
constantize(processCategoryEnum)

// 工序生产检验方式
const processInspectTypeEnum = {
  SINGLE_UNSCAN: { L: '单件(不扫码)', K: 'SINGLE_UNSCAN', V: 0, T: 'info' },
  SINGLE_SCAN: { L: '单件(需扫码)', K: 'SINGLE_SCAN', V: 1, T: 'warning' },
  BATCH_UNSCAN: { L: '批量(不扫码)', K: 'BATCH_UNSCAN', V: 2, T: 'danger' },
  BATCH_SCAN: { L: '批量(需扫码)', K: 'BATCH_SCAN', V: 3, T: 'success' }
}
constantize(processInspectTypeEnum)

// 工序生产上报方式
const processReportTypeEnum = {
  MACHINE: { L: '机联', K: 'MACHINE', V: 4, T: '' },
  SINGLE_UNSCAN: { L: '单件(不扫码)', K: 'SINGLE_UNSCAN', V: 0, T: 'info' },
  SINGLE_SCAN: { L: '单件(需扫码)', K: 'SINGLE_SCAN', V: 1, T: 'warning' },
  BATCH_UNSCAN: { L: '批量(不扫码)', K: 'BATCH_UNSCAN', V: 2, T: 'danger' },
  BATCH_SCAN: { L: '批量(需扫码)', K: 'BATCH_SCAN', V: 3, T: 'success' }
}
constantize(processReportTypeEnum)

// mes 产品类型
const componentTypeEnum = {
  MACHINE_PART: { L: '零件', SL: '零件', K: 'MACHINE_PART', V: 1 << 0, T: '', COLOR: '#fad400' },
  ASSEMBLE: { L: '部件', SL: '部件', K: 'ASSEMBLE', V: 1 << 4, T: 'success', COLOR: '#40ed8d' },
  ARTIFACT: { L: '构件', SL: '构件', K: 'ARTIFACT', V: 1 << 1, T: 'success', COLOR: '#00babd' },
  ENCLOSURE: { L: '围护', SL: '围护', K: 'ENCLOSURE', V: 1 << 2, T: 'warning', COLOR: '#ff7800' },
  AUXILIARY_MATERIAL: { L: '辅材', SL: '辅材', K: 'AUXILIARY_MATERIAL', T: 'info', V: 1 << 3, COLOR: '#f5f7fa' }
}
constantize(componentTypeEnum)

const taskTypeENUM = {
  MACHINE_PART: componentTypeEnum.MACHINE_PART,
  ASSEMBLE: componentTypeEnum.ASSEMBLE,
  ARTIFACT: componentTypeEnum.ARTIFACT,
  ENCLOSURE: componentTypeEnum.ENCLOSURE,
  AUXILIARY_MATERIAL: componentTypeEnum.AUXILIARY_MATERIAL,
  PARENT_PART: { L: '母件', SL: '母件', K: 'PARENT_PART', V: 1 << 5 }
}
constantize(taskTypeENUM)

// 构件工序
const artifactProcessEnum = {
  ONCE: { L: '一次工序', K: 'ONCE', V: componentTypeEnum.ASSEMBLE.V },
  TWICE: { L: '二次工序', K: 'TWICE', V: componentTypeEnum.ARTIFACT.V }
}
constantize(artifactProcessEnum)

// 出入库类型
const mesWarehouseStateTypeEnum = {
  INBOUND: { L: '入库', K: 'INBOUND', V: 1 },
  OUTBOUND: { L: '出库', K: 'OUTBOUND', V: 2 }
}
constantize(mesWarehouseStateTypeEnum)

// 含有工序的材料类型
const processMaterialListTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: componentTypeEnum.ARTIFACT.V, T: '' },
  ASSEMBLE: { L: '部件', K: 'ASSEMBLE', V: componentTypeEnum.ASSEMBLE.V, T: 'info' },
  MACHINE_PART: { L: '零件', K: 'MACHINE_PART', V: componentTypeEnum.MACHINE_PART.V, T: 'success' },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: componentTypeEnum.ENCLOSURE.V, T: 'warning' }
}
constantize(processMaterialListTypeEnum)

// 工价定额单价类型
const wageQuotaTypeEnum = {
  WEIGHT: { L: '按重量计价', SL: '重量', K: 'WEIGHT', V: 1 << 0, F: 'weightPrice', unit: '元/吨', meteUnit: '吨', C_UNIT: 't', DP: 'COM_WT__T' },
  LENGTH: { L: '按长度计价', SL: '长度', K: 'LENGTH', V: 1 << 1, F: 'lengthPrice', unit: '元/米', meteUnit: '米', C_UNIT: 'm', DP: 'COM_L__M' },
  QUANTITY: { L: '按数量计价', SL: '数量', K: 'QUANTITY', V: 1 << 3, F: 'quantityPrice', unit: '元/件', meteUnit: '件', C_UNIT: '件', DP: '' },
  AREA: { L: '按面积计价', SL: '面积', K: 'AREA', V: 1 << 2, F: 'areaPice', unit: '元/平方米', meteUnit: '平方米', C_UNIT: '㎡', DP: 'COM_AREA__M2' }
}
constantize(wageQuotaTypeEnum)

// 楼承板子类型
const floorPlateTypeEnum = {
  TRUSS_FLOOR_PLATE: { L: '桁架楼承板', K: 'TRUSS_FLOOR_PLATE', V: 1 << 3 },
  PRESSED_FLOOR_PLATE: { L: '压型楼承板', K: 'PRESSED_FLOOR_PLATE', V: 1 << 4 }
  // OPEN_CLOSED_FLOOR_PLATE: { L: '开闭口楼承板', K: 'OPEN_CLOSED_FLOOR_PLATE', V: 1 << 4 }
}
constantize(floorPlateTypeEnum)

// 围护产品类型
const mesEnclosureTypeEnum = {
  PRESSED_PLATE: { L: '压型板', K: 'PRESSED_PLATE', V: 1 << 1 },
  SANDWICH_BOARD: { L: '夹芯板', K: 'SANDWICH_BOARD', V: 1 << 2 },
  TRUSS_FLOOR_PLATE: floorPlateTypeEnum.TRUSS_FLOOR_PLATE,
  PRESSED_FLOOR_PLATE: floorPlateTypeEnum.PRESSED_FLOOR_PLATE,
  FOLDING_PIECE: { L: '折边件', K: 'FOLDING_PIECE', V: 1 << 5 }
  // FLOOR_PLATE: { L: '楼承板', K: 'FLOOR_PLATE', V: floorPlateTypeEnum.TRUSS_FLOOR_PLATE.V | floorPlateTypeEnum.PRESSED_FLOOR_PLATE.V }
}
constantize(mesEnclosureTypeEnum)

const projectComponentTypeEnum = {
  ARTIFACT: { L: '结构', K: 'ARTIFACT', V: 1 << 0 },
  ...mesEnclosureTypeEnum.ENUM
}
constantize(projectComponentTypeEnum)

// 报表展示 结构、围护
const reportComponentTypeEnum = {
  ARTIFACT: { L: '结构', K: 'ARTIFACT', V: componentTypeEnum.ARTIFACT.V },
  ENCLOSURE: componentTypeEnum.ENCLOSURE
}
constantize(reportComponentTypeEnum)

// 任务下发状态类型
const taskIssueTypeEnum = {
  NOT_ISSUED: { L: '未下发', K: 'NOT_ISSUED', V: false, T: 'danger' },
  HAS_ISSUED: { L: '已下发', K: 'HAS_ISSUED', V: true, T: 'success' }
}
constantize(taskIssueTypeEnum)

// 零件排产下发状态
const machinePartSchedulingIssueStatusEnum = {
  NOT_NESTING: { L: '未套料', K: 'NOT_NESTING', V: 1 << 0, T: 'info' },
  IN_NESTING: { L: '套料中', K: 'IN_NESTING', V: 1 << 1, T: 'warning' },
  OUT_NESTING: { L: '套料完成', K: 'OUT_NESTING', V: 1 << 2, T: '' },
  HAS_ISSUED: { L: '已下发', K: 'HAS_ISSUED', V: 1 << 3, T: 'success' }
}
constantize(machinePartSchedulingIssueStatusEnum)

// 排产状态
const mesSchedulingStatusEnum = {
  NOT: { L: '未排产', K: 'NOT', V: 1 << 0, T: 'info' },
  PARTIAL: { L: '部分排产', K: 'PARTIAL', V: 1 << 1, T: 'warning' },
  HAS: { L: '已排产', K: 'HAS', V: 1 << 2, T: '' }
}
constantize(mesSchedulingStatusEnum)

// 零件排产套料状态
const machinePartNestingStatusEnum = {
  NOT_NESTING: { L: '未下发', K: 'NOT_NESTING', V: machinePartSchedulingIssueStatusEnum.NOT_NESTING.V },
  HAS_NESTING: { L: '已下发', K: 'HAS_NESTING', V: machinePartSchedulingIssueStatusEnum.IN_NESTING.V | machinePartSchedulingIssueStatusEnum.OUT_NESTING.V | machinePartSchedulingIssueStatusEnum.HAS_ISSUED.V }
}
constantize(machinePartNestingStatusEnum)

// 可打包类型
const packTypeEnum = {
  STRUCTURE: { L: '结构', SL: '结构', K: 'STRUCTURE', V: componentTypeEnum.ARTIFACT.V, T: '' },
  ENCLOSURE: { L: '围护', SL: '围护', K: 'ENCLOSURE', V: componentTypeEnum.ENCLOSURE.V, T: 'warning' },
  AUXILIARY_MATERIAL: { L: '辅材', SL: '配套件', K: 'AUXILIARY_MATERIAL', V: componentTypeEnum.AUXILIARY_MATERIAL.V, T: 'success' }
}
constantize(packTypeEnum)

// 打包清单状态
const packStatusTypeEnum = {
  // UNENTRUCK: { L: '未装车', K: 'UNENTRUCK', V: 1 },
  // ENTRUCK: { L: '已装车', K: 'ENTRUCK', V: 2 },
  // CHECKED: { L: '已出库', K: 'CHECKED', V: 3 }
  UNENTRUCK: { L: '未装车', K: 'UNENTRUCK', V: 0, T: '' },
  ENTRUCK: { L: '已装车', K: 'ENTRUCK', V: 1, T: 'warning' },
  SHIPMENT: { L: '已发运', K: 'SHIPMENT', V: 2, T: 'success' },
  CANCEL: { L: '取消发运', K: 'CANCEL', V: 4, T: 'danger' }
}
constantize(packStatusTypeEnum)

// 收货状态
const receiptStatusEnum = {
  RECEIVED: { L: '已收货', K: 'RECEIVED', V: 1, COLOR: '#40ed8d', T: 'success' },
  UNRECEIVED: { L: '未收货', K: 'UNRECEIVED', V: 2, COLOR: '#f5f7fa', T: 'warning' }
}
constantize(receiptStatusEnum)

// 物流计价方式
const logisticsPriceTypeEnum = {
  WEIGHT: { L: '重量', K: 'WEIGHT', V: 1, T: '', unit: '元/吨' },
  TRAINS: { L: '车次', K: 'TRAINS', V: 2, T: 'warning', unit: '元/车' }
}
constantize(logisticsPriceTypeEnum)

// 发运审核状态
const shipAuditStatusEnum = {
  UNCHECKED: { L: '未审核', K: 'UNCHECKED', V: 2, T: 'warning' },
  CHECKED: { L: '已审核', K: 'CHECKED', V: 1, T: 'success' }
}
constantize(shipAuditStatusEnum)

// 变更异常方式状态
const abnormalHandleTypeEnum = {
  PRODUCTION_CHANGE: { K: 'PRODUCTION_CHANGE', L: '生产变更', V: 1 << 1 },
  SCHEDULE_CHANGE: { K: 'SCHEDULE_CHANGE', L: '排产变更', V: 1 << 0 },
  MACHINE_PART: { K: 'MACHINE_PART', L: '零件变更', V: 1 << 2 }
}
constantize(abnormalHandleTypeEnum)

// 变更异常处理状态
const abnormalHandleStatusEnum = {
  PENDING: { L: '待处理', K: 'PENDING', V: 1 << 0, TAG: '' },
  PROCESSING: { L: '处理中', K: 'PROCESSING', V: 1 << 1, TAG: 'warning' },
  PROCESSING_COMPLETE: { L: '处理完成', K: 'PROCESSING_COMPLETE', V: 1 << 2, TAG: 'success' },
  CANCEL: { L: '已取消', K: 'CANCEL', V: 1 << 3, TAG: 'info' }
}
constantize(abnormalHandleStatusEnum)

// 多余清单处理状态
const surplusHandleStatusEnum = {
  PENDING: { L: '未处理', K: 'PENDING', V: 1 << 0, TAG: '' },
  SCRAPPED: { L: '报废', K: 'SCRAPPED', V: 1 << 1, TAG: 'danger' },
  SECONDARY_USE: { L: '二次利用', K: 'SECONDARY_USE', V: 1 << 2, TAG: 'warning' }
}
constantize(surplusHandleStatusEnum)

// 变更上报类型状态
const abnormalReportTypeEnum = {
  NORMAL: { L: '正常上报', K: 'NORMAL', V: 0 },
  ABNORMAL: { L: '异常上报', K: 'ABNORMAL', V: 1 }
}
constantize(abnormalReportTypeEnum)

// 异常状态
const abnormalStatusEnum = {
  NORMAL: { L: '正常', K: 'NORMAL', V: false },
  ABNORMAL: { L: '暂停', K: 'ABNORMAL', V: true }
}
constantize(abnormalStatusEnum)

// 排产状态
const schedulingStatusEnum = {
  UN_START: { L: '未分配', K: 'UN_START', V: 1 },
  UNFINISHED: { L: '未分配完', K: 'UNFINISHED', V: 2 },
  FINISHED: { L: '已分配完', K: 'FINISHED', V: 4 }
}
constantize(schedulingStatusEnum)

// 变更变更类型状态
const abnormalChangeTypeEnum = {
  ARTIFACT_REDUCE: { L: '构件减少', K: 'ARTIFACT_REDUCE', V: 1 << 0 },
  ARTIFACT_DELETE: { L: '构件删除', K: 'ARTIFACT_DELETE', V: 1 << 1 },
  ASSEMBLE_REDUCE: { L: '部件减少', K: 'ASSEMBLE_REDUCE', V: 1 << 2 },
  ASSEMBLE_DELETE: { L: '部件删除', K: 'ASSEMBLE_DELETE', V: 1 << 3 },
  MACHINE_PART_REDUCE: { L: '零件减少', K: 'MACHINE_PART_REDUCE', V: 1 << 4 },
  MACHINE_PART_DELETE: { L: '零件删除', K: 'MACHINE_PART_DELETE', V: 1 << 5 }
}
constantize(abnormalChangeTypeEnum)

// 问题整改状态
const improveStatusEnum = {
  WAIT_RECTIFIED: { L: '未整改', K: 'WAIT_RECTIFIED', V: 1 << 0, T: '' },
  RECTIFIED: { L: '已反馈', K: 'RECTIFIED', V: 1 << 1, T: 'warning' },
  ADOPT: { L: '整改通过', K: 'ADOPT', V: 1 << 2, T: 'success' },
  UN_ADOPT: { L: '整改未通过', K: 'UN_ADOPT', V: 1 << 3, T: 'danger' }
}
constantize(improveStatusEnum)

// 油漆类型
const paintingTypeEnum = {
  PRIMER: { L: '底漆', K: 'PRIMER', V: 1 << 0 },
  INTERMEDIATE_PAINT: { L: '中间漆', K: 'INTERMEDIATE_PAINT', V: 1 << 1 },
  TOPCOAT: { L: '面漆', K: 'TOPCOAT', V: 1 << 2 }
}
constantize(paintingTypeEnum)

// 标签类型
const labelTypeEnum = {
  COMMON: { L: '常规型', K: 'COMMON', V: 1 << 0, size: {
    [componentTypeEnum.ARTIFACT.V]: ' 100 * 75 ',
    [componentTypeEnum.ENCLOSURE.V]: ' 100 * 30 '
  }},
  SIMPLE: { L: '简约型', K: 'SIMPLE', V: 1 << 1, size: {
    [componentTypeEnum.ARTIFACT.V]: ' 100 * 75 ',
    [componentTypeEnum.ENCLOSURE.V]: ' 无 '
  }},
  CUSTOM: { L: '定制型', K: 'CUSTOM', V: 1 << 2, size: {
    [componentTypeEnum.ARTIFACT.V]: ' 100 * 75 ',
    [componentTypeEnum.ENCLOSURE.V]: ' 100 * 50 '
  }}
}
constantize(labelTypeEnum)

// 打印的产品类型
const printProductTypeEnum = {
  ARTIFACT: { L: '构件', K: 'ARTIFACT', V: 1 << 0 },
  ENCLOSURE: { L: '围护', K: 'ENCLOSURE', V: 1 << 1 },
  PACKAGE: { L: '包', K: 'PACKAGE', V: 1 << 2 }
}
constantize(printProductTypeEnum)

// 在制品详情报表类型
const inProductionDetailReportEnum = {
  COMPLETE: { L: '完成品', K: 'COMPLETE', V: 1 << 0 },
  IN_PRODUCTION: { L: '在制品', K: 'IN_PRODUCTION', V: 1 << 1 },
  UN_PRODUCTION: { L: '未生产', K: 'UN_PRODUCTION', V: 1 << 2 }
}
constantize(inProductionDetailReportEnum)

// 构件配置传统/智能生产线
const artifactProductLineEnum = {
  TRADITION: { L: '传统线', K: 'TRADITION', V: 1 << 0 },
  INTELLECT: { L: '智能线', K: 'INTELLECT', V: 1 << 1 }
}
constantize(artifactProductLineEnum)

// 构件配置智能线父类型
const intellectParentType = {
  PILLAR: { L: '柱', K: 'PILLAR', V: 1 << 1 },
  BRIDGE: { L: '梁', K: 'BRIDGE', V: 1 << 0 }
}
constantize(intellectParentType)

// 最小数值类型
const minEqualTypeEnum = {
  YES: { L: '≥', K: 'YES', V: true },
  NO: { L: '>', K: 'NO', V: false }
}
constantize(minEqualTypeEnum)

// 最大数值类型
const maxEqualTypeEnum = {
  YES: { L: '≤', K: 'YES', V: true },
  NO: { L: '<', K: 'NO', V: false }
}
constantize(maxEqualTypeEnum)

// 构件类型
const artifactTypeEnum = {
  COMMON: { L: '主构件', K: 'COMMON', V: 1 },
  SMALL: { L: '次构件', K: 'SMALL', V: 2 }
}
constantize(artifactTypeEnum)

// 打码方式
const codingTypeEnum = {
  SINGLE: { L: '一物一码', K: 'SINGLE', V: 1 },
  BATCH: { L: '一码多件', K: 'BATCH', V: 2 }
}
constantize(codingTypeEnum)

// 切割配置/切割类型
const cuttingConfigEnum = {
  FLAME_CUT: { L: '火焰切割', K: 'FLAME_CUT', V: 1 << 0 },
  PLASMA_CUT: { L: '等离子切割', K: 'PLASMA_CUT', V: 1 << 1 },
  LASER_CUT: { L: '激光切割', K: 'LASER_CUT', V: 1 << 2 },
  PLATE_CUT: { L: '剪版', K: 'PLATE_CUT', V: 1 << 3 }
}
constantize(cuttingConfigEnum)

// 送货状态
const deliveryStatusEnum = {
  NORMAL: { L: '正常送货', K: 'NORMAL', V: 1 << 0 },
  RETURN: { L: '取消送货', K: 'RETURN', V: 1 << 1 }
}
constantize(deliveryStatusEnum)

// 送货状态
const deliveryReceiptStatusEnum = {
  DELIVERY: { L: '送货中', K: 'DELIVERY', V: 1 << 0, T: '' },
  SIGN: { L: '已签收', K: 'SIGN', V: 1 << 2, T: 'success' },
  RETURN: { L: '取消送货', K: 'RETURN', V: 1 << 1, T: 'warning' }
}
constantize(deliveryReceiptStatusEnum)

// 日期查询类型
const searchDateTypeEnum = {
  DELIVERY_DATE: { L: '发运日期', K: 'DELIVERY_DATE', V: 1 << 0 },
  UPDATE_DATE: { L: '状态更新日期', K: 'UPDATE_DATE', V: 1 << 1 }
}
constantize(searchDateTypeEnum)

// 收货运费变更类型
const freightChangeTypeEnum = {
  CONTINUE: { L: '运费保留', K: 'CONTINUE', SL: '运费正常', V: 1 << 0 },
  CANCEL: { L: '运费作废', K: 'CANCEL', SL: '运费作废', V: 1 << 1 },
  CHANGE: { L: '运费变更', K: 'CHANGE', SL: '运费变更', V: 1 << 2 }
}
constantize(freightChangeTypeEnum)

// mes配置-零件前缀类型
const partKeyWordEnum = {
  P: { L: 'P', K: 'P', V: 'P' },
  PL: { L: 'PL', K: 'PL', V: 'PL' }
}
constantize(partKeyWordEnum)

// 生产订单 零件是否有孔
const hasHoleEnum = {
  TRUE: { L: '有孔', K: 'TRUE', V: true },
  FALSE: { L: '无孔', K: 'FALSE', V: false }
}
constantize(hasHoleEnum)

// 构件规格是否修正
const artifactSpecReviseEnum = {
  NOT: { L: '未修正', K: 'NOT', V: false },
  REVISED: { L: '已修正', K: 'REVISED', V: true }
}
constantize(artifactSpecReviseEnum)

// 任务跟踪/工单跟踪
const taskTrackingSchedulingStatusEnum = {
  NOT_FINISHED: { L: '未完成', K: 'NOT_FINISHED', V: 1 },
  FINISHED: { L: '已完成', K: 'FINISHED', V: 2 }
}
constantize(taskTrackingSchedulingStatusEnum)

// 型材套料状态
const projectNestingStatusEnum = {
  NOT_NESTING: { L: '未套料', K: 'NOT_NESTING', V: 1 << 0, T: 'danger', COLOR: '#E82121' },
  PARTIAL_NESTING: { L: '部分套料', K: 'PARTIAL_NESTING', V: 1 << 1, T: 'warning', COLOR: '#E6A23C' },
  END_NESTING: { L: '已套料', K: 'END_NESTING', V: 1 << 2, T: 'success', COLOR: '#35D552' }
}
constantize(projectNestingStatusEnum)

// 型材套料成果 下发状态
const MesBuildingTypesettingStatusEnum = {
  UNCONFIRMED: { L: '待确认', K: 'UNCONFIRMED', V: 1 << 0, T: '' },
  COMPLETE: { L: '套料完成', K: 'COMPLETE', V: 1 << 1, T: 'success' },
  EXPIRED: { L: '已过期', K: 'EXPIRED', V: 1 << 2, T: 'info' },
  ISSUED: { L: '已下发', K: 'ISSUED', V: 1 << 3, T: 'warning' },
  PRODUCTION: { L: '生产中', K: 'PRODUCTION', V: 1 << 4, T: 'danger' }
}
constantize(MesBuildingTypesettingStatusEnum)

// 型材套料成果/套料文件
const nestingFileTypeEnum = {
  NESTING_FILE: { L: '套料文件', K: 'NESTING_FILE', V: 0 },
  MATERIAL_LIST: { L: '材料清单', K: 'MATERIAL_LIST', V: 1 }
}
constantize(nestingFileTypeEnum)

// 套料设置
const nestingSettingTypeEnum = {
  UN_LOSSY: { L: '无损套料', K: 'UN_LOSSY', V: 1 },
  LOSSY: { L: '有损套料', K: 'LOSSY', V: 2 }
}
constantize(nestingSettingTypeEnum)

// 套料成果
const mesBuildingTypeSettingAssembleTypeEnum = {
  WELDING: { L: '焊接型材', K: 'WELDING', V: 1 },
  FINISHED: { L: '成品型材', K: 'FINISHED', V: 2 }
}
constantize(mesBuildingTypeSettingAssembleTypeEnum)

// 工单管理/零件工单
const mesMachinePartOrderTypeEnum = {
  CUTTING_ORDER: { L: '切割工单', K: 'CUTTING_ORDER', V: 1 << 2 },
  DRILL_ORDER: { L: '钻孔工单', K: 'DRILL_ORDER', V: 1 << 3 }
}
constantize(mesMachinePartOrderTypeEnum)

// 收货状态
const shipStatusEnum = {
  SHIPPING: { L: '发货中', K: 'SHIPPING', V: 1 << 0 },
  SHIPPED: { L: '发货完毕', K: 'SHIPPED', V: 1 << 1 },
  SETTLED: { L: '已结算', K: 'SETTLED', V: 1 << 2 }
}
constantize(shipStatusEnum)

// 工单跟踪
const workOrderTypeEnum = {
  DELAY: { L: '滞后', K: 'DELAY', V: 0 },
  NORMAL: { L: '正常', K: 'NORMAL', V: 1 }
}
constantize(workOrderTypeEnum)

// 型材套料：套料设定
const assembleTypeEnum = {
  SUB_ASSEMBLE: { L: '次部件', K: 'SUB_ASSEMBLE', V: 0 },
  MAIN_ASSEMBLE: { L: '主部件', K: 'MAIN_ASSEMBLE', V: 1 }
}
constantize(assembleTypeEnum)

// 型材套料: nc1文件缺失
const fileNC1TypeEnum = {
  NO_IMPORT: { L: '未导入', K: 'NO_IMPORT', V: 0 },
  HAS_IMPORT: { L: '已导入', K: 'HAS_IMPORT', V: 1 }
}
constantize(fileNC1TypeEnum)

// 切割工单
const sortingListEnum = {
  NESTING_TASK_ORDER: { L: '套料任务单', K: 'NESTING_TASK_ORDER', V: 1 },
  PRODUCTION_TASK_ORDER: { L: '生产任务单', K: 'PRODUCTION_TASK_ORDER', V: 3 },
  SORTING_ORDER: { L: '分拣单', K: 'SORTING_ORDER', V: 2 }
}
constantize(sortingListEnum)

// 制成品出入库详情查询类型
const productSearchTypeEnum = {
  LIST: { L: '清单', K: 'LIST', V: 1 },
  INBOUND: { L: '入库', K: 'INBOUND', V: 2 },
  OUTBOUND: { L: '出库', K: 'OUTBOUND', V: 3 },
  STOCK: { L: '库存', K: 'STOCK', V: 4 }
}
constantize(productSearchTypeEnum)
// 钻孔工单
const drillListEnum = {
  PRODUCTION_TASK_ORDER: { L: '钻孔任务单', K: 'PRODUCTION_TASK_ORDER', V: 1 },
  SORTING_ORDER: { L: '分拣单', K: 'SORTING_ORDER', V: 2 }
}
constantize(drillListEnum)

// 结构工单
const structureOrderTypeEnum = {
  ASSEMBLE: { L: '部件', K: 'ASSEMBLE', V: 1 << 4, T: 'warning' },
  NESTING: { L: '套料', K: 'NESTING', V: 1 << 5, T: 'success' }
}
constantize(structureOrderTypeEnum)

export {
  teamTypeEnum,
  teamAttributeEnum,
  componentTypeEnum,
  processMaterialListTypeEnum,
  processCategoryEnum,
  processInspectTypeEnum,
  processReportTypeEnum,
  wageQuotaTypeEnum,
  mesEnclosureTypeEnum,
  floorPlateTypeEnum,
  taskIssueTypeEnum,
  machinePartSchedulingIssueStatusEnum,
  machinePartNestingStatusEnum,
  packTypeEnum,
  packStatusTypeEnum,
  receiptStatusEnum,
  logisticsPriceTypeEnum,
  shipAuditStatusEnum,
  abnormalHandleTypeEnum,
  abnormalHandleStatusEnum,
  abnormalReportTypeEnum,
  abnormalChangeTypeEnum,
  abnormalStatusEnum,
  improveStatusEnum,
  projectComponentTypeEnum,
  artifactProcessEnum,
  paintingTypeEnum,
  reportComponentTypeEnum,
  labelTypeEnum,
  printProductTypeEnum,
  surplusHandleStatusEnum,
  inProductionDetailReportEnum,
  schedulingStatusEnum,
  artifactProductLineEnum,
  intellectParentType,
  minEqualTypeEnum,
  maxEqualTypeEnum,
  cuttingConfigEnum,
  deliveryStatusEnum,
  deliveryReceiptStatusEnum,
  searchDateTypeEnum,
  freightChangeTypeEnum,
  mesWarehouseStateTypeEnum,
  artifactTypeEnum,
  codingTypeEnum,
  partKeyWordEnum,
  hasHoleEnum,
  artifactSpecReviseEnum,
  taskTrackingSchedulingStatusEnum,
  projectNestingStatusEnum,
  MesBuildingTypesettingStatusEnum,
  nestingFileTypeEnum,
  nestingSettingTypeEnum,
  mesBuildingTypeSettingAssembleTypeEnum,
  mesSchedulingStatusEnum,
  mesMachinePartOrderTypeEnum,
  shipStatusEnum,
  workOrderTypeEnum,
  assembleTypeEnum,
  taskTypeENUM,
  fileNC1TypeEnum,
  sortingListEnum,
  productSearchTypeEnum,
  drillListEnum,
  structureOrderTypeEnum
}

export default {
  teamTypeEnum,
  teamAttributeEnum,
  componentTypeEnum,
  processMaterialListTypeEnum,
  processCategoryEnum,
  processInspectTypeEnum,
  processReportTypeEnum,
  wageQuotaTypeEnum,
  mesEnclosureTypeEnum,
  floorPlateTypeEnum,
  taskIssueTypeEnum,
  machinePartSchedulingIssueStatusEnum,
  machinePartNestingStatusEnum,
  packTypeEnum,
  packStatusTypeEnum,
  receiptStatusEnum,
  logisticsPriceTypeEnum,
  shipAuditStatusEnum,
  abnormalHandleTypeEnum,
  abnormalHandleStatusEnum,
  abnormalReportTypeEnum,
  abnormalChangeTypeEnum,
  abnormalStatusEnum,
  improveStatusEnum,
  projectComponentTypeEnum,
  artifactProcessEnum,
  paintingTypeEnum,
  reportComponentTypeEnum,
  labelTypeEnum,
  printProductTypeEnum,
  surplusHandleStatusEnum,
  inProductionDetailReportEnum,
  schedulingStatusEnum,
  artifactProductLineEnum,
  intellectParentType,
  minEqualTypeEnum,
  maxEqualTypeEnum,
  cuttingConfigEnum,
  deliveryStatusEnum,
  deliveryReceiptStatusEnum,
  searchDateTypeEnum,
  freightChangeTypeEnum,
  mesWarehouseStateTypeEnum,
  artifactTypeEnum,
  codingTypeEnum,
  partKeyWordEnum,
  hasHoleEnum,
  artifactSpecReviseEnum,
  taskTrackingSchedulingStatusEnum,
  projectNestingStatusEnum,
  MesBuildingTypesettingStatusEnum,
  nestingFileTypeEnum,
  nestingSettingTypeEnum,
  mesBuildingTypeSettingAssembleTypeEnum,
  mesSchedulingStatusEnum,
  mesMachinePartOrderTypeEnum,
  shipStatusEnum,
  workOrderTypeEnum,
  assembleTypeEnum,
  taskTypeENUM,
  fileNC1TypeEnum,
  sortingListEnum,
  productSearchTypeEnum,
  drillListEnum,
  structureOrderTypeEnum
}
