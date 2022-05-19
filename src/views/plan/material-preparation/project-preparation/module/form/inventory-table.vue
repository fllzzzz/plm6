<template>
  <common-table
    v-if="props.show"
    ref="tableRef"
    :data="filterList"
    :data-format="columnsDataFormat"
    :height="props.height"
    :default-expand-all="false"
    :cell-class-name="wrongCellMask"
    highlight-current-row
    row-key="id"
  >
    <material-base-info-columns spec-merge fixed="left" />
    <!-- 次要信息 -->
    <material-secondary-info-columns :basic-class="crud.form.materialBasicClass" fixed="left" />
    <!-- 仓库位置信息 -->
    <warehouse-info-columns fixed="left" />
    <!-- 单位及其数量 -->
    <!-- <material-unit-quantity-columns :basic-class="basicClass" /> -->
    <!-- <el-table-column
      key="material.outboundUnit"
      prop="material.outboundUnit"
      label="单位"
      align="center"
      width="70px"
      fixed="left"
      show-overflow-tooltip
    /> -->
    <!-- <el-table-column key="material.usedNumber" prop="material.usedNumber" label="数量" align="center" width="120px" show-overflow-tooltip>
      <template #default="{ row: { sourceRow: row } }">
        <el-tooltip
          effect="dark"
          :content="`当前物料最大可利用数：${row.material.operableNumber}`"
          :show-after="1000"
          placement="top-start"
        >
          <common-input-number
            v-model="row.material.usedNumber"
            :min="row.material.lowerLimitNumber"
            :max="row.material.operableNumber"
            controls-position="right"
            :controls="false"
            :step="1"
            :precision="row.material.outboundUnitPrecision"
            size="mini"
            placeholder="数量"
            @change="(nv, ov) => handleNumberChange(nv, ov, row)"
          />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column
      key="material.usedMete"
      prop="material.usedMete"
      align="center"
      :label="`理论总重（kg）`"
      width="135px"
    /> -->
    <el-table-column prop="material.measureUnit" label="计量单位" align="center" min-width="70px" />
    <el-table-column prop="material.usedQuantity" label="数量" align="center" min-width="120px">
      <template #default="{ row }">
        <template v-if="row.material.measureUnit">
          <el-tooltip
            v-if="row.sourceRow.material.outboundUnitType === measureTypeEnum.MEASURE.V"
            effect="dark"
            :content="`当前物料最大可利用数：${row.sourceRow.material.operableNumber}`"
            :show-after="1000"
            placement="top-start"
          >
            <common-input-number
              v-model="row.sourceRow.material.usedQuantity"
              :min="row.sourceRow.material.lowerLimitNumber"
              :max="row.sourceRow.material.operableNumber"
              :controls="false"
              :step="1"
              :precision="+row.sourceRow.material.measurePrecision"
              size="mini"
              placeholder="数量"
              @change="(nv, ov) => handleQuantityChange(nv, ov, row.sourceRow)"
            />
          </el-tooltip>
          <span v-else>{{ row.material.usedQuantity || 0 }}</span>
        </template>
        <span v-else>-</span>
      </template>
    </el-table-column>
    <el-table-column prop="material.accountingUnit" label="核算单位" align="center" min-width="70px" />
    <el-table-column prop="material.usedMete" label="核算量" align="center" min-width="120px">
      <template #default="{ row }">
        <el-tooltip
          v-if="row.sourceRow.material.outboundUnitType === measureTypeEnum.ACCOUNTING.V"
          effect="dark"
          :content="`当前物料最大可利用数：${row.sourceRow.material.operableNumber}`"
          :show-after="1000"
          placement="top-start"
        >
          <common-input-number
            v-model="row.sourceRow.material.usedMete"
            :min="row.sourceRow.material.lowerLimitNumber"
            :max="row.sourceRow.material.operableNumber"
            :controls="false"
            :step="1"
            :precision="+row.sourceRow.material.accountingPrecision"
            size="mini"
            placeholder="核算量"
            @change="(nv, ov) => handleMeteChange(nv, ov, row.sourceRow)"
          />
        </el-tooltip>
        <span v-else>{{ row.material.usedMete || 0 }}</span>
      </template>
    </el-table-column>
    <el-table-column key="remark" prop="remark" align="center" label="备注" min-width="150px">
      <template #default="{ row: { sourceRow: row } }">
        <el-input v-model="row.remark" placeholder="备注" maxlength="200" size="mini" />
      </template>
    </el-table-column>
    <el-table-column label="操作" width="70" align="center" fixed="right">
      <template #default="{ row: { sourceRow: row }, $index }">
        <common-button
          class="icon-button"
          icon="el-icon-delete"
          type="danger"
          size="mini"
          :disabled="row.material.lowerLimitNumber > 0"
          @click="delRow(row, $index)"
        />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps, defineExpose, reactive, ref, computed, nextTick } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { componentTypeEnum } from '@/utils/enum/modules/building-steel'
import { STEEL_BASE_UNIT, STEEL_ENUM } from '@/settings/config'
import { deepClone, isBlank, isNotBlank, toPrecision } from '@/utils/data-type'
import { createUniqueString } from '@/utils/data-type/string'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { calcTheoryWeight } from '@/utils/wms/measurement-calc'
import cloneDeep from 'lodash/cloneDeep'

import { regExtra } from '@compos/use-crud'
import MaterialBaseInfoColumns from '@/components-system/wms/table-custom-field-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-custom-field-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-custom-field-columns/warehouse-info-columns/index.vue'
import useTableValidate from '@/composables/form/use-table-validate'

const props = defineProps({
  show: {
    type: Boolean
  },
  height: {
    type: Number,
    default: 400
  },
  currentTechRow: {
    // 当前选中的清单
    type: Object
  },
  queryFilter: {
    // 查询过滤
    type: Object,
    default: () => ({})
  }
})

const inventoryList = ref([])

// 根据查询条件过滤列表
const filterList = computed(() => {
  let list = inventoryList.value
  if (props.queryFilter.showOnlySelectTechInfo && props.currentTechRow) {
    list = list.filter((row) => row.boundTechId === props.currentTechRow.id)
  }
  return list
})

// 表格列数据格式转换
const columnsDataFormat = [
  ['material.usedMete', ['to-fixed-field', 'material.accountingPrecision']],
  ['material.usedQuantity', ['to-fixed-field', 'material.measurePrecision']]
]

// 数量校验方式
const validateQuantity = (value, row) => {
  if (row.material.measureUnit) return !!value && value > 0
  return true
}

// 核算量校验
const validateMete = (value, row) => {
  if (!row.material.measureUnit) {
    return !!value && value > 0
  }
  return true
}

// 表格规则
const tableRules = {
  'material.usedMete': [{ validator: validateMete, message: '核算量必须大于0' }],
  'material.usedQuantity': [{ validator: validateQuantity, message: '有计量单位，数量必须大于0' }]
}

// 表格校验
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules, errorMsg: '请修正【需要采购清单】中标红的信息' }) // 表格校验

// 校验
function validate() {
  if (isBlank(inventoryList.value)) {
    crud.form.inventoryList = []
    return true
  }
  const { validResult, dealList } = tableValidate(inventoryList.value)
  inventoryList.value = dealList
  // 为表单赋值
  crud.form.inventoryList = dealList
  return validResult
}

// 获取crud实例，并将实例注册进crud
const { CRUD, crud } = regExtra()

// 初始化表单
CRUD.HOOK.beforeEditDetailLoaded = async (crud, form) => {
  // 设置库存利用清单
  inventoryList.value = await listFM(deepClone(form.inventoryList || []))
  // TODO: 目前仅适用于钢板型材 库存列表中存在的id map
  const inventoryExitIdMap = new Map()
  inventoryList.value.forEach((row) => {
    inventoryExitIdMap.set(row.material.id, true)
  })
  crud.props.inventoryExitIdMap = inventoryExitIdMap

  // 绑定清单汇总列表
  initialBindingTechnologyList(inventoryList.value, form.technologyList)
  // 计算清单备料量及差值
  initialCalcTechPrepMete(inventoryList.value, form.technologyList)
  // 计算库存利用总量
  calcInventoryTotalMete(inventoryList.value)
}

// 库存利用清单格式转换
async function listFM(list) {
  if (!list) return []
  // 格式装换
  await setSpecInfoToList(list, { prefix: 'material' })
  await numFmtByBasicClass(
    list,
    {
      prefix: 'material',
      toNum: true
    },
    {
      mete: ['mete', 'usedMete', 'frozenMete', 'projectUsedMete', 'projectOutboundUsedMete'],
      quantity: ['quantity', 'usedQuantity', 'frozenQuantity', 'projectUsedQuantity', 'projectOutboundUsedQuantity']
    }
  )
  // 计算理论重量
  if (crud.form.materialBasicClass & STEEL_ENUM) {
    await calcTheoryWeight(list, { prefix: 'material' })
  }

  // 数量处理
  list.forEach((row) => {
    const { material } = row
    if (material.outboundUnitType === measureTypeEnum.MEASURE.V) {
      // 实际在出库中使用的数量
      material.number = material.quantity // 仓库数量
      material.usedNumber = material.usedQuantity // 利用数量
      material.frozenNumber = material.frozenQuantity // 冻结数量
      // 项目可操作数量 = 项目利用数量 - 项目利用且已经出库的数量
      material.projectOperableNumber = material.projectUsedQuantity - material.projectOutboundUsedQuantity
    } else {
      // 核算量
      material.number = material.mete // 仓库量
      material.usedNumber = material.usedMete // 核算量
      material.frozenNumber = material.frozenMete // 冻结核算量
      material.projectOperableNumber = material.projectUsedMete - material.projectOutboundUsedMete // 项目可操作核算量
    }
    // 最大数量 = 库存数量 - 冻结数量 + （项目当前可利用的数量 >= 备料单利用数量 ? 备料单利用数量 : 项目当前可利用的数量）
    material.operableNumber =
      material.number -
      (material.frozenNumber || 0) +
      (material.projectOperableNumber >= material.usedNumber ? material.usedNumber : material.projectOperableNumber)
    // 最小数量 = 备料单利用的数量 - 项目当前可利用的数量 && 该数量不小于0
    material.lowerLimitNumber =
      material.usedNumber - material.projectOperableNumber >= 0 ? material.usedNumber - material.projectOperableNumber : 0
    row = reactive(row)

    // 计算理论重量
    if (material.basicClass & STEEL_ENUM) {
      calcSteelTotalWeight(material)
    }
    // rowWatch(row)
  })
  return list
}

// 处理添加
function addRow(row, technologyRow) {
  // 加入map
  crud.props.inventoryExitIdMap.set(row.material.id, true)
  const invRow = reactive(cloneDeep(row))
  invRow.boolAdd = true // 是否添加
  invRow.id = createUniqueString() // 设置记录id（假，用于绑定）
  // 技术清单与库存利用清单互相绑定
  invRow.boundTechId = technologyRow.id
  const tr = crud.props.technologyListKV[invRow.boundTechId]
  if (tr) {
    tr.boundInvIds.push(invRow.id)
  }
  // 监听
  // rowWatch(invRow)
  inventoryList.value.push(invRow)
}

// 删除行
function delRow(row, index) {
  crud.props.inventoryExitIdMap.set(row.material.id, false)
  inventoryList.value.splice(index, 1)
  // 解除技术清单与库存利用清单之间的绑定
  const tId = row.boundTechId
  const techRow = crud.props.technologyListKV[tId]
  if (techRow) {
    techRow.boundInvIds.remove(row.id)
  }
  // 重新计算备料量
  calcTechPrepMete(inventoryList.value, techRow)
  // 重新计算库存利用总量
  calcInventoryTotalMete(inventoryList.value)
}

// 处理数量变化
function handleQuantityChange(newVal, oldVal, row) {
  const { material } = row
  // 填写的字段
  const triggerCalc = () => {
    if (oldVal !== material.usedQuantity) {
      if (material.basicClass & STEEL_ENUM) {
        calcSteelTotalWeight(material)
      } else {
        if (isNotBlank(material.usedQuantity)) {
          material.usedMete = toPrecision((material.usedQuantity / material.quantity) * material.mete, row.accountingPrecision)
        } else {
          material.usedMete = undefined
        }
      }
      calcTechPrepMete(inventoryList.value, crud.props.technologyListKV[row.boundTechId])
      calcInventoryTotalMete(inventoryList.value)
    }
  }

  if (!newVal || newVal < material.lowerLimitNumber) {
    // 设置为空 或 小于最小数量，则设置为最小数量
    nextTick(() => {
      material.usedQuantity = material.lowerLimitNumber
      triggerCalc()
    })
  } else {
    triggerCalc()
  }
}

// 处理核算量变化
function handleMeteChange(newVal, oldVal, row) {
  const { material } = row
  // 填写的字段
  const triggerCalc = () => {
    if (oldVal !== material.usedMete) {
      if (material.measureUnit && isNotBlank(material.usedMete)) {
        material.usedQuantity = toPrecision((material.usedMete / material.mete) * material.quantity, row.measurePrecision)
      } else {
        material.usedQuantity = undefined
      }
      calcTechPrepMete(inventoryList.value, crud.props.technologyListKV[row.boundTechId])
      calcInventoryTotalMete(inventoryList.value)
    }
  }

  if (!newVal || newVal < material.lowerLimitNumber) {
    // 设置为空 或 小于最小数量，则设置为最小数量
    nextTick(() => {
      material.usedMete = material.lowerLimitNumber
      triggerCalc()
    })
  } else {
    triggerCalc()
  }
}

// 计算总库存利用量
function calcInventoryTotalMete(inventoryList) {
  if (isNotBlank(inventoryList)) {
    crud.props.inventoryTotalMete = inventoryList.reduce((sum, cur) => {
      return (sum += cur.material.usedMete || 0)
    }, 0)
  } else {
    crud.props.inventoryTotalMete = 0
  }
}

// 计算钢板总重
function calcSteelTotalWeight(material) {
  if (isNotBlank(material.theoryWeight) && material.usedQuantity) {
    material.usedMete = material.theoryWeight * material.usedQuantity
  } else {
    material.usedMete = undefined
  }
}

// 初始-绑定清单汇总列表
function initialBindingTechnologyList(inventoryList, technologyList) {
  // 类型是钢板或者型钢
  if (crud.form.technologyListType === componentTypeEnum.STRUCTURE.V) {
    const steelClassifyConfICKV = crud.props.steelClassifyConfICKV // 钢材配置
    inventoryList.forEach((invRow) => {
      const { material } = invRow
      for (const techRow of technologyList) {
        const boundClassifyIds = steelClassifyConfICKV[techRow.steelClassifyConfId] || []
        // ------------- 钢板 ------------------
        if (
          material.basicClass === matClsEnum.STEEL_PLATE.V && // 钢板
          boundClassifyIds.includes(material.classifyId) && // 属于绑定分类
          material.specNameKV['材质'] === techRow.material && // 材质相同
          `${material.theoryThickness}` === techRow.specification // 厚度相同
        ) {
          invRow.boundTechId = techRow.id
          techRow.boundInvIds.push(invRow.id)
          break
        }
        // ------------- 型材 ------------------
        if (
          material.basicClass === matClsEnum.SECTION_STEEL.V && // 型材
          boundClassifyIds.includes(material.classifyId) && // 属于绑定分类
          material.specNameKV['材质'] === techRow.material && // 材质相同
          material.specNameKV[material.nationalStandard] === techRow.specification // 规格相同
        ) {
          invRow.boundTechId = techRow.id
          techRow.boundInvIds.push(invRow.id)
          break
        }
      }
    })
  }

  // 辅材
  if (crud.form.technologyListType === componentTypeEnum.AUXILIARY_MATERIAL.V) {
    // 遍历库存利用清单
    inventoryList.forEach((invRow) => {
      const { material } = invRow
      for (const techRow of technologyList) {
        // 是否相同
        const boolSameMat = Boolean(
          material.serialNumber === techRow.serialNumber && // 相同编号
            material.brand === techRow.brand && // 相同品牌
            material.color === techRow.color // 相同颜色
        )
        // 相同则绑定
        if (boolSameMat) {
          invRow.boundTechId = techRow.id
          techRow.boundInvIds.push(invRow.id)
          break
        }
      }
    })
  }
}

/**
 * 初始-计算清单备料量
 * @param {array} inventoryList 库存利用清单
 * @param {array} technologyList 技术汇总清单
 * @param {number} materialBasicClass 备料基础分类（bit）
 */
function initialCalcTechPrepMete(inventoryList, technologyList) {
  technologyList.forEach((techRow) => {
    // 初始化赋值
    crud.props.techPrepMeteKV[techRow.id] = crud.props.techPrepMeteKV[techRow.id] || {}
    // 计算当前技术清单行的备料量
    calcTechPrepMete(inventoryList, techRow)
  })
}

/**
 * 计算清单备料量
 * @param {array} inventoryList 库存利用清单
 * @param {object} technologyRow 技术汇总清单 行数据
 * @param {number} materialBasicClass 备料基础分类（bit）
 */
function calcTechPrepMete(inventoryList, technologyRow) {
  const info = crud.props.techPrepMeteKV[technologyRow.id]
  const boundMaterial = [] // 绑定物料的数组
  // 遍历“技术清单行”绑定的库存利用清单id
  technologyRow.boundInvIds.forEach((id) => {
    // 遍历“库存利用清单”，将绑定物料加入“绑定物料的数组”
    for (const invRow of inventoryList) {
      if (invRow.id === id) {
        boundMaterial.push(invRow.material)
        break
      }
    }
  })
  let summary = 0 // 总量
  let precision = 2 // 小数精度
  // -------------- 钢板或型钢 ----------------
  if (crud.form.technologyListType === componentTypeEnum.STRUCTURE.V) {
    // 累加理论重量
    summary = boundMaterial.reduce((sum, cur) => {
      return (sum += cur.usedMete || 0)
    }, 0)
    // 钢材基础单位
    precision = STEEL_BASE_UNIT.weight.precision
  }

  // 辅材
  if (crud.form.technologyListType === componentTypeEnum.AUXILIARY_MATERIAL.V) {
    summary = boundMaterial.reduce((sum, cur) => {
      return (sum += cur.usedMete || 0)
    }, 0)
    // 核算单位
    precision = technologyRow.accountingPrecision
  }

  info.inventory = toPrecision(summary, precision) // 库存利用量
  info.preparation = toPrecision((info.inventory || 0) + (info.purchase || 0), precision) // 总备料量
  info.diff = toPrecision(info.preparation - technologyRow.listMete, precision) // 差值 = 总备料量 - 清单量
  info.isEnough = info.diff >= 0 // 是否超出
}

// 合计
// function getSummaries(param) {
//   return tableSummary(param, {
//     props: [
//       ['usedNumber', 0],
//       ['usedMete', STEEL_BASE_UNIT.weight.precision]
//     ]
//   })
// }
defineExpose({
  add: addRow,
  validate: validate
})
</script>
