<template>
  <common-table
    v-if="props.show"
    ref="tableRef"
    :data="inventoryList"
    :data-format="columnsDataFormat"
    :height="props.height"
    :default-expand-all="false"
    :cell-class-name="wrongCellMask"
    highlight-current-row
    row-key="recordId"
  >
    <material-base-info-columns spec-merge fixed="left" />
    <!-- 次要信息 -->
    <material-secondary-info-columns :basic-class="crud.form.materialBasicClass" fixed="left" />
    <!-- 仓库位置信息 -->
    <warehouse-info-columns fixed="left" />
    <!-- 单位及其数量 -->
    <!-- <material-unit-quantity-columns :basic-class="basicClass" /> -->
    <el-table-column
      key="outboundUnit"
      prop="outboundUnit"
      label="计量单位"
      align="center"
      width="70px"
      fixed="left"
      show-overflow-tooltip
    />
    <el-table-column key="usedNumber" prop="usedNumber" label="数量" align="center" width="120px" show-overflow-tooltip>
      <template #default="{ row: { sourceRow: row } }">
        <el-tooltip effect="dark" :content="`当前物料最大可利用数：${row.operableNumber}`" :show-after="1000" placement="top-start">
          <common-input-number
            v-model="row.usedNumber"
            :min="row.lowerLimitNumber"
            :max="row.operableNumber"
            controls-position="right"
            :controls="false"
            :step="1"
            :precision="row.outboundUnitPrecision"
            size="mini"
            placeholder="数量"
            @change="(nv, ov) => handleNumberChange(nv, ov, row)"
          />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column key="usedTheoryTotalWeight" prop="usedTheoryTotalWeight" align="center" :label="`理论总重（kg）`" width="135px" />
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
          :disabled="row.lowerLimitNumber > 0"
          @click="delRow(row, $index)"
        />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineProps, defineExpose, reactive, ref, computed, nextTick } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { deepClone, isBlank, isNotBlank, toPrecision } from '@/utils/data-type'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { calcTheoryWeight } from '@/utils/wms/measurement-calc'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { createUniqueString } from '@/utils/data-type/string'
import { STEEL_BASE_UNIT, STEEL_ENUM } from '@/settings/config'
import { positiveNumPattern } from '@/utils/validate/pattern'
import cloneDeep from 'lodash/cloneDeep'

import { regExtra } from '@compos/use-crud'
import MaterialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import MaterialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import useTableValidate from '@/composables/form/use-table-validate'

const props = defineProps({
  show: {
    type: Boolean
  },
  height: {
    type: Number,
    default: 400
  }
})

const inventoryList = ref([])

// 表格列数据格式转换
const columnsDataFormat = [['usedTheoryTotalWeight', ['to-fixed-field', 'accountingUnit']]]

// 表格规则
const tableRules = {
  usedNumber: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  ]
}

// 表格校验
const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules, errorMsg: '请修正【需要采购清单】中标红的信息' }) // 表格校验

// 校验
function validate() {
  if (isBlank(inventoryList.value)) return true
  const { validResult, dealList } = tableValidate(inventoryList.value)
  inventoryList.value = dealList
  // 为表单赋值
  crud.form.inventoryList = dealList
  return validResult
}

// 获取crud实例，并将实例注册进crud
const { CRUD, crud } = regExtra()
// 是钢板或者型钢
const isSPorSS = computed(
  () => crud.form.materialBasicClass & matClsEnum.STEEL_PLATE.V || crud.form.materialBasicClass & matClsEnum.SECTION_STEEL.V
)

// 初始化表单
CRUD.HOOK.beforeEditDetailLoaded = async (crud, form) => {
  // 设置库存利用清单
  inventoryList.value = await listFM(deepClone(form.inventoryList || []))
  // TODO: 目前仅适用于钢板型材 库存列表中存在的id map
  const inventoryExitIdMap = new Map()
  inventoryList.value.forEach((row) => {
    inventoryExitIdMap.set(row.id, true)
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
  await setSpecInfoToList(list)
  await numFmtByBasicClass(list, {
    toNum: true
  })
  // 计算理论重量
  await calcTheoryWeight(list)

  // 数量处理
  list.forEach((row) => {
    if (row.outboundUnitType === measureTypeEnum.MEASURE.V) {
      // 实际在出库中使用的数量
      row.number = row.quantity // 仓库数量
      row.usedNumber = row.usedQuantity // 利用数量
      row.frozenNumber = row.frozenQuantity // 冻结数量
      row.projectOperableNumber = row.projectUsedQuantity - row.projectOutboundUsedQuantity // 项目可操作数量
    } else {
      // 核算量
      row.number = row.mete // 仓库量
      row.usedNumber = row.usedMete // 核算量
      row.frozenNumber = row.frozenMete // 冻结核算量
      row.projectOperableNumber = row.projectUsedMete - row.projectOutboundUsedMete // 项目可操作核算量
    }
    // 最大数量
    row.operableNumber =
      row.number - (row.frozenNumber || 0) + (row.projectOperableNumber >= row.usedNumber ? row.usedNumber : row.projectOperableNumber)
    // 最小数量
    row.lowerLimitNumber = row.usedNumber - row.projectOperableNumber >= 0 ? row.usedNumber - row.projectOperableNumber : 0
    row = reactive(row)
    // 计算理论重量
    if (row.basicClass & STEEL_ENUM) {
      calcSteelTotalWeight(row)
    }
    // rowWatch(row)
  })
  return list
}

// 处理添加
function addRow(row, technologyRow) {
  // 加入map
  crud.props.inventoryExitIdMap.set(row.id, true)
  const invRow = reactive(cloneDeep(row))
  invRow.recordId = createUniqueString() // 设置记录id（假，用于绑定）
  // 技术清单与库存利用清单互相绑定
  invRow.boundTech = technologyRow
  technologyRow.boundInvIds.push(invRow.recordId)
  // 监听
  // rowWatch(invRow)
  inventoryList.value.push(invRow)
}

// 删除行
function delRow(row, index) {
  crud.props.inventoryExitIdMap.set(row.id, false)
  inventoryList.value.splice(index, 1)
  // 解除技术清单与库存利用清单之间的绑定
  const techRow = row.boundTech
  techRow.boundInvIds.remove(row.recordId)
  // 重新计算备料量
  calcTechPrepMete(inventoryList.value, techRow)
  // 重新计算库存利用总量
  calcInventoryTotalMete(inventoryList.value)
}

// 处理数量变化
function handleNumberChange(newVal, oldVal, row) {
  const triggerCalc = () => {
    if (oldVal !== row.number) {
      if (row.basicClass & STEEL_ENUM) {
        calcSteelTotalWeight(row)
      }
      calcTechPrepMete(inventoryList.value, row.boundTech)
      calcInventoryTotalMete(inventoryList.value)
    }
  }

  if (!newVal || newVal < row.lowerLimitNumber) {
    // 设置为空 或 小于最小数量，则设置为最小数量
    nextTick(() => {
      row.number = row.lowerLimitNumber
      triggerCalc()
    })
  } else {
    triggerCalc()
  }
}

// 计算总库存利用量
function calcInventoryTotalMete(inventoryList) {
  if (isNotBlank(inventoryList)) {
    // 钢板或型材
    if (isSPorSS.value) {
      crud.props.inventoryTotalMete = inventoryList.reduce((sum, cur) => {
        return (sum += cur.usedTheoryTotalWeight || 0)
      }, 0)
    }
  } else {
    crud.props.inventoryTotalMete = 0
  }
}

// 计算钢板总重
function calcSteelTotalWeight(row) {
  if (isNotBlank(row.theoryWeight) && row.usedNumber) {
    row.usedTheoryTotalWeight = row.theoryWeight * row.usedNumber
  } else {
    row.usedTheoryTotalWeight = undefined
  }
}

// 初始-绑定清单汇总列表
function initialBindingTechnologyList(inventoryList, technologyList) {
  // 类型是钢板或者型钢
  if (isSPorSS.value) {
    const steelClassifyConfICKV = crud.props.steelClassifyConfICKV // 钢材配置
    inventoryList.forEach((invRow) => {
      for (const techRow of technologyList) {
        const boundClassifyIds = steelClassifyConfICKV[techRow.steelClassifyConfId] || []
        // ------------- 钢板 ------------------
        if (
          invRow.basicClass === matClsEnum.STEEL_PLATE.V && // 钢板
          boundClassifyIds.includes(invRow.classifyId) && // 属于绑定分类
          invRow.specNameKV['材质'] === techRow.material && // 材质相同
          `${invRow.theoryThickness}` === techRow.specification // 厚度相同
        ) {
          invRow.boundTech = techRow
          techRow.boundInvIds.push(invRow.recordId)
          break
        }
        // ------------- 型材 ------------------
        if (
          invRow.basicClass === matClsEnum.SECTION_STEEL.V && // 型材
          boundClassifyIds.includes(invRow.classifyId) && // 属于绑定分类
          invRow.specNameKV['材质'] === techRow.material && // 材质相同
          invRow.specNameKV[invRow.nationalStandard] === techRow.specification // 规格相同
        ) {
          invRow.boundTech = techRow
          techRow.boundInvIds.push(invRow.recordId)
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
    // 遍历“库存利用清单”，将绑定物料推入“绑定物料的数组”
    for (const invRow of inventoryList) {
      if (invRow.recordId === id) {
        boundMaterial.push(invRow)
        break
      }
    }
  })
  // -------------- 钢板或型钢 ----------------
  if (isSPorSS.value) {
    // 累加理论重量
    const summary = boundMaterial.reduce((sum, cur) => {
      return (sum += cur.usedTheoryTotalWeight || 0)
    }, 0)

    info.inventory = toPrecision(summary, STEEL_BASE_UNIT.weight.precision) // 库存利用量
    info.preparation = toPrecision((info.inventory || 0) + (info.purchase || 0), STEEL_BASE_UNIT.weight.precision) // 总备料量
    info.diff = toPrecision(info.preparation - technologyRow.listMete, STEEL_BASE_UNIT.weight.precision) // 差值 = 总备料量 - 清单量
    info.isEnough = info.diff >= 0 // 是否超出
  }
}

// 合计
// function getSummaries(param) {
//   return tableSummary(param, {
//     props: [
//       ['usedNumber', 0],
//       ['usedTheoryTotalWeight', STEEL_BASE_UNIT.weight.precision]
//     ]
//   })
// }
defineExpose({
  add: addRow,
  validate: validate
})
</script>
