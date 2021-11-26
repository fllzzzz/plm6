<template>
  <common-table
    v-bind="$attrs"
    :data="form.steelPlateList"
    :cell-class-name="wrongCellMask"
    :expand-row-keys="expandRowKeys"
    row-key="uid"
    style="width: 100%"
  >
    <el-expand-table-column :data="form.steelPlateList" v-model:expand-row-keys="expandRowKeys" row-key="uid">
      <template #default="{ row }">
        <el-input v-model="row.remark" :rows="3" type="textarea" placeholder="备注" maxlength="1000" show-word-limit />
      </template>
    </el-expand-table-column>
    <el-table-column label="序号" type="index" align="center" width="60" />
    <el-table-column prop="serialNumber" label="编号" align="center" min-width="110px" />
    <el-table-column prop="classifyFullName" label="物料种类" align="center" width="120px" />
    <el-table-column prop="specification" label="规格" align="center" width="100px">
      <template #default="{ row }">
        <el-tooltip :content="row.specCName" placement="top">
          <span>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="thickness" align="center" width="100px" :label="`厚 (mm)`">
      <template #default="{ row }">
        <el-input-number
          v-model="row.thickness"
          :max="999999"
          controls-position="right"
          :controls="false"
          :min="0"
          :precision="baseUnit.thicknessRetain"
          size="mini"
          style="width: 98%"
          placeholder="厚"
        />
      </template>
    </el-table-column>
    <el-table-column prop="width" align="center" width="135px" :label="`宽 mm`">
      <template #default="{ row }">
        <el-input-number
          v-model="row.width"
          :max="999999"
          controls-position="right"
          :controls="false"
          :min="0"
          :precision="0"
          size="mini"
          style="width: 98%"
          placeholder="宽"
        />
      </template>
    </el-table-column>
    <el-table-column prop="length" align="center" width="135px" :label="`长 (mm)`">
      <template #default="{ row }">
        <el-input-number
          v-model="row.length"
          :max="999999"
          :controls="false"
          :min="0"
          :precision="0"
          size="mini"
          style="width: 98%"
          placeholder="长"
        />
      </template>
    </el-table-column>
    <el-table-column prop="number" align="center" width="135px" :label="`数量 (${baseUnit.measure.unit})`">
      <template #default="{ row }">
        <el-input-number
          v-model="row.number"
          :max="999999999"
          controls-position="right"
          :controls="false"
          :min="1"
          :step="5"
          :precision="0"
          size="mini"
          style="width: 98%"
          placeholder="数量"
        />
      </template>
    </el-table-column>
    <el-table-column
      key="weighingTotalWeight"
      prop="weighingTotalWeight"
      align="center"
      :label="`总重 (${baseUnit.weight.unit})`"
      width="135px"
    >
      <template #default="{ row }">
        <el-tooltip
          class="item"
          effect="dark"
          :content="`理论重量：${row.theoryTotalWeight}， ${overDiffTip}`"
          :disabled="!row.hasOver"
          placement="top"
        >
          <el-input-number
            v-model="row.weighingTotalWeight"
            :max="999999999"
            controls-position="right"
            :controls="false"
            :min="0"
            :precision="baseUnit.weight.precision"
            size="mini"
            style="width: 98%"
            placeholder="重量"
            :class="{ 'over-weight-tip': row.hasOver }"
          />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="brand" label="品牌" align="center" min-width="150px">
      <template #default="{ row }">
        <el-input v-model.trim="row.brand" maxlength="60" size="mini" placeholder="品牌" style="width: 98%" />
      </template>
    </el-table-column>
    <el-table-column prop="furnaceLotNumber" label="炉批号/卷号" align="center" min-width="200px">
      <template #default="{ row }">
        <el-input v-model.trim="row.furnaceLotNumber" size="mini" placeholder="炉批号/卷号" style="width: 98%" />
      </template>
    </el-table-column>
    <el-table-column label="操作" width="70" align="center">
      <template #default="{ row, $index }">
        <common-button icon="el-icon-delete" type="danger" size="mini" @click="matSpecRef.delListItem(row.sn, $index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, computed, ref, inject, watchEffect, reactive } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { numOrPctEnum } from '@/utils/enum/modules/common'

import { regExtra } from '@/composables/form/use-form'
import useWmsConfig from '@/composables/store/use-wms-config'
import useTableValidate from '@compos/form/use-table-validate'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import { createUniqueString } from '@/utils/data-type/string'
import { calcSteelPlateWeight } from '@/utils/wms/measurement-calc'
import { isBlank, isNotBlank } from '@/utils/data-type'
import { convertUnits } from '@/utils/convert/unit'
import { STEEL_DIFF_UNIT } from '@/settings/config'

// 当前物料基础类型
const basicClass = matClsEnum.STEEL_PLATE.V

const tableRules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  width: [{ required: true, message: '请填写宽度', trigger: 'change' }],
  thickness: [{ required: true, message: '请填写厚度', trigger: 'change' }],
  weight: [{ required: true, message: '请填写重量', trigger: 'change' }],
  length: [{ required: true, message: '请填写长度', trigger: 'change' }],
  number: [{ required: true, message: '请填写数量', trigger: 'change' }]
}

const matSpecRef = inject('matSpecRef')
const { baseUnit } = useMatBaseUnit(basicClass)
const { inboundSteelCfg } = useWmsConfig()
const { form } = regExtra()
const expandRowKeys = ref([])

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules })

// 超出提示
const overDiffTip = computed(() => {
  if (inboundSteelCfg.value.steelDiffType === numOrPctEnum.PERCENTAGE.V) {
    return `实际重量与理论重量的误差不可超过理论重量的${inboundSteelCfg.value.steelDiff || 0}%`
  }
  return `实际重量与理论重量的误差不可超过${inboundSteelCfg.value.steelDiff || 0}g`
})

// 校验
function validate() {
  const { validResult, dealList } = tableValidate(form.list)
  form.list = dealList
  return validResult
}

// 行初始化
function rowInit(row) {
  // console.log('ins', ins, )
  // console.log(row)
  const _row = reactive({
    uid: createUniqueString(),
    sn: row.sn, // 该科目规格唯一编号
    specCName: row.specCName, // 规格中文
    serialNumber: row.classify.serialNumber, // 科目编号
    classifyId: row.classify.id, // 科目id
    classifyFullName: row.classify.fullName, // 全路径名称
    specification: row.spec, // 规格
    specificationMap: row.specKV, // 规格KV格式
    measureUnit: row.classify.measureUnit, // 计量单位
    accountingUnit: row.classify.accountingUnit, // 核算单位
    accountingPrecision: row.classify.accountingPrecision, // 核算单位小数精度
    measurePrecision: row.classify.measurePrecision, // 计量单位小数精度
    number: undefined, // 数量
    thickness: undefined, // 厚度
    length: undefined, // 长度
    width: undefined, // 宽度
    weight: undefined // 重量
  })
  watchEffect(() => calcUnitWeight(_row))
  watchEffect(() => calcTotalWeight(_row))
  watchEffect(() => weightOverDiff(_row))
  console.log('1')
  return _row
}

// 总重计算与单位重量计算分开，避免修改数量时需要重新计算单位重量
// 计算单位重量
function calcUnitWeight(row) {
  row.theoryUnitWeight = calcSteelPlateWeight({
    name: row.classifyFullName,
    length: row.length,
    width: row.width,
    thickness: row.thickness,
    lengthUnit: baseUnit.value.length.unit,
    weightUnit: baseUnit.value.weight.unit,
    decimals: baseUnit.value.weight.precision
  })
}

// 计算总重
function calcTotalWeight(row) {
  if (isNotBlank(row.theoryUnitWeight) && row.number) {
    row.theoryTotalWeight = row.theoryUnitWeight * row.number
    row.weighingTotalWeight = row.theoryUnitWeight * row.number
  } else {
    row.theoryTotalWeight = undefined
    row.weighingTotalWeight = undefined
  }
}

// 计算重量是否在正常范围内
function weightOverDiff(row) {
  if (isBlank(row.weighingTotalWeight) && isBlank(row.theoryTotalWeight)) return

  let hasOver = false
  const overNum = row.weighingTotalWeight - row.theoryTotalWeight
  const steelDiff = inboundSteelCfg.value.steelDiff
  const steelDiffType = inboundSteelCfg.value.steelDiffType
  if (steelDiffType === numOrPctEnum.PERCENTAGE.V) {
    hasOver = Math.abs(row.weighingTotalWeight / row.theoryTotalWeight - 1) * 100 > steelDiff
  }
  if (steelDiffType === numOrPctEnum.NUMBER.V) {
    hasOver =
      convertUnits(
        Math.abs(row.weighingTotalWeight - row.theoryTotalWeight),
        baseUnit.value.weight.unit,
        STEEL_DIFF_UNIT,
        baseUnit.value.weight.precision
      ) > steelDiff
  }
  row.hasOver = hasOver
  row.overNum = overNum
}

defineExpose({
  rowInit,
  validate
})
</script>
