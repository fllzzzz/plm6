<template>
  <common-table
    v-bind="$attrs"
    :data="form.sectionSteelList"
    :cell-class-name="wrongCellMask"
    :expand-row-keys="expandRowKeys"
    row-key="uid"
  >
    <el-expand-table-column :data="form.sectionSteelList" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
      <template #default="{ row }">
        <el-input v-model="row.remark" :rows="1" type="textarea" placeholder="备注" maxlength="1000" show-word-limit />
      </template>
    </el-expand-table-column>
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left" />
    <el-table-column prop="classifyFullName" label="物料种类" align="center" width="120px" fixed="left" />
    <el-table-column prop="specification" label="规格" align="center" width="170px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" placement="top">
          <span>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="length" align="center" width="135px" :label="`定尺长度 (mm)`">
      <template #default="{ row }">
        <el-input-number v-model="row.length" :max="999999" :controls="false" :min="0" :precision="0" size="mini" placeholder="长" />
      </template>
    </el-table-column>
    <el-table-column prop="number" align="center" width="135px" :label="`数量 (${baseUnit.measure.unit})`">
      <template #default="{ row }">
        <el-input-number
          v-model="row.number"
          :min="1"
          :max="999999999"
          controls-position="right"
          :controls="false"
          :step="5"
          :precision="0"
          size="mini"
          placeholder="数量"
        />
      </template>
    </el-table-column>
    <el-table-column prop="totalLength" align="center" width="135px" :label="`总长度 (m)`" />
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
          :content="`单位重量：${row.unitWeight} kg/m， 理论重量：${row.theoryTotalWeight} kg， ${overDiffTip}`"
          :disabled="!row.hasOver"
          placement="top"
        >
          <el-input-number
            v-model="row.weighingTotalWeight"
            :min="0"
            :max="999999999"
            controls-position="right"
            :controls="false"
            :precision="baseUnit.weight.precision"
            size="mini"
            placeholder="重量"
            :class="{ 'over-weight-tip': row.hasOver }"
          />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="brand" label="品牌" align="center" min-width="100px">
      <template #default="{ row }">
        <el-input v-model.trim="row.brand" maxlength="60" size="mini" placeholder="品牌" />
      </template>
    </el-table-column>
    <el-table-column prop="heatNoAndBatchNo" label="炉批号/卷号" align="center" min-width="150px">
      <template #default="{ row }">
        <el-input v-model.trim="row.heatNoAndBatchNo" size="mini" placeholder="炉批号/卷号" />
      </template>
    </el-table-column>
    <el-table-column label="操作" width="70" align="center" fixed="right">
      <template #default="{ row, $index }">
        <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row.sn, $index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineEmits, defineExpose, ref, inject, watchEffect, reactive, watch } from 'vue'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { isBlank, isNotBlank } from '@/utils/data-type'

import { regExtra } from '@/composables/form/use-form'
import useTableValidate from '@compos/form/use-table-validate'
import useMatBaseUnit from '@/composables/store/use-mat-base-unit'
import useWeightOverDiff from '@/composables/wms/use-steel-weight-over-diff'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import { createUniqueString } from '@/utils/data-type/string'
import { calcSectionSteelTotalLength, calcSectionSteelWeight } from '@/utils/wms/measurement-calc'

const emit = defineEmits(['calc-weight'])

// 当前物料基础类型
const basicClass = matClsEnum.SECTION_STEEL.V

const matSpecRef = inject('matSpecRef') // 调用兄弟组件matSpecRef
const { baseUnit } = useMatBaseUnit(basicClass) // 当前分类基础单位
const { form } = regExtra() // 表单
const expandRowKeys = ref([]) // 展开行key

const { overDiffTip, weightOverDiff, diffSubmitValidate } = useWeightOverDiff(baseUnit) // 过磅重量超出理论重量处理

// 校验规则
const tableRules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  length: [{ required: true, message: '请填写定尺长度', trigger: 'blur' }],
  number: [{ required: true, message: '请填写数量', trigger: 'blur' }],
  weighingTotalWeight: [
    { required: true, message: '请填写重量', trigger: 'blur' },
    { validator: diffSubmitValidate, message: '超出误差允许范围,不可提交', trigger: 'blur' }
  ]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules, errorMsg: '请修正【型钢清单】中标红的信息' }) // 表格校验

// 行初始化
function rowInit(row) {
  const _row = reactive({
    uid: createUniqueString(),
    sn: row.sn, // 该科目规格唯一编号
    specificationLabels: row.specificationLabels, // 规格中文
    serialNumber: row.classify.serialNumber, // 科目编号
    classifyId: row.classify.id, // 科目id
    classifyFullName: row.classify.fullName, // 全路径名称
    basicClass: row.classify.basicClass, // 基础类型
    specification: row.spec, // 规格
    specificationMap: row.specKV, // 规格KV格式
    measureUnit: row.classify.measureUnit, // 计量单位
    accountingUnit: row.classify.accountingUnit, // 核算单位
    accountingPrecision: row.classify.accountingPrecision, // 核算单位小数精度
    measurePrecision: row.classify.measurePrecision, // 计量单位小数精度
    unitWeight: row.unitWeight, // 单位重量 kg/m
    number: undefined, // 数量
    length: undefined, // 定尺长度
    totalLength: undefined, // 总长度
    theoryWeight: undefined, // 理论单件重量
    theoryTotalWeight: undefined, // 理论总重量
    weighingTotalWeight: undefined, // 过磅重量
    hasOver: false // 是否超出理论重量
  })
  watchEffect(() => calcTheoryWeight(_row))
  watchEffect(() => calcTotalWeight(_row))
  watchEffect(() => calcTotalLength(_row))
  watchEffect(() => weightOverDiff(_row))
  watch(
    () => _row.weighingTotalWeight,
    () => {
      emit('calc-weight')
    }
  )
  return _row
}

// 总重计算与单位重量计算分开，避免修改数量时需要重新计算单件重量
// 计算单件重量
function calcTheoryWeight(row) {
  row.theoryWeight = calcSectionSteelWeight({
    length: row.length, // 长度
    unitWeight: row.unitWeight, // 单位重量
    lengthUnit: baseUnit.value.length.unit, // 长度单位
    weightUnit: baseUnit.value.weight.unit, // 重量单位
    precision: baseUnit.value.weight.precision // 重量小数精度
  })
}

// 计算总长
function calcTotalLength(row) {
  if (isNotBlank(row.length) && row.number) {
    row.totalLength = calcSectionSteelTotalLength({
      length: row.length, // 长度
      number: row.number // 数量
    })
  } else {
    row.totalLength = undefined
  }
}

// 计算总重
function calcTotalWeight(row) {
  if (isNotBlank(row.theoryWeight) && row.number) {
    row.theoryTotalWeight = row.theoryWeight * row.number
    row.weighingTotalWeight = row.theoryWeight * row.number
  } else {
    row.theoryTotalWeight = undefined
    row.weighingTotalWeight = undefined
  }
}

// 删除行
function delRow(sn, $index) {
  matSpecRef.value.delListItem(sn, $index)
  emit('calc-weight')
}

// 校验
function validate() {
  if (isBlank(form.sectionSteelList)) return true
  const { validResult, dealList } = tableValidate(form.sectionSteelList)
  form.sectionSteelList = dealList
  return validResult
}

defineExpose({
  rowInit,
  validate
})
</script>
