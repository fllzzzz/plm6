<template>
  <common-table v-bind="$attrs" :data="form.steelPlateList" :cell-class-name="wrongCellMask" :expand-row-keys="expandRowKeys" row-key="uid">
    <el-expand-table-column :data="form.steelPlateList" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
      <template #default="{ row }">
        <div class="mtb-10">
          <el-input
            v-model="row.remark"
            :rows="1"
            :autosize="{ minRows: 1, maxRows: 1 }"
            type="textarea"
            placeholder="备注"
            maxlength="200"
            show-word-limit
            style="width: 400px"
          />
        </div>
      </template>
    </el-expand-table-column>
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left" />
    <el-table-column prop="classifyName" label="物料种类" align="center" fixed="left" width="120" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
          <span v-empty-text="row.classifyName" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="specification" label="规格" align="center" width="100px" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" placement="top">
          <span>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="thickness" align="center" width="100px" :label="`厚 (${baseUnit.thickness.unit})`">
      <template #default="{ row }">
        <common-input-number
          v-model="row.thickness"
          :min="0"
          :max="999999"
          controls-position="right"
          :controls="false"
          :precision="baseUnit.thickness.precision"
          size="mini"
          placeholder="厚"
        />
      </template>
    </el-table-column>
    <el-table-column prop="width" align="center" width="135px" :label="`宽 (${baseUnit.width.unit})`">
      <template #default="{ row }">
        <common-input-number
          v-model="row.width"
          :min="0"
          :max="999999"
          controls-position="right"
          :controls="false"
          :precision="baseUnit.width.precision"
          size="mini"
          placeholder="宽"
        />
      </template>
    </el-table-column>
    <el-table-column prop="length" align="center" width="135px" :label="`长 (${baseUnit.length.unit})`">
      <template #default="{ row }">
        <common-input-number
          v-model="row.length"
          :max="999999"
          :controls="false"
          :min="0"
          :precision="baseUnit.length.precision"
          size="mini"
          placeholder="长"
        />
      </template>
    </el-table-column>
    <el-table-column prop="quantity" align="center" width="135px" :label="`数量 (${baseUnit.measure.unit})`">
      <template #default="{ row }">
        <common-input-number
          v-model="row.quantity"
          :min="1"
          :max="999999999"
          controls-position="right"
          :controls="false"
          :step="1"
          :precision="baseUnit.measure.precision"
          size="mini"
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
          :content="`理论重量：${row.theoryTotalWeight} kg， ${overDiffTip}`"
          :disabled="!row.hasOver"
          placement="top"
        >
          <common-input-number
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
    <el-table-column prop="heatNoAndBatchNo" label="炉批号" align="center" min-width="150px">
      <template #default="{ row }">
        <el-input v-model.trim="row.heatNoAndBatchNo" size="mini" placeholder="炉批号" maxlength="200" />
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
import { calcSteelPlateWeight } from '@/utils/wms/measurement-calc'
import { positiveNumPattern } from '@/utils/validate/pattern'

const emit = defineEmits(['calc-weight'])

// 当前物料基础类型
const basicClass = matClsEnum.STEEL_PLATE.V

const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const { baseUnit } = useMatBaseUnit(basicClass) // 当前分类基础单位
const { form } = regExtra() // 表单
const expandRowKeys = ref([]) // 展开行key

const { overDiffTip, weightOverDiff, diffSubmitValidate } = useWeightOverDiff(baseUnit) // 过磅重量超出理论重量处理

const tableRules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  width: [
    { required: true, message: '请填写宽度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '宽度必须大于0', trigger: 'blur' }
  ],
  thickness: [
    { required: true, message: '请填写厚度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '厚度必须大于0', trigger: 'blur' }
  ],
  weighingTotalWeight: [
    { required: true, message: '请填写重量', trigger: 'blur' },
    { validator: diffSubmitValidate, message: '超出误差允许范围,不可提交', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '重量必须大于0', trigger: 'blur' }
  ],
  length: [
    { required: true, message: '请填写长度', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '长度必须大于0', trigger: 'blur' }
  ],
  quantity: [
    { required: true, message: '请填写数量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '数量必须大于0', trigger: 'blur' }
  ]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules, errorMsg: '请修正【钢板清单】中标红的信息' }) // 表格校验

// 行初始化
function rowInit(row) {
  const _row = reactive({
    uid: createUniqueString(),
    sn: row.sn, // 该科目规格唯一编号
    specificationLabels: row.specificationLabels, // 规格中文
    serialNumber: row.serialNumber, // 科目编号 - 规格
    classifyId: row.classify.id, // 科目id
    classifyFullName: row.classify.fullName, // 全路径名称
    classifyName: row.classify.name, // 当前科目名称
    classifyParentFullName: row.classify.parentFullName, // 父级路径名称
    basicClass: row.classify.basicClass, // 基础类型
    specification: row.spec, // 规格
    specificationMap: row.specKV, // 规格KV格式
    measureUnit: row.classify.measureUnit, // 计量单位
    accountingUnit: row.classify.accountingUnit, // 核算单位
    accountingPrecision: row.classify.accountingPrecision, // 核算单位小数精度
    measurePrecision: row.classify.measurePrecision, // 计量单位小数精度
    quantity: undefined, // 数量
    thickness: undefined, // 厚度
    length: undefined, // 长度
    width: undefined, // 宽度
    theoryWeight: undefined, // 理论单件重量
    theoryTotalWeight: undefined, // 理论总重量
    weighingTotalWeight: undefined // 过磅重量
  })
  rowWatch(_row)
  return _row
}

// 行监听
// 使用watch 监听方法，优点：初始化时表单数据时，可以不立即执行（惰性），可以避免“草稿/修改”状态下重量被自动修改；缺点：初始化时需要指定监听参数
function rowWatch(row) {
  // watchEffect(() => calcTheoryWeight(row))
  // watchEffect(() => calcTotalWeight(row))
  watchEffect(() => weightOverDiff(row))
  // 计算单件理论重量
  watch([() => row.length, () => row.width, () => row.thickness, baseUnit], () => calcTheoryWeight(row))
  // 计算总重
  watch([() => row.theoryWeight, () => row.quantity], () => calcTotalWeight(row))
  // 钢材总重计算
  watch(
    () => row.weighingTotalWeight,
    () => {
      emit('calc-weight')
    },
    { immediate: true }
  )
}

// 总重计算与单位重量计算分开，避免修改数量时需要重新计算单件重量
// 计算单件重量
async function calcTheoryWeight(row) {
  row.theoryWeight = await calcSteelPlateWeight(
    {
      name: row.classifyFullName, // 名称，用于判断是否为不锈钢，不锈钢与普通钢板密度不同
      length: row.length,
      width: row.width,
      thickness: row.thickness
    },
    false
  )
}

// 计算总重
function calcTotalWeight(row) {
  if (isNotBlank(row.theoryWeight) && row.quantity) {
    row.theoryTotalWeight = row.theoryWeight * row.quantity
    row.weighingTotalWeight = row.theoryWeight * row.quantity
  } else {
    row.theoryTotalWeight = undefined
    row.weighingTotalWeight = undefined
  }
}

// 删除行
function delRow(sn, $index) {
  if (matSpecRef.value) {
    matSpecRef.value.delListItem(sn, $index)
  } else {
    form.steelPlateList.splice($index, 1)
  }
  emit('calc-weight')
}

// 校验
function validate() {
  if (isBlank(form.steelPlateList)) return true
  const { validResult, dealList } = tableValidate(form.steelPlateList)
  form.steelPlateList = dealList
  return validResult
}

defineExpose({
  rowInit,
  rowWatch,
  validate
})
</script>
