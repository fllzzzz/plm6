<template>
  <common-table
    v-bind="$attrs"
    :data="form.list"
    :cell-class-name="wrongCellMask"
    :show-empty-symbol="false"
    return-source-data
    row-key="uid"
  >
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column prop="serialNumber" show-overflow-tooltip label="编号" align="center" fixed="left">
      <template #default="{ row }">
        <table-cell-tag
          :show="row.requisitionMode === requisitionModeEnum.USE_INVENTORY.V"
          :name="requisitionModeEnum.USE_INVENTORY.L"
          color="#e6a23c"
        />
        <span>{{ row.serialNumber }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="classifyName" label="物料种类" align="center" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.classifyParentFullName" :disabled="!row.classifyParentFullName" :show-after="500" placement="top">
          <span v-empty-text="row.classifyName" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="specification" label="规格" align="center" fixed="left" show-overflow-tooltip>
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" placement="top">
          <span v-empty-text="row.specification" />
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="measureUnit" label="计量单位" align="center">
      <template #default="{ row }">
        <span v-empty-text>{{ row.measureUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="quantity" label="数量" align="center">
      <template #default="{ row }">
        <common-input-number
          v-if="row.measureUnit"
          v-model="row.quantity"
          :min="0"
          :max="row.requisitionMode === requisitionModeEnum.USE_INVENTORY.V ? row.canUseQuantity : 999999999"
          :controls="false"
          :step="1"
          :precision="row.measurePrecision"
          size="mini"
          placeholder="数量"
        />
        <span v-else v-empty-text />
      </template>
    </el-table-column>
    <el-table-column prop="accountingUnit" label="核算单位" align="center">
      <template #default="{ row }">
        <span v-empty-text>{{ row.accountingUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="mete" label="核算量" align="center">
      <template #default="{ row }">
        <common-input-number
          v-model="row.mete"
          :min="0.000001"
          :max="999999999"
          :controls="false"
          :step="1"
          :precision="row.accountingPrecision"
          size="mini"
          placeholder="核算量"
        />
      </template>
    </el-table-column>

    <el-table-column prop="color" label="颜色" align="center">
      <template #default="{ row }">
        <el-input v-model.trim="row.color" maxlength="20" size="mini" placeholder="颜色" />
      </template>
    </el-table-column>
    <el-table-column prop="brand" label="品牌" align="center">
      <template #default="{ row }">
        <el-input v-model.trim="row.brand" maxlength="60" size="mini" placeholder="品牌" />
      </template>
    </el-table-column>
    <el-table-column label="操作" width="140" align="center" fixed="right">
      <template #default="{ row, $index }">
        <common-button type="primary" size="mini" @click="search(row, $index)">查询</common-button>
        <common-button icon="el-icon-delete" type="danger" size="mini" @click="delRow(row.sn, $index)" />
      </template>
    </el-table-column>
  </common-table>
</template>

<script setup>
import { defineExpose, defineEmits, inject, reactive } from 'vue'
import { createUniqueString } from '@/utils/data-type/string'
import { requisitionModeEnum } from '@/utils/enum/modules/wms'
import { positiveNumPattern } from '@/utils/validate/pattern'

import { regExtra } from '@/composables/form/use-form'
import useTableValidate from '@compos/form/use-table-validate'

const emit = defineEmits(['search-inventory'])

const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const { form } = regExtra() // 表单

// 数量校验方式
const validateQuantity = (value, row) => {
  if (row.measureUnit) return !!value

  return true
}

const rules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  quantity: [{ validator: validateQuantity, message: '请填写数量', trigger: 'blur' }],
  mete: [
    { required: true, message: '请填写核算量', trigger: 'blur' },
    { pattern: positiveNumPattern, message: '核算量必须大于0', trigger: 'blur' }
  ]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: rules }) // 表格校验

// 行初始化
function rowInit(row) {
  const _row = reactive({
    uid: createUniqueString(),
    sn: row.sn, // 该科目规格唯一编号
    requisitionMode: requisitionModeEnum.PURCHASE.V,
    specificationLabels: row.specificationLabels, // 规格中文
    serialNumber: row.serialNumber, // 科目编号 - 规格
    classifyId: row.classify.id, // 科目id
    classifyFullPathId: row.classify.fullPathId, // 全路径id
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
    mete: undefined, // 核算量
    quantity: undefined // 数量
  })
  return _row
}

function search(row, index) {
  emit('search-inventory', row, index)
}

// 删除行
function delRow(sn, $index) {
  if (matSpecRef.value) {
    matSpecRef.value.delListItem(sn, $index)
  } else {
    form.list.splice($index, 1)
  }
}

// 校验
function validate() {
  const { validResult, dealList } = tableValidate(form.list)
  form.list = dealList
  return validResult
}

defineExpose({
  rowInit,
  validate
})
</script>
