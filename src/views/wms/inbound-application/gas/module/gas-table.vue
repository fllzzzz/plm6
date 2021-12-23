<template>
  <common-table v-bind="$attrs" :data="form.list" :cell-class-name="wrongCellMask" :expand-row-keys="expandRowKeys" row-key="uid">
    <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="uid" fixed="left">
      <template #default="{ row }">
        <el-input
          v-model="row.remark"
          :rows="1"
          :autosize="{ minRows: 1, maxRows: 1 }"
          type="textarea"
          placeholder="备注"
          maxlength="200"
          show-word-limit
          style="width:400px"
        />
      </template>
    </el-expand-table-column>
    <el-table-column label="序号" type="index" align="center" width="60" fixed="left" />
    <el-table-column prop="serialNumber" label="编号" align="center" width="110px" fixed="left" />
    <el-table-column prop="classifyFullName" label="物料种类" align="center" min-width="200px" fixed="left" />
    <el-table-column prop="specification" label="规格" align="center" min-width="200px" fixed="left">
      <template #default="{ row }">
        <el-tooltip :content="row.specificationLabels" placement="top">
          <span v-empty-text>{{ row.specification }}</span>
        </el-tooltip>
      </template>
    </el-table-column>
    <el-table-column prop="measureUnit" label="计量单位" align="center" min-width="70px">
      <template #default="{ row }">
        <span v-empty-text>{{ row.measureUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="quantity" label="数量" align="center" min-width="120px">
      <template #default="{ row }">
        <el-input-number
          v-if="row.measureUnit"
          v-model="row.quantity"
          :min="1"
          :max="999999999"
          :controls="false"
          :step="1"
          :precision="row.measurePrecision"
          size="mini"
          placeholder="数量"
        />
        <span v-else v-empty-text />
      </template>
    </el-table-column>
    <el-table-column prop="accountingUnit" label="核算单位" align="center" min-width="70px">
      <template #default="{ row }">
        <span v-empty-text>{{ row.accountingUnit }}</span>
      </template>
    </el-table-column>
    <el-table-column prop="mete" label="核算量" align="center" min-width="120px">
      <template #default="{ row }">
        <el-input-number
          v-model="row.mete"
          :min="1"
          :max="999999999"
          :controls="false"
          :step="1"
          :precision="row.accountingPrecision"
          size="mini"
          placeholder="核算量"
        />
      </template>
    </el-table-column>
    <el-table-column prop="brand" label="品牌" align="center" min-width="120px">
      <template #default="{ row }">
        <el-input v-model.trim="row.brand" maxlength="60" size="mini" placeholder="品牌" />
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
import { defineExpose, ref, inject, reactive } from 'vue'
import { isBlank } from '@/utils/data-type'

import { regExtra } from '@/composables/form/use-form'
import useTableValidate from '@compos/form/use-table-validate'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import { createUniqueString } from '@/utils/data-type/string'

const matSpecRef = inject('matSpecRef') // 调用父组件matSpecRef
const { form } = regExtra() // 表单
const expandRowKeys = ref([]) // 展开行key

// 数量校验方式
const validateQuantity = (value, row) => {
  if (row.measureUnit && isBlank(value)) {
    return false
  }
  return true
}

const tableRules = {
  classifyId: [{ required: true, message: '请选择物料种类', trigger: 'change' }],
  quantity: [{ validator: validateQuantity, message: '请填写数量', trigger: 'blur' }],
  mete: [{ required: true, message: '请填写核算量', trigger: 'blur' }]
}

const { tableValidate, wrongCellMask } = useTableValidate({ rules: tableRules }) // 表格校验

// 行初始化
function rowInit(row) {
  const _row = reactive({
    uid: createUniqueString(),
    sn: row.sn, // 该科目规格唯一编号
    specificationLabels: row.specificationLabels, // 规格中文
    serialNumber: row.serialNumber, // 科目编号 - 规格
    classifyId: row.classify.id, // 科目id
    classifyFullName: row.classify.fullName, // 全路径名称
    basicClass: row.classify.basicClass, // 基础类型
    specification: row.spec, // 规格
    specificationMap: row.specKV, // 规格KV格式
    measureUnit: row.classify.measureUnit, // 计量单位
    accountingUnit: row.classify.accountingUnit, // 核算单位
    accountingPrecision: row.classify.accountingPrecision, // 核算单位小数精度
    measurePrecision: row.classify.measurePrecision, // 计量单位小数精度
    quantity: undefined // 数量
  })
  return _row
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
